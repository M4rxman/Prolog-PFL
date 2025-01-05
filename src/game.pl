:- module(game, [play/0]).
:- use_module(library(lists)).
:- use_module(library(clpfd)). 
:-use_module(library(between)).
:- use_module(library(random)).
:- include('display.pl').
:- include('ai.pl').



% play/0: Starts the game loop by displaying the menu and initializing the game state.
play :-
    display_menu,
    read_config(GameConfig),
    GameConfig = game_config(_, board_size(4),_),
    initial_state(GameConfig, GameState),
    game_loop(GameState, GameConfig).

% read_config(-GameConfig): Reads user input to determine the game configuration.
read_config(GameConfig) :-
    repeat,
    read(Option),
    (   get_game_type(Option, GameType)
    ->  ((Option == 2 ; Option == 3; Option== 4)  -> choose_level(Level) ; Level = 1),
        GameConfig = game_config(type(GameType), board_size(4), level(Level))
    ;   write('Invalid option. Please enter a number between 1 and 4.'), nl, fail
    ).


% get_game_type(+Option, -GameType): Maps user input to a specific game type.
get_game_type(1, h_h).
get_game_type(2, h_pc).
get_game_type(3, pc_h).
get_game_type(4, pc_pc).

% choose_level(-Level): Display level options and get a valid choice
choose_level(Level) :-
    repeat,
    write('Choose level (Random Moves - 1, Greedy Choice - 2): '), nl,
    read(Level),
    (   member(Level, [1, 2]) 
    ->  !   % Cut to stop repeat loop
    ;   write('Invalid choice. Please enter 1 or 2.'), nl, fail
    ).


% initial_state(+GameConfig, -GameState): Creates the initial game state based on the configuration.
initial_state(GameConfig, game_state(Board, player1, RemainingPipes, SetsOfThree)) :-
    GameConfig = game_config(_, board_size(BoardSize), _),
    create_empty_board(BoardSize, Board),
    NumberOfPipes is BoardSize + 2,
    RemainingPipes = [player1: [small-NumberOfPipes, medium-NumberOfPipes, large-NumberOfPipes], player2: [small-NumberOfPipes, medium-NumberOfPipes, large-NumberOfPipes]],
    SetsOfThree = [player1-0, player2-0].


% create_empty_board(+Size, -Board): Generates an empty S*S board.
create_empty_board(Size, Board) :-
    length(Board, Size),
    maplist(same_length(Board), Board),
    maplist(maplist(=([])), Board).
    
% Main game loop
% game_loop(+GameState): Executes the game loop, displaying the state and handling moves until the game ends.
game_loop(GameState, GameConfig) :-
    nl,nl,nl,nl,nl,nl,nl,
    GameState = game_state(_, Player, _, _),
    value(GameState, Player, Value),
    format('Current player value: ~w~n', [Value]),
    display_game(GameState),
    (   game_over(GameState, Winner)
    ->  announce_winner(Winner)
    ;   
        get_next_move(GameConfig, GameState, Move),
        move(GameState, Move, NewGameState),
        game_loop(NewGameState , GameConfig)
    ).


%Movements


% move(+GameState, +Move, -NewGameState): Validates and executes a move where user places a new pipe, updating the game state.
move(GameState, place(Player, Size, X, Y), NewGameState) :-
    GameState = game_state(Board, CurrentPlayer, RemainingPipes,_),
    CurrentPlayer = Player,
    % Validate the move
    within_board(X, Y, Board),
    valid_placement(Board, X, Y,Size),
    place_pipe(Board, X, Y, (Player, Size), NewBoard),
    update_pipes(RemainingPipes, Player, Size, UpdatedPipes),
    update_sets_of_three( NewBoard , UpdatedSetsOfThree),
    switch_player(Player, NextPlayer),
    NewGameState = game_state(NewBoard, NextPlayer, UpdatedPipes, UpdatedSetsOfThree).

% move(+GameState, +Move, -NewGameState): Validates and executes a move where the user transfers a pipe to a new position, updating the game state.
move(GameState,  transfer(Player, Size, FromX, FromY, ToX, ToY), NewGameState) :-
    GameState = game_state(Board, CurrentPlayer, RemainingPipes,_),
    CurrentPlayer = Player,
    % Validate the move
    within_board(FromX, FromY, Board),
    within_board(ToX, ToY, Board),
    valid_transfer(Board,CurrentPlayer, FromX, FromY, ToX, ToY, Size),
    transfer_pipe(Board, FromX, FromY, ToX, ToY, (Player, Size), NewBoard),
    update_sets_of_three( NewBoard, UpdatedSetsOfThree),
    switch_player(Player, NextPlayer),
    NewGameState = game_state(NewBoard, NextPlayer, RemainingPipes,UpdatedSetsOfThree).


% within_board(+X, +Y, +Board): Ensures the coordinates are valid for the board.
within_board(X, Y, Board) :-
    length(Board, Size),
    between(1, Size, X),
    between(1, Size, Y).

% valid_placement(+Board, +X, +Y): Ensures the target cell allows placement.
valid_placement(Board, X, Y, Size) :-
    nth1(Y, Board, Row),
    nth1(X, Row, Cell),
    \+ member((_, Size), Cell), %There can only be one pipe of each size per cell
    length(Cell, Count), Count < 3. % Each cell can only have three pipes

% valid_transfer(+Board, +Player, +FromX, +FromY, +ToX, +ToY, +Size): Check if a trasnfer is valid
valid_transfer(Board,Player, FromX, FromY, ToX, ToY, Size) :-
    nth1(FromY, Board, FromRow),
    nth1(FromX, FromRow, FromCell),
    nth1(ToY, Board, ToRow),
    nth1(ToX, ToRow, ToCell),
    member((Player, Size), FromCell), % Ensure the pipe of the given size is in the FromCell and belongs to the player
    \+member((_, Size), ToCell), % Ensure the pipe of the given size is not in the ToCell
    length(ToCell, Count), Count < 3, % Ensure the ToCell can accept another pipe
    player_has_played_all_sizes(Player, Board). % Ensure the player has played a pipe of each size

% player_has_played_all_sizes(+Player, +Board): Checks if the player has played a pipe of each size
player_has_played_all_sizes(Player, Board) :-
    \+ (member(Size, [small, medium, large]),
        \+ (member(Row, Board),
            member(Cell, Row),
             Cell \= [],
           member((Player, Size), Cell))).


% update_board(+Board, +X, +Y, +Pipe, -NewBoard): Places a pipe on the board.
place_pipe(Board, X, Y, Pipe, NewBoard) :-
    nth1(Y, Board, Row, RestRows),          %find destine row
    nth1(X, Row, Cell, RestCells),          %find destine cell
    append(Cell, [Pipe], NewCell),          %append pipe to cell 
    nth1(X, NewRow, NewCell, RestCells),    %reconstruct the destine row 
    nth1(Y, NewBoard, NewRow, RestRows).    %create the final board

% transfer_pipe(+Board, +FromX, +FromY, +ToX, +ToY, +Pipe, -NewBoard): Move a pipe from one cell to another
transfer_pipe(Board, FromX, FromY, ToX, ToY, Pipe, NewBoard) :-
    nth1(FromY, Board, FromRow, RestRows1),         %find the row where the pipe is originaly 
    nth1(FromX, FromRow, FromCell, RestCells1),     %find the cell
    delete(FromCell, Pipe, NewFromCell),            %delete the pipe from the cell
    nth1(FromX, NewFromRow, NewFromCell, RestCells1),      %reconstruct the row
    nth1(FromY, TempBoard, NewFromRow, RestRows1),         %reconstruct the board
    nth1(ToY, TempBoard, ToRow, RestRows2),                %find destine row
    nth1(ToX, ToRow, ToCell, RestCells2),                  %find destine cell
    append(ToCell, [Pipe], NewToCell),                     %append pipe to cell
    nth1(ToX, NewToRow, NewToCell, RestCells2),            %reconstruct the destine row 
    nth1(ToY, NewBoard, NewToRow, RestRows2).              %create the final board
  

% update_pipes(+RemainingPipes, +Player, +Size, -UpdatedPipes): Reduces the count of a specific pipe size for the given player.
update_pipes(RemainingPipes, Player, Size, UpdatedPipes) :-
    select(Player: Pipes, RemainingPipes, OtherPlayersPipes),
    select(Size-Count, Pipes, RemainingPipesForPlayer),
    NewCount is Count - 1,
    UpdatedPipesForPlayer = [Size-NewCount | RemainingPipesForPlayer],
    UpdatedPipes = [Player: UpdatedPipesForPlayer | OtherPlayersPipes].


% switch_player(+CurrentPlayer, -NextPlayer): Alternates the turn between the two players.
switch_player(player1, player2).
switch_player(player2, player1).

% valid_moves(+GameState, -ListOfMoves): Generates a list of all valid moves for the current player.
valid_moves(game_state(Board, CurrentPlayer, RemainingPipes,_), ListOfMoves) :-
    findall(place(CurrentPlayer, Size, X, Y),
        (   member(CurrentPlayer:Pipes, RemainingPipes),
            member(Size-Count, Pipes),
            Count > 0, % Ensure the player has pipes of this size
            within_board(X, Y, Board),
            valid_placement(Board, X, Y, Size)
        ),
        PlaceMoves),
    findall(transfer(CurrentPlayer, Size, FromX, FromY, ToX, ToY),
        (   within_board(FromX, FromY, Board),
            within_board(ToX, ToY, Board),
            valid_transfer(Board,CurrentPlayer, FromX, FromY, ToX, ToY, Size)
        ),
        TransferMoves),
    append(PlaceMoves, TransferMoves, ListOfMoves).


%Get Next Movement


% get_next_move(+GameConfig, +GameState, -Move): Determines the next move for the current player.
get_next_move(game_config(type(GameType), _, level(Level)), GameState, Move) :-
    GameState = game_state(_, Player, _, _),
    (   (GameType = h_h)  % Human vs Human
    ;   (GameType = h_pc, Player = player1)  % Human vs PC, Human's turn
    ;   (GameType = pc_h, Player = player2)  % PC vs Human, Human's turn
    )
    ->  ask_human_for_move(GameState, Move)   % Human player's turn
    ;   choose_move(GameState, Level, Move).     % PC player's turn



% ask_pc_for_move(+GameState,*Level, -Move): Chooses a move for the computer player. !!!Must be corrected according to the requirements
choose_move(GameState, Level, Move) :-
    valid_moves(GameState,Moves),
    (   Level = 1
    ->  random_member(Move, Moves)
    ;   Level = 2
    ->  best_move(GameState, Moves,Moves, -1, _, Move)  % Nível 2: Melhor jogada
    ).
    
% ask_human_for_move(+GameState, -Move): Asks the human player to choose a move and repeats until a valid one is chosen.
ask_human_for_move(GameState, Move) :-
    valid_moves(GameState, Moves),
    repeat,
    write('Example move: place(player1,small,X,Y) / transfer(player1,small,X1,Y1,X2,Y2).'),nl,nl,
    write('Choose your move:'), nl,
    read(Move),
    (   member(Move, Moves)
    ->  !
    ;   write('Invalid move. Try again.'), nl, fail
    ).

value(game_state(Board, _, _, _),Player, Value) :-
    count_sets_of_three(Board, Player, SetsOfThree),
    (   check_four_in_a_row(Board, Player)
    ->  Value is 1000 + SetsOfThree * 100
    ;   player_has_played_all_sizes(Player, Board)
    ->  Value is SetsOfThree * 100 + 50
    ;   Value is SetsOfThree * 100
    ).

% game_over(+GameState, -Winner): Checks if the game has ended and determines the winner or if it's a draw.
game_over(game_state(Board, _, _, SetsOfThree), Winner) :-
    (   check_victory(Board, player1, SetsOfThree)
    ->  Winner = player1
    ;   check_victory(Board, player2, SetsOfThree)
    ->  Winner = player2
    ;   board_full(Board)
    ->  Winner = draw
    ;   fail % Game is not over yet
    ).

% check_victory(+Board, +Player): Checks if the given player has achieved a victory condition.
check_victory(Board, Player,SetsOfThree) :-
    check_four_in_a_row(Board, Player);
    member(Player-Count, SetsOfThree),
    Count >= 4.

% Checks if the board is full
% board_full(+Board): Succeeds if there are no empty cells on the board.
board_full(Board) :-
        \+ (member(Row, Board), member(Cell, Row), length(Cell, L), L < 3).

% check_four_in_a_row(+Board, +Player): Checks if the player has four pipes of the same size in a row.
check_four_in_a_row(Board, Player) :-
    member(Size, [small, medium, large]),
    (   check_rows(Board, Player, Size);
        check_columns(Board, Player, Size);
        check_diagonals(Board, Player, Size)
    ).
% check_rows(+Board, +Player, +Size): Check rows for four in a row
check_rows(Board, Player, Size) :-
    member(Row, Board),
    append(_, [Cell1, Cell2, Cell3, Cell4 | _], Row),
    member((Player, Size), Cell1),
    member((Player, Size), Cell2),
    member((Player, Size), Cell3),
    member((Player, Size), Cell4).

% check_columns(+Board, +Player, +Size): Check columns for four in a row
check_columns(Board, Player, Size) :-
    transpose(Board, TransposedBoard),
    check_rows(TransposedBoard, Player, Size).

%check_diagonals(+Board, +Player, +Size): Check diagonals for four in a row
check_diagonals(Board, Player, Size) :-
    % Check top-left to bottom-right diagonals
    append(_, [Row1, Row2, Row3, Row4 | _], Board),
    nth1(Index, Row1, Cell1),
    NextIndex1 is Index + 1,
    nth1(NextIndex1, Row2, Cell2),
    NextIndex2 is NextIndex1 + 1,
    nth1(NextIndex2, Row3, Cell3),
    NextIndex3 is NextIndex2 + 1,
    nth1(NextIndex3, Row4, Cell4),
    member((Player, Size), Cell1),
    member((Player, Size), Cell2),
    member((Player, Size), Cell3),
    member((Player, Size), Cell4).

check_diagonals(Board, Player, Size) :-
    % Check top-right to bottom-left diagonals
    append(_, [Row1, Row2, Row3, Row4 | _], Board),
    nth1(Index, Row1, Cell1),
    PrevIndex1 is Index - 1,
    nth1(PrevIndex1, Row2, Cell2),
    PrevIndex2 is PrevIndex1 - 1,
    nth1(PrevIndex2, Row3, Cell3),
    PrevIndex3 is PrevIndex2 - 1,
    nth1(PrevIndex3, Row4, Cell4),
    member((Player, Size), Cell1),
    member((Player, Size), Cell2),
    member((Player, Size), Cell3),
    member((Player, Size), Cell4).



% update_sets_of_three(+Board, -UpdatedSetsOfThree): Updates the count of sets of three for both players, after a move
update_sets_of_three(Board, UpdatedSetsOfThree) :-
    count_sets_of_three(Board, player1, Count1),
    count_sets_of_three(Board, player2, Count2),
    UpdatedSetsOfThree = [player1-Count1, player2-Count2].

% count_sets_of_three(+Board, +Player, -Count): Counts the sets of three pipes in a row of a given player
count_sets_of_three(Board, Player,Count) :-
    findall(Size, check_three_in_a_row(Board, Player, Size), Sets),
    length(Sets, Count).

%check_three_in_a_row(+Board, +Player, +Size): Check if there are three pipes of the same size and player in a row/column or diagonal
check_three_in_a_row(Board, Player, Size) :-
    member(Size, [small, medium, large]),
    (   check_three_in_row(Board, Player, Size);
        check_three_in_column(Board, Player, Size);
        check_three_in_diagonal(Board, Player, Size)
    ).

%check_three_in_row(+Board, +Player, +Size): Row Check (Sliding window approach)
check_three_in_row(Board, Player, Size) :-
    member(Row, Board),
    sliding_window(Row, Player, Size).

%check_three_in_column(+Board, +Player, +Size): Column Check (Transpose board and apply row check)
check_three_in_column(Board, Player, Size) :-
    transpose(Board, TransposedBoard),
    check_three_in_row(TransposedBoard, Player, Size).

% Diagonal Check (Main and Anti-Diagonal using sliding window)
check_three_in_diagonal(Board, Player, Size) :-
    check_main_diagonal(Board, Player, Size);
    check_anti_diagonal(Board, Player, Size).

% check_main_diagonal(+Board, +Player, +Size): Main Diagonal Check (Sliding window over diagonals)
check_main_diagonal(Board, Player, Size) :-
    diagonal(Board, Diagonal),
    sliding_window(Diagonal, Player, Size).

% check_anti_diagonal(+Board, +Player, +Size): Anti-Diagonal Check
check_anti_diagonal(Board, Player, Size) :-
    anti_diagonal(Board, AntiDiagonal),
    sliding_window(AntiDiagonal, Player, Size).

%sliding_window(+List, +Player, +Size) Sliding window logic for sets of three
sliding_window(List, Player, Size) :-
    append(_, [Cell1, Cell2, Cell3 | _], List),
    member((Player, Size), Cell1),
    member((Player, Size), Cell2),
    member((Player, Size), Cell3).

%diagonal(+Board, +Diagonal): Extract the main diagonal from the board
diagonal(Board, Diagonal) :-
    findall(Cell, (nth1(N, Board, Row), nth1(N, Row, Cell)), Diagonal).

%anti_diagonal(+Board, +AntiDiagonal): Extract the anti-diagonal from the board
anti_diagonal(Board, AntiDiagonal) :-
    length(Board, N),
    findall(Cell, (nth1(X, Board, Row), Y is N - X + 1, nth1(Y, Row, Cell)), AntiDiagonal).
