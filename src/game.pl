:- module(game, [play/0, test_false_three_in_row/0,test_true_three_in_row/0]).
:- use_module(library(lists)).
:- use_module(library(clpfd)). % For transpose and other list predicates.
:-use_module(library(between)).

% Define ANSI escape codes
ansi_format(Style, Text) :-
    ansi_code(Style, Code),
    format('\e[~dm~w\e[0m', [Code, Text]).

ansi_code(bold, 1).
ansi_code(underline, 4).
ansi_code(red, 31).
ansi_code(green, 32).
ansi_code(yellow, 33).
ansi_code(blue, 34).
ansi_code(magenta, 35).
ansi_code(cyan, 36).
ansi_code(white, 37).

% Main predicate to start the game
% play/0: Starts the game loop by displaying the menu and initializing the game state.
play :-
    display_menu,
    read_config(GameConfig),
    initial_state(GameConfig, GameState),
    game_loop(GameState).

% Displays the game menu
% display_menu/0: Prints the main game menu options.
display_menu :-
write('Welcome to Aqua Pipe!'), nl,
    write('1. Human vs Human (H/H)'), nl,
    write('2. Human vs Computer (H/PC)'), nl,
    write('3. Computer vs Human (PC/H)'), nl,
    write('4. Computer vs Computer (PC/PC)'), nl,
    write('Choose an option (1-4):'), nl.

% Reads the game configuration from the user
% read_config(-GameConfig): Reads user input to determine the game configuration.
read_config(GameConfig) :-
    read(Option),
    get_game_type(Option, GameType),
    GameConfig = game_config(type(GameType), board_size(4), difficulty(_)).

% Maps menu option to game type
% get_game_type(+Option, -GameType): Maps user input to a specific game type.
get_game_type(1, h_h).
get_game_type(2, h_pc).
get_game_type(3, pc_h).
get_game_type(4, pc_pc).

% Main game loop
% game_loop(+GameState): Executes the game loop, displaying the state and handling moves until the game ends.
game_loop(GameState) :-
    display_game(GameState),
    (   game_over(GameState, Winner)
    ->  announce_winner(Winner)
    ;   
        write('ok1'),
        valid_moves(GameState, Moves),
        write('ok2'),
        get_next_move(GameState, Moves, Move),
        write('ok3'),
        move(GameState, Move, NewGameState),
        write('ok4'),
        game_loop(NewGameState)
    ).

% Announces the winner or draw
% announce_winner(+Winner): Displays the result of the game.
announce_winner(Winner) :-
    (   Winner = draw
    ->  write('The game is a draw!'),nl
    ;   format('Player ~w wins!~n', [Winner])
    ).

% Determines the next move
% get_next_move(+GameState, +Moves, -Move): Determines the next move for the current player.
get_next_move(GameState, Moves, Move) :-
    GameState = game_state(_, Player, _),
    write('Im here'),
    (   (GameType = h_h, CurrentPlayer = player1)
    ;   (GameType = h_h, CurrentPlayer = player2)
    ;   (GameType = h_pc, CurrentPlayer = player1)
    ;   (GameType = pc_h, CurrentPlayer = player2)
    )
    ->  ask_human_for_move(Moves, Move)
    ;  write('Player not human').

% Ask the human for a move
% ask_human_for_move(+Moves, -Move): Prompts the human player to choose a move.
ask_human_for_move(Moves, Move) :-
    write('Choose your move:'),nl,
    read(Move),
    (   member(Move, Moves)
    ->  true
    ;   write('Invalid move. Try again.'),nl,
        ask_human_for_move(Moves, Move)
    ).

% Initialize the game state
% initial_state(+GameConfig, -GameState): Creates the initial game state based on the configuration.
initial_state(GameConfig, game_state(Board, player1, RemainingPipes)) :-
    GameConfig = game_config(_, board_size(BoardSize), _),
    create_empty_board(BoardSize, Board),
    NumberOfPipes is BoardSize + 2,
    RemainingPipes = [player1: [small-NumberOfPipes, medium-NumberOfPipes, large-NumberOfPipes], player2: [small-NumberOfPipes, medium-NumberOfPipes, large-NumberOfPipes]].

% Create an empty board
% create_empty_board(+Size, -Board): Generates an empty NxN board.
create_empty_board(Size, Board) :-
    length(Board, Size),
    maplist(same_length(Board), Board),
    maplist(maplist(=([])), Board).

% Displays the current game state
% display_game(+GameState): Prints the board and the current player to the terminal.
display_game(game_state(Board, CurrentPlayer, _)) :-
    nl, write('Current Board:'),nl,
    length(Board, Size),
    print_board(Board, Size),
    print_coordinates(Size),
    format('~nPlayer ~w\'s turn.~n', [CurrentPlayer]).

% Prints the board row by row
% print_board(+Board, +Size): Iterates through the board rows and prints them.
print_board(Board, Size) :-
    reverse(Board, ReversedBoard),  % Inverte a lista de linhas
    print_rows(ReversedBoard, Size, Size),
    print_coordinates(Size).

% Prints each row of the board
% print_rows(+Board, +Size, +CurrentRow): Recursively prints rows with coordinates.
print_rows([], _, _).
print_rows([Row|Rest], Size, CurrentRow) :-
    format('~w | ', [CurrentRow]),
    print_row(Row),
    nl,
    NextRow is CurrentRow - 1,
    (NextRow > 0 -> print_rows(Rest, Size, NextRow) ; true).

% Prints a single row
% print_row(+Row): Prints the contents of a single row cell by cell.
print_row([]).
print_row([Cell|Rest]) :-
    (   Cell = []
    ->  write('[ ] ')
    ;   write('['),
        print_pipes(Cell),
        write('] ')
    ),
    print_row(Rest).

% Prints pipes within a cell
% print_pipes(+Cell): Displays pipes of all sizes in a cell.
print_pipes(Cell) :-
    member((Player, Size), Cell),
    pipe_symbol(Size, Symbol),
    player_color(Player, Color),
    format('~w~w\e[0m', [Color, Symbol]), % Reset color after symbol
    fail; true.

% Determines the symbol for a pipe size
% pipe_symbol(+Size, -Symbol): Maps a pipe size to its symbol.
pipe_symbol(small, 'º').
pipe_symbol(medium, 'o').
pipe_symbol(large, 'O').

% Determines the color for a player


player_color(player1, Color) :- Color = '\e[31m'. % Red
player_color(player2, Color) :- Color = '\e[34m'. % Blue

% Prints the column coordinates
% print_coordinates(+Size): Prints the bottom axis coordinates of the board.
print_coordinates(Size) :-
    write('    +'), print_horizontal_line(Size), nl,
    print_rows(Board, 1, Size).

print_horizontal_line(0) :- !.
print_horizontal_line(Size) :-
    write('---'),
    NewSize is Size - 1,
    print_horizontal_line(NewSize).

print_rows([], _, _).
print_rows([Row | Rest], RowIndex, Size) :-
    format('~|~` t~d~2+ | ', [RowIndex]),
    print_cells(Row),
    nl,
    write('    +'), print_horizontal_line(Size), nl,
    NextRowIndex is RowIndex + 1,
    print_rows(Rest, NextRowIndex, Size).

print_cells([]).
print_cells([Cell | Rest]) :-
    write(Cell), write(' | '),
    print_cells(Rest).

% Executes a move
% move(+GameState, +Move, -NewGameState): Validates and executes a move, updating the game state.
move(GameState, place(Player, Size, X, Y), NewGameState) :-
    GameState = game_state(Board, CurrentPlayer, RemainingPipes),
    CurrentPlayer = Player,
    % Validate the move
    within_board(X, Y, Board),
    valid_placement(Board, X, Y,Size),
    place_pipe(Board, X, Y, (Player, Size), NewBoard),
    update_pipes(RemainingPipes, Player, Size, UpdatedPipes),
    switch_player(Player, NextPlayer),
    NewGameState = game_state(NewBoard, NextPlayer, UpdatedPipes).

move(GameState,  transfer(Player, Size, FromX, FromY, ToX, ToY), NewGameState) :-
    GameState = game_state(Board, CurrentPlayer, RemainingPipes),
    CurrentPlayer = Player,
    % Validate the move
    within_board(FromX, FromY, Board),
    within_board(ToX, ToY, Board),
    valid_transfer(Board,CurrentPlayer, FromX, FromY, ToX, ToY, Size),
    transfer_pipe(Board, FromX, FromY, ToX, ToY, (Player, Size), NewBoard),
    switch_player(Player, NextPlayer),
    NewGameState = game_state(NewBoard, NextPlayer, RemainingPipes).
% Checks if coordinates are within the board
% within_board(+X, +Y, +Board): Ensures the coordinates are valid for the board.
within_board(X, Y, Board) :-
    length(Board, Size),
    (   var(X)
    ->  between(1, Size, X)
    ;   X > 0, X =< Size
    ),
    (   var(Y)
    ->  between(1, Size, Y)
    ;   Y > 0, Y =< Size
    ).

% Checks if placement is valid
% valid_placement(+Board, +X, +Y): Ensures the target cell allows placement.
valid_placement(Board, X, Y, Size) :-
    nth1(Y, Board, Row),
    nth1(X, Row, Cell),
    \+ member((_, Size), Cell), % Não permite tubos do mesmo tamanho
    length(Cell, Count), Count < 3. % Célula pode ter no máximo 3 tubos

% Check if a trasnfer is valid
valid_transfer(Board,Player, FromX, FromY, ToX, ToY, Size) :-
    nth1(FromY, Board, FromRow),
    nth1(FromX, FromRow, FromCell),
    nth1(ToY, Board, ToRow),
    nth1(ToX, ToRow, ToCell),
    member((Player, Size), FromCell), % Ensure the pipe of the given size is in the FromCell and belongs to the player
    \+member((_, Size), ToCell), % Ensure the pipe of the given size is not in the ToCell
    length(ToCell, Count), Count < 3, % Ensure the ToCell can accept another pipe
    player_has_played_all_sizes(Player, Board). % Ensure the player has played a pipe of each size

% Checks if the player has played a pipe of each size
player_has_played_all_sizes(Player, Board) :-
    \+ (member(Size, [small, medium, large]),
        \+ (member(Row, Board),
            member(Cell, Row),
             Cell \= [],
           member((Player, Size), Cell))).

% Updates the board with a new pipe
% update_board(+Board, +X, +Y, +Pipe, -NewBoard): Places a pipe on the board.
place_pipe(Board, X, Y, Pipe, NewBoard) :-
    nth1(Y, Board, Row, RestRows),
    nth1(X, Row, Cell, RestCells),
    append(Cell, [Pipe], NewCell),
    nth1(X, NewRow, NewCell, RestCells),
    nth1(Y, NewBoard, NewRow, RestRows).

% Move a pipe from one cell to another
transfer_pipe(Board, FromX, FromY, ToX, ToY, Pipe, NewBoard) :-
    nth1(FromY, Board, FromRow, RestRows1),
    nth1(FromX, FromRow, FromCell, RestCells1),
    delete(FromCell, Pipe, NewFromCell),
    nth1(FromX, NewFromRow, NewFromCell, RestCells1),
    nth1(FromY, TempBoard, NewFromRow, RestRows1),
    nth1(ToY, TempBoard, ToRow, RestRows2),
    nth1(ToX, ToRow, ToCell, RestCells2),
    append(ToCell, [Pipe], NewToCell),
    nth1(ToX, NewToRow, NewToCell, RestCells2),
    nth1(ToY, NewBoard, NewToRow, RestRows2).
  
% Updates the remaining pipes after a move
% update_pipes(+RemainingPipes, +Player, +Size, -UpdatedPipes): Reduces the count of a specific pipe size for the given player.
update_pipes(RemainingPipes, Player, Size, UpdatedPipes) :-
    select(Player: Pipes, RemainingPipes, OtherPlayersPipes),
    select(Size-Count, Pipes, RemainingPipesForPlayer),
    NewCount is Count - 1,
    UpdatedPipesForPlayer = [Size-NewCount | RemainingPipesForPlayer],
    UpdatedPipes = [Player: UpdatedPipesForPlayer | OtherPlayersPipes].

% Switches the current player
% switch_player(+CurrentPlayer, -NextPlayer): Alternates the turn between the two players.
switch_player(player1, player2).
switch_player(player2, player1).

% valid_moves(+GameState, -ListOfMoves): Generates a list of all valid moves for the current player.
valid_moves(game_state(Board, CurrentPlayer, RemainingPipes), ListOfMoves) :-
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

% choose_move(+GameState, +Level, -Move): Chooses the best move for the AI based on the difficulty level.


% game_over(+GameState, -Winner): Checks if the game has ended and determines the winner or if it's a draw.
game_over(game_state(Board, _, _), Winner) :-
    (   check_victory(Board, player1)
    ->  Winner = player1
    ;   check_victory(Board, player2)
    ->  Winner = player2
    ;   board_full(Board)
    ->  Winner = draw
    ;   fail % Game is not over yet
    ).

% check_victory(+Board, +Player): Checks if the given player has achieved a victory condition.
check_victory(Board, Player) :-
    check_four_in_a_row(Board, Player);
    check_four_sets_of_three(Board, Player).

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
% Check rows for four in a row
check_rows(Board, Player, Size) :-
    member(Row, Board),
    append(_, [Cell1, Cell2, Cell3, Cell4 | _], Row),
    member((Player, Size), Cell1),
    member((Player, Size), Cell2),
    member((Player, Size), Cell3),
    member((Player, Size), Cell4).

% Check columns for four in a row
check_columns(Board, Player, Size) :-
    transpose(Board, TransposedBoard),
    check_rows(TransposedBoard, Player, Size).

% Check diagonals for four in a row
check_diagonals(Board, Player, Size) :-
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

% check_four_sets_of_three(+Board, +Player): Checks if the player has four sets of three in a row.
% Check for four sets of three pipes in-a-row of the same size
check_four_sets_of_three(Board, Player) :-
    findall((X, Y, Size), check_three_in_a_row(Board, Player, X, Y, Size), Sets),
    length(Sets, Count),
    Count >= 4.

% Check if three pipes of the same size form a row/column/diagonal
check_three_in_a_row(Board, Player, X, Y, Size) :-
    member(Size, [small, medium, large]),
    (   check_three_in_row(Board, Player, Size, X, Y);
        check_three_in_column(Board, Player, Size, X, Y);
        check_three_in_diagonal(Board, Player, Size, X, Y)
    ).

% Row Check (Fix: Ensure strict size matching in consecutive cells)
check_three_in_row(Board, Player, Size, X, Y) :-
    nth1(Y, Board, Row),
    append(_, [Cell1, Cell2, Cell3 | _], Row),
    member((Player, Size), Cell1),
    member((Player, Size), Cell2),
    member((Player, Size), Cell3),
    X = Y.  % Return the row index

% Column Check (Transpose board to check columns like rows)
check_three_in_column(Board, Player, Size, X, Y) :-
    transpose(Board, TransposedBoard),
    check_three_in_row(TransposedBoard, Player, Size, Y, X).

% Diagonal Check (Main and Anti-Diagonal)
check_three_in_diagonal(Board, Player, Size, X, Y) :-
    (   check_three_in_main_diagonal(Board, Player, Size, X, Y);
        check_three_in_anti_diagonal(Board, Player, Size, X, Y)
    ).

% Main Diagonal Check (Fixed to correctly track cells)
check_three_in_main_diagonal(Board, Player, Size, X, Y) :-
    length(Board, N),
    between(1, N, Start),
    check_main_diagonal(Board, Start, Player, Size, X, Y).

check_main_diagonal(Board, Start, Player, Size, X, Y) :-
    nth1(Start, Board, Row1),
    nth1(Start, Row1, Cell1),
    Next is Start + 1,
    nth1(Next, Board, Row2),
    nth1(Next, Row2, Cell2),
    Next2 is Next + 1,
    nth1(Next2, Board, Row3),
    nth1(Next2, Row3, Cell3),
    member((Player, Size), Cell1),
    member((Player, Size), Cell2),
    member((Player, Size), Cell3),
    X = Start,
    Y = Start.

% Anti-Diagonal Check
check_three_in_anti_diagonal(Board, Player, Size, X, Y) :-
    length(Board, N),
    between(1, N, Start),
    check_anti_diagonal(Board, Start, Player, Size, X, Y).

check_anti_diagonal(Board, Start, Player, Size, X, Y) :-
    nth1(Start, Board, Row1),
    nth1(_, Row1, Cell1),
    Next is Start + 1,
    nth1(Next, Board, Row2),
    nth1(_, Row2, Cell2),
    Next2 is Next + 1,
    nth1(Next2, Board, Row3),
    nth1(_, Row3, Cell3),
    member((Player, Size), Cell1),
    member((Player, Size), Cell2),
    member((Player, Size), Cell3),
    X = Start,
    Y = 1.