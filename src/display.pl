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

% display_menu/0: Prints the main game menu options.
display_menu :-
    write('Welcome to Aqua Pipe!'), nl,
    write('1. Human vs Human (H/H)'), nl,
    write('2. Human vs Computer (H/PC)'), nl,
    write('3. Computer vs Human (PC/H)'), nl,
    write('4. Computer vs Computer (PC/PC)'), nl,
    write('Choose an option (1-4):'), nl.

% display_game(+GameState): Displays the current game state, including the board, remaining pipes, sets of three, and the current player's turn.
display_game(game_state(Board, CurrentPlayer, RemainingPipes, SetsOfThree)) :-
    nl,
    % Extract sets of three for each player
    member(player1-Sets1, SetsOfThree),
    member(player2-Sets2, SetsOfThree),
    % Display sets of three for each player
    format('Player 1 sets of three: ~w~n', [Sets1]),
    format('Player 2 sets of three: ~w~n', [Sets2]),
    nl, write('Current Board:'), nl,
    % Get the size of the board and print it
    length(Board, Size),
    print_board(Board, Size),
    nl,nl,
    % Extract remaining pipes for each player
    member(player1:Pipes1, RemainingPipes),
    member(player2:Pipes2, RemainingPipes),
    % Display remaining pipes for each player
    format('Player 1 remaining pipes: ~w~n', [Pipes1]),
    format('Player 2 remaining pipes: ~w~n', [Pipes2]),nl,nl,
    % Display the current player's turn
    format(' ~w\'s turn.~n', [CurrentPlayer]),nl,nl.

% print_board(+Board, +Size): Prints the board with coordinates.
print_board(Board, Size) :-
    reverse(Board, ReversedBoard),  % Flips the board vertically so that row 1 is at the bottom
    print_rows(ReversedBoard, Size, Size),
    print_coordinates(Size).


% print_rows(+Board, +Size, +CurrentRow): Recursively prints rows with coordinates.
print_rows([], _, _).
print_rows([Row|Rest], Size, CurrentRow) :-
    format('~w | ', [CurrentRow]),
    print_row(Row),
    nl,
    NextRow is CurrentRow - 1,
    (NextRow > 0 -> print_rows(Rest, Size, NextRow) ; true).

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


% print_pipes(+Cell): Displays pipes of all sizes in a cell, order by size.
print_pipes(Cell) :-
    member((Player, small), Cell),
    pipe_symbol(small, SmallSymbol),
    player_color(Player, SmallColor),
    format('~w~w\e[0m', [SmallColor, SmallSymbol]), % Reset color after symbol
    fail;
    member((Player, medium), Cell),
    pipe_symbol(medium, MediumSymbol),
    player_color(Player, MediumColor),
    format('~w~w\e[0m', [MediumColor, MediumSymbol]), % Reset color after symbol
    fail;
    member((Player, large), Cell),
    pipe_symbol(large, LargeSymbol),
    player_color(Player, LargeColor),
    format('~w~w\e[0m', [LargeColor, LargeSymbol]), % Reset color after symbol
    fail;
    true.

% pipe_symbol(+Size, -Symbol): Maps a pipe size to its symbol.
pipe_symbol(small, 'o').
pipe_symbol(medium, 'O').
pipe_symbol(large, '()').

% Determines the color for a player
player_color(player1, Color) :- Color = '\e[31m'. % Red
player_color(player2, Color) :- Color = '\e[34m'. % Blue

% print_coordinates(+Board, +Size): Prints the bottom axis coordinates of the board.
print_coordinates( Size) :-
   write('   '), print_horizontal_line(Size),nl,
    write('   '),
    print_column_numbers(1, Size),nl.

% print_column_numbers(+Current, +Size): Recursively prints column numbers.
print_column_numbers(Current, Size) :-
    (   Current =< Size
    ->  format('[~w] ', [Current]),
        Next is Current + 1,
        print_column_numbers(Next, Size)
    ;   nl
    ).

print_horizontal_line(0) :- !.
print_horizontal_line(Size) :-
    write('---'),
    NewSize is Size - 1,
    print_horizontal_line(NewSize).


% announce_winner(+Winner): Displays the result of the game.
announce_winner(Winner) :-
    (   Winner = draw
    ->  write('The game is a draw!'),nl
    ;   format('Player ~w wins!~n', [Winner])
    ).