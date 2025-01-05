% Base case- No more moves to evaluate, return the best move found
best_move(_, [], AllMoves, BestValueSoFar, BestMoveSoFar, BestMove) :-
    write('Current Best Value: '), write(BestValueSoFar), nl,
    (   BestValueSoFar =< 0
    ->  random_member(BestMove, AllMoves)  % In case of a tie, choose a random move
    ;   BestMove = BestMoveSoFar
    ),
    nl, nl,
    write('Selected Move: '), write(BestMove), nl.

% best_move(+GameState, +[Move | Rest], +AllMoves, +BestValueSoFar, +BestMoveSoFar, -BestMove) evaluates a list of possible moves and determines the best move based on their evaluated values.
best_move(GameState, [Move | Rest],AllMoves, BestValueSoFar, BestMoveSoFar, BestMove) :-
    evaluate_move(GameState, Move, Value),  % Avalia a jogada
    (   Value > BestValueSoFar
    ->  NewBestValue = Value,
        NewBestMove = Move,
        write('New Best Move Found: '), write(Move), nl
    ;   NewBestValue = BestValueSoFar,
        NewBestMove = BestMoveSoFar
    ),
    best_move(GameState, Rest, AllMoves, NewBestValue, NewBestMove, BestMove).

% evaluate_move(+GameState, +Move, -Value): Evaluates a specific move
evaluate_move(GameState, Move, Value) :-
    % Extract the current player from the game state
    GameState = game_state(_, Player, _, _),
    % Check if the move will result in four in a row for the current player
    (   will_have_four_in_a_row(GameState, Player, Move)
    ->  Value is 1000  % Assign a high value if it results in four in a row
    ;   % Otherwise, check if the move will result in four in a row for the opponent
        switch_player(Player, Opponent),
        will_have_four_in_a_row(GameState, Opponent, Move)
    ->  Value is 900  % Assign a slightly lower value if it results in four in a row for the opponent
    ;   % Otherwise, perform the move and evaluate the resulting game state
        move(GameState, Move, NewGameState)
    ->  % Extract the new board from the new game state
        NewGameState = game_state(NewBoard, _, _, _),
        % Count the sets of three for the current player on the new board
        count_sets_of_three(NewBoard, Player, SetsOfThree),
        % Assign a value based on the number of sets of three
        Value is SetsOfThree * 100
    ;   %If none of the above verifies the move value is 0
        Value is 0
    ),
    % Print the move and its value
    format('Move: ~w, Value: ~w~n', [Move, Value]).

% will_have_four_in_a_row(+GameState, +Player, +Move): Checks if a move results in four in a row for a given player.
will_have_four_in_a_row(GameState, Player, Move) :-
    (Move = place(CurrentPlayer, Size, X, Y) -> TestMove = place(Player, Size, X, Y) ; Move = transfer(CurrentPlayer, Size, X1, Y1, X2, Y2), TestMove = transfer(Player, Size, X1, Y1, X2, Y2)),
    GameState = game_state(Board, CurrentPlayer, RemainingPipes, SetsOfThree),
    NewState = game_state(Board, Player, RemainingPipes, SetsOfThree),
    (   move(NewState, TestMove, TestState)
    ->  TestState = game_state(NewBoard, _, _, _),
            check_four_in_a_row(NewBoard, Player)
        ;   false
    ).
    

