% Encontra o melhor movimento com base nos valores avaliados
best_move(_, [],AllMoves, BestValueSoFar, BestMoveSoFar, BestMove) :-
    write('Current Best Value: '), write(BestValueSoFar), nl,
    (   BestValueSoFar =< 0
    ->  random_member(BestMove, AllMoves)  % Em caso de empate, joga aleatoriamente
    ;   BestMove = BestMoveSoFar
    ),
    write('Selected Best Move: '), write(BestMove), nl.

% Caso recursivo: Avalia cada jogada e escolhe a melhor
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


% Avalia um movimento específico
evaluate_move(GameState, Move, Value) :-
    GameState = game_state(_, Player, _, _),
    (   will_have_four_in_a_row(GameState, Player, Move)
    ->  Value is 1000
    ;   switch_player(Player, Opponent),
        will_have_four_in_a_row(GameState, Opponent, Move)
    ->  Value is 900
    ;   move(GameState, Move, NewGameState)
    ->  NewGameState = game_state(NewBoard, _, _, _),
        count_sets_of_three(NewBoard, Player, SetsOfThree),
        Value is SetsOfThree * 100
    ;   Value is 0
    ),
    format('Move: ~w, Value: ~w~n', [Move, Value]).


% Verifica se haverá quatro em uma linha após um movimento
will_have_four_in_a_row(GameState, Player, Move) :-
    GameState = game_state(Board, CurrentPlayer, RemainingPipes, SetsOfThree),
    NewState = game_state(Board, Player, RemainingPipes, SetsOfThree),
    (   move(NewState, Move, TestState)
    ->  TestState = game_state(NewBoard, _, _, _),
            check_four_in_a_row(NewBoard, Player)
        ;   false
    ).
    

