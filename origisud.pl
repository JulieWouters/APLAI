:-lib(ic).
:-lib(listut).

solve(Board,Vb) :-
    sudoku(Board,Vb), print_board(Board), writeln(" "), print_board(Vb).

sudoku(Board,Vb) :-
    constraints(Board,Vb), 
    %search(Board),
    search(Vb).

constraints(Board,Vboard) :-
    dim(Board,[9,9]), 
    dim(Vboard,[9,9]),
    Board :: [1..9], 
    Vboard :: [1..9], 
    ( for(I,1,9), param(Board) 
    do 
        Row is Board[I,1..9], 
        Col is Board[1..9,I], 
        alldifferent(Row),
        alldifferent(Col) ), 
        ( multifor([J,K],1,7,3), param(Board) 
        do 
            Block is Board[J..J+2,K..K+2], 
        flatten(Block, BlockList), 
        alldifferent(BlockList) ),
    channel(Board,Vboard),
    ( for(I,1,9), param(Vboard) 
    do 
        Row is Vboard[I,1..9], 
        Col is Vboard[1..9,I], 
        alldifferent(Row),
        alldifferent(Col) 
    ),
    diffblocks(Vboard).

search(Board) :-
    term_variables(Board, Vars), 
    labeling(Vars).

channel(Board,Vboard) :-
    (multifor([A,B,K],1,9), param(Board,Vboard)
    do
        V is Board[A,B],
        V2 is Vboard[K,B],
        #=(V, K, C),
        #=(V2, A, C)
    ).

diffblocks(Board) :-


print_board(Board) :-
    ( foreachelem(El,Board,[_,J]) 
    do 
        ( J =:= 1 -> nl ; true ), write(" "), ( var(El) -> write("_") ; write(El) ) 
    ),
    writeln(" ").