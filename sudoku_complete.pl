:-lib(ic).
:-lib(listut).

convert(Board,GoodBoard) :-
    (for(I,0,8), 
    fromto([],List,[A|List],GoodList),
    param(Board,GoodBoard,GoodList)
    do
        nth0(I,Board,Row),
        array_list(A,Row)
    ),
    array_list(GoodBoard,GoodList).

solve(BoardList,B) :- solve([],BoardList,B).
solve([],BoardList,B) :- solve([new,original],BoardList,B).
solve(Options,BoardList,B) :-
    convert(BoardList,Board),
    sudoku(Board,Vboard,Rboard,Cboard,B, Options), 
    print_board(Board), writeln(" "), print_board(Vboard).

sudoku(Board,Vboard,Rboard,Cboard,B,Options) :-
    constraints(Board,Vboard,Rboard,Cboard,Options),
    %search(Board),
    ic:search(Vboard,0,first_fail,indomain,complete,[backtrack(B)]).

constraints(Board,Vboard,Rboard,Cboard,Options) :-
  dim(Board,[9,9]), 
  dim(Vboard,[9,9]),
	dim(Rboard,[9,9]),
	dim(Cboard,[9,9]),
  Board :: [1..9], 
  Vboard :: [1..9],
	Rboard :: [1..9],
	Cboard :: [1..9],
  (member(original,Options) -> 
    channel(Board,Vboard),
    constraints_original(Board)
    ;
    true
  ),
  (member(new,Options) ->
    channel(Board,Vboard),
    constraints_new(Vboard)
    ;
    true
  ).
  %channel(Vboard,Rboard,Cboard),
	%constraints(Rboard,Cboard).
	
constraints_original(Board) :- %constraints for the original viewpoint
  (for(I,1,9), param(Board) do 
      Row is Board[I,1..9], 
      Col is Board[1..9,I], 
      alldifferent(Row),
      alldifferent(Col) 
    ), 
    (multifor([J,K],1,7,3), param(Board) do 
      Block is Board[J..J+2,K..K+2], 
      flatten(Block, BlockList), 
      alldifferent(BlockList) 
    ).

%constraints for our new viewpoint
constraints_new(Vboard) :-
  (for(I,1,9), param(Vboard) do 
    Row is Vboard[I,1..9], 
    Col is Vboard[1..9,I], 
    alldifferent(Row),
    alldifferent(Col) 
  ),
  constraints_square(Vboard). %different numbers in each square

constraints(Rboard,Cboard) :-
	constraints_rows(Rboard),
	constraints_cols(Cboard).
	
constraints_rows(Rboard) :-
	(for(I,1,9), param(Rboard) do
		(for(J,1,7,3), param(Rboard,I) do
			Block is Rboard[I,J..J+2],
			Block::[J..J+2]
		),
		Row is Rboard[I,1..9],
		alldifferent(Row)
	).
	
constraints_cols(Cboard) :-
	(for(I,1,9), param(Cboard) do
		Col is Cboard[I,1..9],
		alldifferent(Col),
		(for(J,1,7,3), param(Cboard,I) do
			Cboard[I,J]::[1..3],
			Cboard[I,J+1]::[4..6],
			Cboard[I,J+2]::[7..9]
		)
	).

constraints_square(Vboard) :-
  (for(I,1,9), param(Vboard)
  do
    (for(J,1,7,3), param(Vboard,I)
    do
      V1 is Vboard[I,J],
      V2 is Vboard[I,J+1],
      V3 is Vboard[I,J+2],
      #>=(V1,7,A),
      #>=(V2,7,B),
      #>=(V3,7,C),
      A + B + C #= 1,
      #=<(V1,3,D),
      #=<(V2,3,E),
      #=<(V3,3,F),
      D + E + F #= 1
    )
  ).

%channel between original viewpoint (Board) and our viewpoint (Vboard)
channel(Board,Vboard) :-
  (multifor([A,B,K],1,9), param(Board,Vboard) do
    V is Board[A,B],
    V2 is Vboard[K,A],
    #=(V, K, C),
    #=(V2, B, C)
  ).
	
channel(Vboard,Rboard,Cboard) :-
  (multifor([A,B,K],1,9), param(Vboard,Rboard,Cboard) do
    V is Vboard[A,B],
    Vr is Rboard[A,K],
		Vc is Cboard[A,K],
    #=(Vr, B, C),
    #=(Vc, V, C)
  ).


print_board(Board) :-
  (foreachelem(El,Board,[_,J]) do 
    ( J =:= 1 -> nl ; true ), write(" "), ( var(El) -> write("_") ; write(El) ) 
  ),
  writeln(" ").
	
%solve([]([](_, _, 2, _, _, 5, _, 7, 9),[](1, _, 5, _, _, 3, _, _, _),[](_, _, _, _, _, _, 6, _, _),[](_, 1, _, 4, _, _, 9, _, _),[](_, 9, _, _, _, _, _, 8, _),[](_, _, 4, _, _, 9, _, 1, _),[](_, _, 9, _, _, _, _, _, _),[](_, _, _, 1, _, _, 3, _, 6),[](6, 8, _, 3, _, _, 4, _, _)),V,R,C).
