:-lib(ic).
:-lib(listut).
:-compile("sudokus").

convert(Board,GoodBoard) :-
    (for(I,0,8), 
    fromto([],List,[A|List],GoodList),
    param(Board,GoodBoard,GoodList)
    do
        INew is 8-I,
        nth0(INew,Board,Row),
        array_list(A,Row)
    ),
    array_list(GoodBoard,GoodList).

solve(P,B,Method) :- solve([],P,B,Method).
solve([],P,B, Method) :- solve([new,original],P,B, Method).
solve(Options,P,B, Method) :-
    puzzles(BoardList,P),
    convert(BoardList,Board),
    sudoku(Board,Rboard,B, Options, Method), 
    print_board(Board).

sudoku(Board,Rboard, B,Options, Method) :-
    constraints(Board,Rboard,Options),
    (member(new,Options) -> 
      ic:search(Rboard,0,Method,indomain,complete,[backtrack(B)])
      ;
      ic:search(Board,0,Method,indomain,complete,[backtrack(B)])
    ).

constraints(Board,Rboard,Options) :-
  %dim(Board,[9,9]), 
  dim(Rboard,[9,9]),
  Board :: [1..9], 
  Rboard :: [1..9],
  (member(original,Options) -> 
    constraints_original(Board)
    ;
    true
  ),
  (member(new,Options) ->
    channel(Board,Rboard),
    constraints_new(Rboard)
    ;
    true
  ).
  %channel(Rboard,Rboard,Cboard),
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
    alldifferent(BlockList)  %,
    %new_constraints(BlockList,I,J,Board)
  ).

/*
new_constraints(Block,I,J,Board) :-
  Row is Board[I,1..9],
  J1 is J+1,
  J2 is J+2,
  RowList = [],
  for(I,1,9), param(Row,J,List) do
    (I \= J, I \= J1, I \= J2 ->
      (get_domain_as_list(I,VarList),
      append(RowList,VarList,List2),
      RowList = List2)
      ;
      true
    )
  BlockList is Block[]
*/

%constraints for our new viewpoint
constraints_new(Rboard) :-
  (for(I,1,9), param(Rboard) do 
    Row is Rboard[I,1..9], 
    Col is Rboard[1..9,I], 
    alldifferent(Row)
    %alldifferent(Col) 
  ),
  constraints_square(Rboard). %different numbers in each square

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

constraints_square(Rboard) :-
  (for(I,1,9), param(Rboard)
  do
    (for(J,1,7,3), param(Rboard,I)
    do
      V1 is Rboard[I,J],
      V2 is Rboard[I,J+1],
      V3 is Rboard[I,J+2],
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

%channel between original viewpoint (Board) and our viewpoint (Rboard)
channel(Board,Rboard) :-
  (multifor([A,B,K],1,9), param(Board,Rboard) do
    V is Board[A,B],
    V2 is Rboard[K,B],
    #=(V, K, C),
    #=(V2, A, C)
  ).
	
channel(Rboard,Rboard,Cboard) :-
  (multifor([A,B,K],1,9), param(Rboard,Rboard,Cboard) do
    V is Rboard[A,B],
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
