:- lib(ic).
:- lib(lists).
:- import occurrences/3 from ic_global.
:- import nth1/3 from listut.

solve(ProblemName,B) :-
	problem(ProblemName,Hint,Rows,Cols),
	battleship(Board,Hint,Rows,Cols,B),
	print_board(Board).
	
battleship(Board,Hint,Rows,Cols,B) :-
	length(Rows,Lr),
	length(Cols,Lc),
	dim(Board,[Lr,Lc]),
	Board[1..Lr,1..Lc] :: 0..4,
	process_hint(Hint,Board),
	(for(I,1,Lr), param(Board,Rows,Cols,Lc,Lr) do
		Row is Board[I,1..Lc],
		Col is Board[1..Lr,I],
		nth1(I,Rows,X),
		nth1(I,Cols,Y),
		T1 is Lc - X,
		T2 is Lr - Y,
		occurrences(0,Row,T1),
		occurrences(0,Col,T2)
	),
	(for(A,1,Lc-1), param(Board,Lr) do
		(for(B,1,Lr-1), param(Board,A) do
			V1 is Board[A,B],
			V2 is Board[A+1,B+1],
			#>(V1,0,B1),
			#>(V2,0,B2),
			#=<(B1+B2,1,1),
			V3 is Board[A+1,B],
			V4 is Board[A,B+1],
			#>(V3,0,B3),
			#>(V4,0,B4),
			#=<(B3+B4,1,1),
			V5 is Board[A,B+1],
			V6 is Board[A+1,B],
			#=(V1,V5,C),
			#=(V1,0,D),
			#=(V5,0,E),
			C + D + E #>= 1,
			#=(V1,V6,F),
			#=(V1,0,G),
			#=(V6,0,H),
			F + G + H #>= 1
		)
	),
	Block is Board[1..Lr,1..Lc],
	flatten(Block,BoardList),
	occurrences(1,BoardList,4),
	occurrences(2,BoardList,6),
	occurrences(3,BoardList,6),
	occurrences(4,BoardList,4),
	search(Board,0,first_fail,indomain,complete,[backtrack(B)]).
	/**
	term_variables(Board, Vars),
	labeling(Vars).
	*/
print_board(Board) :- 
	dim(Board, [W,H]),
	( for(I,1,H), param(Board,W) do 
		( for(J,1,W), param(Board,I) do
			X is Board[I,J],
			( var(X) -> write(" ?") ; printf(" %1d", [X]))
		), nl
	), nl.
	
process_hint([],_).
process_hint([(R,C,V)|H],Board) :- 
	Board[R,C] #= V,
	process_hint(H,Board).
	
problem(1, [ (7,10,1), (1,6,0) ],
[2, 4, 3, 3, 2, 4, 1, 1, 0, 0] ,
[0, 5, 0, 2, 2, 3, 1, 3, 2, 2] ).

