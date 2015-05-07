:- lib(ic).
:- lib(lists).
:- import occurrences/3 from ic_global.
:- import sumlist/2 from ic_global.
:- import nth1/3 from listut.
:- import nth0/3 from listut.

solve(ProblemName,B) :-
	problem(ProblemName,Hint,Rows,Cols),
	battleship(Board,Hint,Rows,Cols,B),
	print_board(Board).
	
battleship(Board,Hint,Rows,Cols,B) :-
	length(Rows,N),
	N1 is N + 2,
	dim(Board,[N1,N1]),
	Board[1..N1,1..N1] :: 0..4,
	Board[1,1..N1] :: 0,
	Board[N1,1..N1] :: 0,
	Board[1..N1,1] :: 0,
	Board[1..N1,N1] :: 0,
	process_hint(Hint,Board),
	water(Board,N,Rows,Cols),
	Block is Board[1..N1,1..N1],
	flatten(Block,BoardList),
	occurrences(1,BoardList,4),
	occurrences(2,BoardList,6),
	occurrences(3,BoardList,6),
	occurrences(4,BoardList,4),
	(for(X,2,N+1), param(Board,N,N1) do
		(for(Y,2,N+1), param(Board,X,N1) do
			diagonal_constraints(Board,X,Y),
			horizontal_vertical_constraints(Board,X,Y),
			ship_size_constraints(Board,X,Y,N1)
		)
	),
	search(Board,0,first_fail,indomain,complete,[backtrack(B)]).
	
water(Board,N,Rows,Cols) :- 
	(for(A,2,N+1), param(Board,N,Rows,Cols) do
		Row is Board[A,2..N+1],
		Col is Board[2..N+1,A],
		J is A-1,
		nth1(J,Rows,X),
		nth1(J,Cols,Y),
		T1 is N - X,
		T2 is N - Y,
		occurrences(0,Row,T1),
		occurrences(0,Col,T2)
	).
	
diagonal_constraints(Board,X,Y) :-
	VC is Board[X,Y],
	VNW is Board[X-1,Y-1],
	VNE is Board[X-1,Y+1],
	VSE is Board[X+1,Y+1],
	VSW is Board[X+1,Y-1],
	#>(VC,0,BC),
	#>(VNW,0,BNW),
	#>(VNE,0,BNE),
	#>(VSE,0,BSE),
	#>(VSW,0,BSW),
	#=<(BC+BNW,1,1),
	#=<(BC+BNE,1,1),
	#=<(BC+BSE,1,1),
	#=<(BC+BSW,1,1).
	
horizontal_vertical_constraints(Board,X,Y) :-
	VC is Board[X,Y],
	VN is Board[X-1,Y],
	VS is Board[X+1,Y],
	VW is Board[X,Y-1],
	VE is Board[X,Y+1],
	#=(VC,0,BC),
	#=(VN,0,BN),
	#=(VS,0,BS),
	#=(VW,0,BW),
	#=(VE,0,BE),
	#=(VC,VN,N),
	#=(VC,VS,S),
	#=(VC,VW,W),
	#=(VC,VE,E),
	BC + BN + N #>= 1,
	BC + BS + S #>= 1,
	BC + BW + W #>= 1,
	BC + BE + E #>= 1.
	
ship_size_constraints(Board,X,Y,N1) :-
	VC is Board[X,Y],
	Urev is Board[1..X,Y],
	reverse(Urev,U),
	cnt(U,Su),
	D is Board[X..N1,Y],
	cnt(D,Sd),
	Lrev is Board[X,1..Y],
	reverse(Lrev,L),
	cnt(L,Sl),
	R is Board[X,Y..N1],
	cnt(R,Sr),
	Su + Sd + Sl + Sr + 1 #= VC.
	
cnt(L1,S) :-
	length(L1,L),
	length(L2,L),
	nth1(1,L1,I),
	nth1(2,L1,J),
	nth1(1,L2,K),
	#>(I,0,V1),
	#>(J,0,V2),
	#=(V1+V2,2,K),
	(for(X,2,L), param(L1,L2) do
		X1 is X - 1,
		nth1(X1,L2,A),
		nth1(X,L1,B),
		nth1(X,L2,C),
		#>(A,0,T1),
		#>(B,0,T2),
		#=(T1+T2,2,C)
	),
	sumlist(L2,S).

print_board(Board) :- 
	dim(Board, [N,N]),
	( for(I,2,N-1), param(Board,N) do 
		( for(J,2,N-1), param(Board,I) do
			X is Board[I,J],
			( var(X) -> write(" ?") ; printf(" %1d", [X]))
		), nl
	), nl.
	
process_hint([],_).
process_hint([(R,C,V)|H],Board) :- 
	Board[R+1,C+1] #= V,
	process_hint(H,Board).
	
problem(1, [ (7,10,1), (1,6,0) ],
[2, 4, 3, 3, 2, 4, 1, 1, 0, 0] ,
[0, 5, 0, 2, 2, 3, 1, 3, 2, 2] ).
