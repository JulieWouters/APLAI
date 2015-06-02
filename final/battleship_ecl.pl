:- lib(ic).
:- lib(lists).
:- import occurrences/3 from ic_global.
:- import sumlist/2 from ic_global.
:- import nth1/3 from listut.
:- import nth0/3 from listut.

solve(ProblemName,B) :-
	problem(ProblemName,Hint,Rows,Cols),
	battleship(Board,Hint,Rows,Cols,B),
	print_board(Board,Rows,Cols).
	
/**
* Main part of the solution:
* - set dimension of the board
* - establish the domains of the squares on it (outside border only have domain [0])
* - set fixed amount of occurences for ship values on the board (e.g. 1 occurs 4 times) 
* - set constraints for each of the individual squares
* - take hints into account
* - initiate search
**/
battleship(Nboard,Hint,Rows,Cols,B) :-
	length(Rows,N),
	N1 is N + 2,
	dim(Board,[N1,N1]),
	Board[1..N1,1..N1] :: 0..4,
	Board[1,1..N1] :: 0,
	Board[N1,1..N1] :: 0,
	Board[1..N1,1] :: 0,
	Board[1..N1,N1] :: 0,
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
			%horizontal_vertical_constraints(Board,X,Y),
			ship_size_constraints(Board,X,Y,N1)
		)
	),
	transform(Board,Nboard),
	process_hint(Hint,Nboard),
	search(Nboard,0,largest,indomain,complete,[backtrack(B)]).
	
/**
* Set amount of occuring 0 values for each row and column 
**/
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
	
/**
* Squares diagonally adjacent to a square with value greater than 0, should be 0
**/
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
	
/**
* Squares horizontally or vertically adjacent of each other can either have the same value or be 0
**/
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
	
/**
* A square with value v should be part of a chain with size v consisting of values greater than 0
**/
ship_size_constraints(Board,X,Y,N1) :-
	V1 is Board[X,Y],
	#>(V1,0,T),
	dim(Cboard,[N1,N1]),
	Cboard[1..N1,Y] :: 0..1,
	Cboard[X,1..N1] :: 0..1,
	Cboard[1..X-1,1..Y-1] :: 0,
	Cboard[1..X-1,Y+1..N1] :: 0,
	Cboard[X+1..N1,1..Y-1] :: 0,
	Cboard[X+1..N1,Y+1..N1] :: 0,
	V2 is Cboard[X,Y],
	#=(V2,T),
	Block is Cboard[1..N1,1..N1],
	flatten(Block,Clist),
	occurrences(1,Clist,V1),
	UpRev is Board[1..X,Y],
	reverse(UpRev,Up),
	UpRevC is Cboard[1..X,Y],
	reverse(UpRevC,UpC),
	cnt(Up,UpC),
	Down is Board[X..N1,Y],
	DownC is Cboard[X..N1,Y],
	cnt(Down,DownC),
	LeftRev is Board[X,1..Y],
	reverse(LeftRev,Left),
	LeftRevC is Cboard[X,1..Y],
	reverse(LeftRevC,LeftC),
	cnt(Left,LeftC),
	Right is Board[X,Y..N1],
	RightC is Cboard[X,Y..N1],
	cnt(Right,RightC).
	
cnt(L1,L2) :-
	length(L1,L),
	(for(X,2,L), param(L1,L2) do
		X1 is X-1,
		nth1(X1,L2,A),
		nth1(X,L1,B),
		nth1(X,L2,C),
		#>(A,0,T1),
		#>(B,0,T2),
		#=(T1+T2,2,C)
	).
	
/**
* determine the orientation of the vessels on the board
**/
transform(Oboard,Nboard) :-
	dim(Oboard,[N,N]),
	dim(Nboard,[N,N]),
	Nboard[1,1..N] :: 0,
	Nboard[N,1..N] :: 0,
	Nboard[1..N,1] :: 0,
	Nboard[1..N,N] :: 0,
	Nboard[2..N-1,2..N-1] :: 0..6,
	(for(X,2,N-1), param(Oboard,Nboard,N) do
		(for(Y,2,N-1), param(Oboard,Nboard,X) do
			VO is Oboard[X,Y],
			VN is Nboard[X,Y],
			VU is Oboard[X-1,Y],
			VD is Oboard[X+1,Y],
			VL is Oboard[X,Y-1],
			VR is Oboard[X,Y+1],
			#>(VO,0,B),
			#=(VU,0,BU),
			#=(VD,0,BD),
			#=(VL,0,BL),
			#=(VR,0,BR),
			C #= BU + BD + BL + BR,
			#>(VN,0,B),
			and(B,#=(C,4),B1),
			#=(VN,1,B1),
			and(B,#=(C,2),B2),
			#=(VN,2,B2),
			and(B,#=(C,3),BG),
			and(BG,neg(BD),B3),
			#=(VN,3,B3),
			and(BG,neg(BU),B4),
			#=(VN,4,B4),
			and(BG,neg(BR),B5),
			#=(VN,5,B5),
			and(BG,neg(BL),B6),
			#=(VN,6,B6)
		)
	).
	
val(0,'.',water).
val(1,'c',circle).
val(2,'m',middle).
val(3,'t',top).
val(4,'b',bottom).
val(5,'l',left).
val(6,'r',right).

print_board(Board,R,C) :- 
	dim(Board, [N,N]),
	( for(I,2,N-1), param(Board,N,R) do 
		( for(J,2,N-1), param(Board,I) do
			X is Board[I,J],
			( var(X) -> write("?") ; val(X,Y,_), write(Y))
		), 
		write(" "),
		I1 is I-1,
		nth1(I1,R,V),
		write(V),
		nl
	),
	(for(K,1,N-2), param(C) do
		nth1(K,C,W),
		write(W)
	), nl.
	
process_hint([],_).
process_hint([(R,C,V)|H],Board) :- 
	val(V1,_,V),
	Board[R+1,C+1] #= V1,
	process_hint(H,Board).
	
problem(1, [ (7,10,circle), (1,6,water) ],
[2, 4, 3, 3, 2, 4, 1, 1, 0, 0] ,
[0, 5, 0, 2, 2, 3, 1, 3, 2, 2] ).

problem( 35, [ (8,1,circle), (8,7,right), (3,4,water), (4,6,water) ], 
 			[1, 0, 3, 1, 1, 2, 2, 4, 1, 5] , 
 			[3, 1, 5, 1, 0, 3, 2, 2, 1, 2] ).
			
problem( 13, [ (5,10,water), (2,10,water) ], 
 			[0, 3, 2, 0, 5, 2, 5, 2, 1, 0] , 
 			[3, 2, 0, 1, 2, 2, 3, 2, 3, 2] ).
			
problem( 15, [ (2,6,water) ], 
 			[0, 1, 1, 0, 4, 5, 1, 1, 2, 5] , 
 			[2, 0, 3, 1, 2, 2, 4, 1, 2, 3] ).
