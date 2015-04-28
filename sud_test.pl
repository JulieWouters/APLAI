:- lib(ic).
:- lib(gfd).

solve :-
	sudoku(Rboard,Cboard),
	print_board(Rboard),
	print_board(Cboard).
	
sudoku(Rboard,Cboard) :- 
	constraints(Rboard,Cboard),
	search(Rboard),
	search(Cboard).
	
constraints(Rboard,Cboard) :-
	constraints_rows(Rboard),
	constraints_cols(Cboard),
	dim(Fboard,[9,9]),
	Fboard[1..9,1..9]::[1..9],
	(for(I,1,9), param(Rboard,Cboard,Fboard) do
		(for(J,1,9), param(Rboard,Cboard,Fboard) do
			R is Rboard[I,J],
			C is Cboard[I,J],
			Fboard[I,C] is R
		),
		Col is Fboard[1..9,I],
		alldifferent(Col)
	).
constraints_rows(Rboard) :-
	dim(Rboard,[9,9]),
	(for(I,1,9), param(Rboard) do
		(for(J,1,9,3), param(Rboard,I) do
			Block is Rboard[I,J..J+2],
			Block::[J..J+2]
		),
		Row is Rboard[I,1..9],
		alldifferent(Row)
	).
	
constraints_cols(Cboard) :-
	dim(Cboard,[9,9]),
	(for(I,1,9), param(Cboard) do
		Row is Cboard[I,1..9],
		alldifferent(Row),
		(for(J,1,9,3), param(Cboard,I) do
			Cboard[I,J]::[1..3],
			Cboard[I,J+1]::[4..6],
			Cboard[I,J+2]::[7..9]
		)
	).
	
search(Board) :-
	term_variables(Board,Vars),
	labeling(Vars).
	
print_board(P) :- 
	dim(P, [N,N]),
	( for(I,1,N), param(P,N) do 
		( for(J,1,N), param(P,I) do
			X is P[I,J],
			( var(X) -> write("  _") ; printf(" %2d", [X]))
		), nl
	), nl.