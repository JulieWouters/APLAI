% author: Or Sharir <tnidtnid@GMAIL.COM>

:-use_module(library(chr)).
:-set_prolog_flag(chr_toplevel_show_store,false).
:-chr_constraint cell/3, phase1/0, phase2/0,columns/1.

phase1, columns([]) <=> true.
phase1, cell(R,0,num(1)), columns(L) <=> select(C,L,L1) | cell(R,C,num(1)), columns(L1).
cell(R,C,num(1)) \ cell(R,C,pos(_)) <=> true.
cell(R1,C1,num(1)), cell(R2,C2,num(1)) <=> R1 = R2, C1 = C2 | fail.
cell(R1,C1,num(1)), cell(R2,C2,num(1)) <=> R1 = R2+1, C1 = C2+1 | fail.
cell(R1,C1,num(1)), cell(R2,C2,num(1)) <=> R1 = R2+1, C1 = C2-1 | fail.
cell(R1,C1,num(1)), cell(R2,C2,num(1)) <=> R1 = R2-1, C1 = C2+1 | fail.
cell(R1,C1,num(1)), cell(R2,C2,num(1)) <=> R1 = R2-1, C1 = C2-1 | fail.
phase2, cell(R,C,pos(_)) <=> cell(R,C,num(0)).

loop_phase1 :- find_chr_constraint(columns(_)), phase1, loop_phase1.
loop_phase1 :- \+find_chr_constraint(columns(_)), loop_phase2.

loop_phase2 :- find_chr_constraint(cell(_,_,pos(_))), phase2, loop_phase2.
loop_phase2 :- \+find_chr_constraint(cell(_,_,pos(_))), !.

solve:-
	problem(1,_,R,C),
	length(R,N),
	N1 is N*N,
	occurences(R,C,1,[],Rows,[],Cols),
	columns(Cols),
	make_coordinates(Rows),
	create_cells(0,N,N1),
	loop_phase1,
	print_board(0,N1).

occurences([],[],_,R,R,C,C) :-!.
occurences([Row|R],[Col|C],N,Tr,Rows,Tc,Cols) :-
	N1 is N+1,
	make_line(Row,[],Tr1,N),
	make_line(Col,[],Tc1,N),
	append(Tr,Tr1,Tr2),
	append(Tc,Tc1,Tc2),
	occurences(R,C,N1,Tr2,Rows,Tc2,Cols).
	
make_line(0,F,F,_).
make_line(T,R,F,I) :-
	T > 0,
	T1 is T - 1,
	make_line(T1,[I|R],F,I).
	
make_coordinates([]).
make_coordinates([Row|R]) :-
	cell(Row,0,num(1)),
	make_coordinates(R).
	
unique(_,_,[]).
unique(R,C,[(R1,C1)|L]) :-
	(
		R \= R1
	;
		C \= C1
	),
	unique(R,C,L).

create_cells(N,_,N):-!.
create_cells(P,S,N):-
	NewP is P+1,
	create_cells(NewP,S,N),
	Row is P//S + 1,
	Col is P mod S + 1,
	cell(Row,Col,pos([0,1])).

print_board(N,N).
print_board(P,N):-
	Row is P//10 + 1,
	Col is P mod 10 + 1,
	find_chr_constraint(cell(Row,Col,num(N))),
	write(N),write(' '),
	(Col == 10 -> nl;true),
	NewP is P+1,
	print_board(NewP).

%example board:
problem(1, [ (7,10,1), (1,6,0) ],
[2, 4, 3, 3, 2, 4, 1, 1, 0, 0] ,
[0, 5, 0, 2, 2, 3, 1, 3, 2, 2] ).

