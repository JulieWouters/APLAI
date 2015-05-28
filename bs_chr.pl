:-use_module(library(chr)).
:-set_prolog_flag(chr_toplevel_show_store,false).
:-chr_constraint cell/3,hint/0,guess/0,row/2,col/2,boat/1.


%hints
cell(Row,Col,num(0))\cell(Row,Col,pos(_)),hint<=>true.
cell(Row,Col,num(_))\cell(Row,Col,pos(_)),row(Row,[1|R]),col(Col,[1|C]),hint<=>row(Row,R),col(Col,C).

%propagation
row(Row,[])\cell(Row,Col,pos(_))<=>cell(Row,Col,num(0)).
col(Col,[])\cell(Row,Col,pos(_))<=>cell(Row,Col,num(0)).
cell(Row,Col,num(Num))\cell(Row+1,Col+1,pos(_))<=>Num>0|cell(Row+1,Col+1,num(0)).
cell(Row,Col,num(Num))\cell(Row-1,Col+1,pos(_))<=>Num>0|cell(Row-1,Col+1,num(0)).
cell(Row,Col,num(Num))\cell(Row+1,Col-1,pos(_))<=>Num>0|cell(Row+1,Col-1,num(0)).
cell(Row,Col,num(Num))\cell(Row-1,Col-1,pos(_))<=>Num>0|cell(Row-1,Col-1,num(0)).
cell(Row,Col,num(Num))\cell(Row+1,Col,pos(_))<=>Num>0|cell(Row+1,Col,pos([0,Num])).
cell(Row,Col,num(Num))\cell(Row-1,Col,pos(_))<=>Num>0|cell(Row-1,Col,pos([0,Num])).
cell(Row,Col,num(Num))\cell(Row,Col+1,pos(_))<=>Num>0|cell(Row,Col+1,pos([0,Num])).
cell(Row,Col,num(Num))\cell(Row,Col-1,pos(_))<=>Num>0|cell(Row,Col-1,pos([0,Num])).

%guess
cell(Row,Col,pos(Pos))\guess<=>member(N,Pos),cell(Row,Col,num(N)),hint.





solve:-
	problem(1,H,R,C),
	length(R,N),
	occurences(R,C,1),
	N1 is N*N,
	create_cells(0,N,N1),
	%make_boats,
	process_hints(H),
	loop,
	print_board(0,N1,N).

loop:- find_chr_constraint(cell(_,_,pos(_))),guess,loop.
loop:- \+find_chr_constraint(cell(_,_,pos(_))),!.

occurences([],[],_).
occurences([Row|R],[Col|C],N) :-
	N1 is N+1,
	make_list(Row,[],R1),
	make_list(Col,[],C1),
	row(N,R1),
	col(N,C1),
	occurences(R,C,N1).
	
make_list(0,R,R).
make_list(T,L,R) :-
	T > 0,
	T1 is T - 1,
	make_list(T1,[1|L],R).
	
make_boats :-
	make_boat(1,5),
	make_boat(2,6),
	make_boat(3,6),
	make_boat(4,4).
	
make_boat(_,0).
make_boat(B,T) :-
	T > 0,
	T1 is T - 1,
	boat(B),
	make_boat(B,T1).

create_cells(S,_,S):-!.
create_cells(P,N,S):-
	NewP is P+1,
	create_cells(NewP,N,S),
	Row is P//N + 1,
	Col is P mod N + 1,
	cell(Row,Col,pos([0,1,2,3,4])).
	
process_hints([]) :- !.
process_hints([(R,C,V)|H]) :-
	process_hints(H),
	cell(R,C,num(V)),hint.
	

%prints the board
print_board(_,0,_). 
print_board(P,T,N):-
	Row is P//N + 1,
	Col is P mod N + 1,
	( find_chr_constraint(cell(Row,Col,num(_))) ->
		find_chr_constraint(cell(Row,Col,num(V)))
	;
		find_chr_constraint(cell(Row,Col,pos([V|_])))
	),
	write(V),write(' '),
	(Col == N -> nl;true),
	NewP is P+1,
	NewT is T-1,
	print_board(NewP,NewT,N).

%example board:
problem(1, [ (7,10,1), (1,6,0) ],
[2, 4, 3, 3, 2, 4, 1, 1, 0, 0] ,
[0, 5, 0, 2, 2, 3, 1, 3, 2, 2] ).

