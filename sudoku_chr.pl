:- use_module(library(chr)).
:- use_module(sudokus).
:- chr_constraint element/5, in/2, enum, print_sud/1,value/3.

solve(Sud) :- convert(Sud,Sud,1), 
%make_values(9,9), 
enum, writeln(''), print_sud(1), writeln('').

convert([],_,10) :-
    make_elements_row(9,9).
convert([Row | Rest], Sud, I) :-
    INew is I + 1,
    convert_row(Row,Row,I,1),
    convert(Rest,Sud,INew).

convert_row([],_,_,10).
convert_row([Val | Rest], Row, I, J) :-
    nonvar(Val),
    JNew is J + 1,
    block(I,J,BRow,BCol),
    element(I,J,BRow,BCol,Val),
    convert_row(Rest,Row,I,JNew).
convert_row([Val | Rest], Row, I, J) :-
    var(Val),
    JNew is J + 1,
    convert_row(Rest,Row,I,JNew).

make_elements_row(_,0).
make_elements_row(N,NRow) :-
    make_elements_col(NRow,N),
    NewRow is NRow - 1,
    make_elements_row(N, NewRow).

make_elements_col(_,0).
make_elements_col(N,NCol) :-
    block(N,NCol, BRow, BCol),
    element(N,NCol,BRow, BCol, Var),
    in(Var,[1,2,3,4,5,6,7,8,9]),
    NewCol is NCol - 1,
    make_elements_col(N,NewCol).

make_values(_,0).
make_values(N,NVal) :-
    make_values_col(NVal,N),
    NewVal is NVal - 1,
    make_values(N, NewVal).

make_values_col(_,0).
make_values_col(N,NCol) :-
    value(N,RowVar,NCol),
    in(RowVar,[1,2,3,4,5,6,7,8,9]),
    NewCol is NCol - 1,
    make_values_col(N,NewCol).

/**
other_viewpoint([Row | Rest],Sud,I) :-
    INew is I + 1,
    values_row(Row,Row,I,1),
    other_viewpoint(Rest,Sud,INew).

values_row([Val | Rest], Row, I, J) :-
    nonvar(Val),
    JNew is J + 1,
    value(Val,I,J),
    convert_row(Rest,Row,I,JNew).
*/

element(I,J,_,_,Val1) \ element(I,J,_,_,Val2), in(Val2,_) <=> nonvar(Val1), var(Val2) | true.
element(I1,J,_,_,Val) \ value(Val,I2,J) <=> nonvar(Val), nonvar(I1), var(I2) | value(Val,I1,J).

in(_,[])           <=> fail.
in(X,L1), in(X,L2) <=> L1 \== L2 | intersection(L1,L2,L), in(X,L).
in(X,L)            <=> ground(X), member(X,L) | true.
in(X,[N])          <=> X = N.

value(Val,I,J1), value(Val,I,J2) <=> J1 \== J2 | false.
value(Val1,I,J), value(Val2,I,J) <=> Val1 \== Val2 | false.
value(Val1,I,J), element(I,J,_,_,Val2) <=> Val1 \== Val2 | false.
element(I1,J,_,_,Val), element(I2,J,_,_,Val) <=> I1 \== I2 | false.
element(I,J1,_,_,Val), element(I,J2,_,_,Val) <=> J1 \== J2 | false.
element(I1,J1,BR,BC,Val), element(I2,J2,BR,BC,Val) <=> I1 \== I2, J1 \== J2 | false.

element(I1,J,_,_,X), element(I2,J,_,_,Val) \ in(X,D) <=> nonvar(Val), member(Val,D), delete(D,Val,D2), I1 \== I2 | in(X,D2). 
element(I,J1,_,_,X), element(I,J2,_,_,Val) \ in(X,D) <=> nonvar(Val), member(Val,D), delete(D,Val,D2), J1 \== J2 | in(X,D2). 
element(I1,J1,BR,BC,X), element(I2,J2,BR,BC,Val) \ in(X,D) <=> nonvar(Val), member(Val,D), delete(D,Val,D2), I1 \== I2, J1 \== J2| in(X,D2).
value(Val,X,J1), value(Val,RowVal,J2) \ in(X,D) <=> nonvar(RowVal), member(RowVal,D), delete(D,RowVal,D2), J1 \== J2 | in(X,D2).  

block(I, J, BRow, BCol) :-
    BRow is ((I - 1) // 3) + 1,
    BCol is ((J - 1) // 3) + 1.

enum(Val, [X|R]) :-
    Val = X
    ;
    enum(Val,R).

enum \ in(Val,D) <=> enum(Val,D).

print_sud(10), enum <=> true.
print_sud(I) ==> I =:= 4 ; I =:= 7 | writeln('----------------------').
print_sud(I), element(I,1,_,_,X1), element(I,2,_,_,X2), element(I,3,_,_,X3), element(I,4,_,_,X4), 
    element(I,5,_,_,X5), element(I,6,_,_,X6), element(I,7,_,_,X7), element(I,8,_,_,X8), element(I,9,_,_,X9) 
        <=> I2 is I + 1 | write(X1), write(' '), write(X2), write(' '), write(X3), write(' '), write('|'), write(' '),
            write(X4), write(' '), write(X5), write(' '), write(X6), write(' '), write('|'), write(' '), write(X7), write(' '),
                 write(X8), write(' '), writeln(X9), print_sud(I2).