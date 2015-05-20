:- use_module(library(chr)).
:- use_module(sudokus).
:- chr_constraint element/5, in/2, enum/1, print_sud/1,value/3.

solve(Sud) :- convert(Sud,Sud,1), loop, writeln(''), print_sud(1), writeln('').

convert([],_,10) :-
    make_values(9,9).
convert([Row | Rest], Sud, I) :-
    INew is I + 1,
    convert_row(Row,Row,I,1),
    convert(Rest,Sud,INew).

convert_row([],_,_,10).
convert_row([Val | Rest], Row, I, J) :-
    nonvar(Val),
    JNew is J + 1,
    value(Val,I,J),
    convert_row(Rest,Row,I,JNew).
convert_row([Val | Rest], Row, I, J) :-
    var(Val),
    JNew is J + 1,
    convert_row(Rest,Row,I,JNew).

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

value(Val,I1,J) \ value(Val,I2,J), in(I2,_) <=> nonvar(I1), var(I2) | true.

in(_,[])           <=> fail.
in(X,[N])          <=> X = N.

value(Val,I,J1), value(Val,I,J2) <=> J1 \== J2 | false.
value(Val1,I,J), value(Val2,I,J) <=> Val1 \== Val2 | false.
value(Val,I1,J1), value(Val,I2,J2) <=> nonvar(I1),nonvar(I2), block(I1,J1,BR1,BC1), block(I2,J2,BR2,BC2), BR1 =:= BR2, BC1 =:= BC2 | false.

value(Val,X,J1), value(Val,RowVal,J2) \ in(X,D) <=> nonvar(RowVal), member(RowVal,D), delete(D,RowVal,D2), J1 \== J2 | in(X,D2).  

block(I, J, BRow, BCol) :-
    BRow is ((I - 1) // 3) + 1,
    BCol is ((J - 1) // 3) + 1.

enum(Val, [X|R]) :-
    Val = X
    ;
    enum(Val,R).

loop :-
    \+ find_chr_constraint(in(_,_)), !.
loop :-
    find_chr_constraint(in(_,_)),
    enum(2),
    loop.

enum(10) <=> true.
enum(A) \ enum(B) <=> B >= A | true.
enum(E) \ in(Row,D) <=> length(D,E) | enum(Row,D), enum(2).
enum(E) <=> ENew is E + 1 | enum(ENew).

print_sud(10) <=> true.
print_sud(I) ==> I =:= 4 ; I =:= 7 | writeln('----------------------').
print_sud(I), value(X1,I,1), value(X2,I,2), value(X3,I,3), value(X4,I,4), value(X5,I,5), value(X6,I,6), value(X7,I,7)
    , value(X8,I,8), value(X9,I,9)
        <=> I2 is I + 1 | write(X1), write(' '), write(X2), write(' '), write(X3), write(' '), write('|'), write(' '),
            write(X4), write(' '), write(X5), write(' '), write(X6), write(' '), write('|'), write(' '), write(X7), write(' '),
                 write(X8), write(' '), writeln(X9), print_sud(I2).

board([[6,_,_,7,_,_,5,_,_],
       [_,2,8,_,_,_,_,_,_],
       [_,_,_,6,4,_,3,_,_],
       [7,4,_,_,_,_,_,2,_],
       [_,_,1,_,_,_,8,_,_],
       [_,5,_,_,_,_,_,3,7],
       [_,_,3,_,7,6,_,_,_],
       [_,_,_,_,_,_,1,9,_],
       [_,_,4,_,_,5,_,_,8]]).
