:- use_module(library(chr)).
:- consult('sudokus.pl').
:- chr_constraint element/5, in/2, enum, enum/1, print_sud/1,value/3.

solve :-
    solvep(1).
solvep(19).
solvep(X) :-
    solve(X,firstfail),
    X1 is X + 1,
    solvep(X1).

solve(P, firstfail) :- puzzles(Sud,P), convert(Sud,1), enum(2), writeln(''), print_sud(1), writeln('').
solve(P, inputorder) :- puzzles(Sud,P), convert(Sud,1), enum, writeln(''), print_sud(1), writeln('').

convert([],10).
convert([Row | Rest], I) :-
    INew is I + 1,
    convert_row(Row,I,1),
    convert(Rest,INew).

convert_row([],_,10).
convert_row([Val | Rest], I, J) :-
    nonvar(Val),
    JNew is J + 1,
    block(I,J,BRow,BCol),
    element(I,J,BRow,BCol,Val),
    convert_row(Rest,I,JNew).
convert_row([Val | Rest], I, J) :-
    var(Val),
    JNew is J + 1,
    block(I,J,BRow,BCol),
    element(I,J,BRow,BCol,Val),
    in(Val,[1,2,3,4,5,6,7,8,9]),
    convert_row(Rest,I,JNew).

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

element(_,J,_,_,X), element(_,J,_,_,Val) \ in(X,D) <=> nonvar(Val), member(Val,D), delete(D,Val,D2)| in(X,D2). 
element(I,_,_,_,X), element(I,_,_,_,Val) \ in(X,D) <=> nonvar(Val), member(Val,D), delete(D,Val,D2)| in(X,D2). 
element(_,_,BR,BC,X), element(_,_,BR,BC,Val) \ in(X,D) <=> nonvar(Val), member(Val,D), delete(D,Val,D2)| in(X,D2).

in(_,[])           <=> write('B'), fail.
in(X,[N])          <=> X = N.

block(I, J, BRow, BCol) :-
    BRow is ((I - 1) // 3) + 1,
    BCol is ((J - 1) // 3) + 1.

enum(Val, [X|R]) :-
    Val = X
    ;
    enum(Val,R).

enum \ in(Val,D) <=> enum(Val,D).

enum(10) <=> true.
enum(A) \ enum(B) <=> B >= A | true.
enum(E), in(Val,D) <=> length(D,E) | enum(Val,D), enum(2).
enum(E) <=> ENew is E + 1 | enum(ENew).

print_sud(10) <=> true.
print_sud(I) ==> I =:= 4 ; I =:= 7 | writeln('----------------------').
print_sud(I), element(I,1,_,_,X1), element(I,2,_,_,X2), element(I,3,_,_,X3), element(I,4,_,_,X4), 
    element(I,5,_,_,X5), element(I,6,_,_,X6), element(I,7,_,_,X7), element(I,8,_,_,X8), element(I,9,_,_,X9) 
        <=> I2 is I + 1 | write(' '), write(X1), write(' '), write(X2), write(' '), write(X3), write(' '), write('|'), write(' '),
            write(X4), write(' '), write(X5), write(' '), write(X6), write(' '), write('|'), write(' '), write(X7), write(' '),
                 write(X8), write(' '), writeln(X9), print_sud(I2).