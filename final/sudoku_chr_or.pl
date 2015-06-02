:- use_module(library(chr)).
:- consult('sudokus.pl').
:- chr_constraint element/5, in/2, enum, enum/1, print_sud/1,value/3.

% Solve all puzzles one after the other.
solve :-
    solvep(1).
solvep(19).
solvep(X) :-
    time(solve(X,firstfail)),
    X1 is X + 1,
    solvep(X1).

% Solve a given puzzle (name or number) with either firstfail or input order.
solve(P, firstfail) :- puzzles(Sud,P), convert(Sud,1), enum(2), writeln(''), print_sud(1), writeln('').
solve(P, inputorder) :- puzzles(Sud,P), convert(Sud,1), enum, writeln(''), print_sud(1), writeln('').

% Convert puzzle to elements.
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

% Remove values in the same row/column/block from domain.
element(_,J,_,_,X), element(_,J,_,_,Val) \ in(X,D) <=> nonvar(Val), member(Val,D), delete(D,Val,D2)| in(X,D2). 
element(I,_,_,_,X), element(I,_,_,_,Val) \ in(X,D) <=> nonvar(Val), member(Val,D), delete(D,Val,D2)| in(X,D2). 
element(_,_,BR,BC,X), element(_,_,BR,BC,Val) \ in(X,D) <=> nonvar(Val), member(Val,D), delete(D,Val,D2)| in(X,D2).
% Backtrack if domain empty
in(_,[])           <=> write('B'), fail.
% If only one value left in domain, select this value.
in(X,[N])          <=> X = N.

% The row-block and column-block of given row and column.
block(I, J, BRow, BCol) :-
    BRow is ((I - 1) // 3) + 1,
    BCol is ((J - 1) // 3) + 1.

% Try out values in list.
enum(Val, [X|R]) :-
    Val = X
    ;
    enum(Val,R).

% Enum in order.
enum \ in(Val,D) <=> enum(Val,D).

% Enum from smallest domain to larger domains.
enum(10) <=> true.
enum(A) \ enum(B) <=> B >= A | true.
enum(E), in(Val,D) <=> length(D,E) | enum(Val,D), enum(2).
enum(E) <=> ENew is E + 1 | enum(ENew).

% Print out solution
print_sud(10) <=> true.
print_sud(I) ==> I =:= 4 ; I =:= 7 | writeln('----------------------').
print_sud(I), element(I,1,_,_,X1), element(I,2,_,_,X2), element(I,3,_,_,X3), element(I,4,_,_,X4), 
    element(I,5,_,_,X5), element(I,6,_,_,X6), element(I,7,_,_,X7), element(I,8,_,_,X8), element(I,9,_,_,X9) 
        <=> I2 is I + 1 | write(' '), write(X1), write(' '), write(X2), write(' '), write(X3), write(' '), write('|'), write(' '),
            write(X4), write(' '), write(X5), write(' '), write(X6), write(' '), write('|'), write(' '), write(X7), write(' '),
                 write(X8), write(' '), writeln(X9), print_sud(I2).