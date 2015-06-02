:- use_module(library(chr)).
:- consult('sudokus.pl').
:- chr_constraint enum_value/1, value/3,phase1, phase2,print_sud_value/1, enum_value.

solve :-
    solvep(1).
solvep(19).
solvep(X) :-
    solve(X,firstfail),
    X1 is X + 1,
    solvep(X1).

solve(P, firstfail) :- puzzles(Sud,P), convert(Sud,1), phase1, enum_value(2), writeln(''), print_sud_value(1), writeln('').
solve(P, inputorder) :- puzzles(Sud,P), convert(Sud,1), phase1, enum_value, writeln(''), print_sud_value(1), writeln('').

convert([],10).
convert([Row | Rest], I) :-
    INew is I + 1,
    convert_row(Row,I,1),
    convert(Rest,INew).

convert_row([],_,10).
convert_row([Val | Rest], I, J) :-
    nonvar(Val),
    JNew is J + 1,
    value(Val,I,J),
    convert_row(Rest,I,JNew).
convert_row([Val | Rest], I, J) :-
    var(Val),
    JNew is J + 1,
    make_values(I,J),
    convert_row(Rest,I,JNew).

make_values(I,J) :-
    value(1,[I],J), value(2,[I],J), value(3,[I],J), 
    value(4,[I],J), value(5,[I],J), value(6,[I],J), 
    value(7,[I],J), value(8,[I],J), value(9,[I],J). 

phase1, value(Val,I1,J) \ value(Val,I2,J) <=> number(I1), \+number(I2) | true.
phase1 \ value(Val,I1,J), value(Val,I2,J) <=> \+number(I1), \+number(I2), append(I1,I2,INew) | value(Val,INew,J).
phase1 <=> phase2.

phase2, value(Val,I1,J1), value(Val,I2,J2) <=> number(I1),number(I2), block(I1,J1,BR,BC), block(I2,J2,BR,BC), I1 \== I2| write('B'), fail.
phase2, value(Val,RowVal,J1) \ value(Val,D,J2) <=> \+number(D), number(RowVal), member(RowVal,D), same_block(RowVal,J1,D,J2), 
    remove_block_from_domain(RowVal,J1,D,J2,D2) | value(Val,D2,J2).
phase2, value(Val,RowVal,J1) \ value(Val,D,J2) <=> \+number(D), number(RowVal), J1 \== J2, member(RowVal,D), delete(D,RowVal,D2) | 
    value(Val,D2,J2).  
phase2, value(Val1,RowVal,J) \ value(Val2,D,J) <=> \+number(D), number(RowVal), Val1 \== Val2, member(RowVal,D), delete(D,RowVal,D2)| 
    value(Val2,D2,J).
phase2 \ value(_,[],_)      <=> write('B'), fail.
phase2 \ value(Val,[I],J)   <=> value(Val,I,J). 

same_block(I1,J1,[I2 | Rest], J2) :-
    (block(I1,J1,BR,BC),
    block(I2,J2,BR,BC))
    ;
    same_block(I1,J1,Rest,J2).

% 
remove_block_from_domain(_,_,[],_,[]).
remove_block_from_domain(Row2, J2, [Row1 | R], J1, NewDomain) :-
    block(Row1,J1,BR1,BC1),
    block(Row2,J2,BR2,BC2),
    ( BR1 == BR2, BC1 == BC2 ->
            (remove_block_from_domain(Row2,J2,R,J1,OldDomain),
            NewDomain = OldDomain)
            ;
            (remove_block_from_domain(Row2,J2,R,J1,OldDomain),
            NewDomain = [Row1 | OldDomain])
    ).

% The row-block and column-block of given row and column.
block(I, J, BRow, BCol) :-
    BRow is ((I - 1) // 3) + 1,
    BCol is ((J - 1) // 3) + 1.

% Try out values in list.
enum(Val, [X|R]) :-
    Val = X
    ;
    enum(Val,R).

% Enum from smallest domain to larger domains.
enum_value(10) <=> true.
enum_value(A) \ enum_value(B) <=> B >= A | true.
enum_value(E), value(Val,Row,J) <=> \+ number(Row), length(Row,E) /*write(Row),write(Val),chr_show_store(user)*/ | enum(X,Row), value(Val,X,J), enum_value(2).
enum_value(E) <=> ENew is E + 1 | enum_value(ENew).

% Enum in order.
enum_value \ value(Val,Row,J) <=> \+ number(Row) |enum(X,Row), value(Val,X,J).

% Print out solution (and remove values + phase2 from store)
print_sud_value(10), phase2 <=> true.
print_sud_value(I) ==> I == 4 ; I == 7 | writeln('----------------------').
print_sud_value(I), value(X1,I,1), value(X2,I,2), value(X3,I,3), value(X4,I,4), value(X5,I,5), value(X6,I,6), value(X7,I,7)
    , value(X8,I,8), value(X9,I,9)
        <=> I2 is I + 1 | write(X1), write(' '), write(X2), write(' '), write(X3), write(' '), write('|'), write(' '),
            write(X4), write(' '), write(X5), write(' '), write(X6), write(' '), write('|'), write(' '), write(X7), write(' '),
                 write(X8), write(' '), writeln(X9), print_sud_value(I2).
