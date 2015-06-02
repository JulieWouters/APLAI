:- use_module(library(chr)).
:- use_module(sudokus).
:- chr_constraint element/5, in/2, enum/1, enum_value/1, print_sud/1,value/3,phase1, phase2,remove_value, enum, enum_value.

solve :-
    solvep(1).
solvep(19).
solvep(X) :-
    solve(X,firstfail),
    X1 is X + 1,
    solvep(X1).

solve(P,firstfail) :- puzzles(Sud,P), convert(Sud,1), phase1, enum(2), writeln(''), print_sud(1), writeln(''), remove_value, writeln('').
solve(P,inputorder) :- puzzles(Sud,P), convert(Sud,1), phase1, enum, writeln(''), print_sud(1), writeln(''), remove_value, writeln('').

l(L) :-
    length(L,Le),
    print(Le).

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
    value(Val,I,J),
    convert_row(Rest,I,JNew).
convert_row([Val | Rest], I, J) :-
    var(Val),
    JNew is J + 1,
    block(I,J, BRow, BCol),
    element(I,J,BRow,BCol,Var),
    in(Var,[1,2,3,4,5,6,7,8,9]),
    make_values(I,J),
    convert_row(Rest,I,JNew).

make_values(I,J) :-
    value(1,[I],J), value(2,[I],J), value(3,[I],J), 
    value(4,[I],J), value(5,[I],J), value(6,[I],J), 
    value(7,[I],J), value(8,[I],J), value(9,[I],J). 

phase1, value(Val,I1,J) \ value(Val,I2,J) <=> number(I1), \+number(I2) | true.
phase1 \ value(Val,I1,J), value(Val,I2,J) <=> \+number(I1), \+number(I2), append(I1,I2,INew) | value(Val,INew,J).
phase1 <=> phase2.

phase2, element(I1,J,_,_,Val) \ value(Val,I2,J) <=> \+number(I2) | value(Val,I1,J).
phase2, value(Val1,I,J) \ element(I,J,BR,BC,Val2), in(Val2,_) <=> number(I) | element(I,J,BR,BC,Val1).

%phase2, value(Val,I1,J1), value(Val,I2,J2) <=> number(I1),number(I2), block(I1,J1,BR,BC), block(I2,J2,BR,BC), I1 \== I2| fail.

phase2, value(Val,RowVal,J1) \ value(Val,D,J2) <=> \+number(D), number(RowVal), member(RowVal,D), same_block(RowVal,J1,D,J2), 
    remove_block_from_domain(RowVal,J1,D,J2,D2) | value(Val,D2,J2).
phase2, value(Val,RowVal,J1) \ value(Val,D,J2) <=> \+number(D), number(RowVal), J1 \== J2, member(RowVal,D), delete(D,RowVal,D2) | 
    value(Val,D2,J2).  
phase2, value(Val1,RowVal,J) \ value(Val2,D,J) <=> \+number(D), number(RowVal), Val1 \== Val2, member(RowVal,D), delete(D,RowVal,D2)/*, print('5')*/| 
    value(Val2,D2,J).
phase2 \ value(_,[],_)      <=> write('B'), fail.
phase2 \ value(Val,[I],J)   <=> value(Val,I,J). 

phase2, element(I1,J,_,_,X), element(I2,J,_,_,Val) \ in(X,D) <=> nonvar(Val), member(Val,D), delete(D,Val,D2), I1 \== I2 | in(X,D2). 
phase2, element(I,J1,_,_,X), element(I,J2,_,_,Val) \ in(X,D) <=> nonvar(Val), member(Val,D), delete(D,Val,D2), J1 \== J2 | in(X,D2). 
phase2, element(I1,J1,BR,BC,X), element(I2,J2,BR,BC,Val) \ in(X,D) <=> nonvar(Val), member(Val,D), delete(D,Val,D2), I1 \== I2, J1 \== J2| in(X,D2).
phase2 \ in(_,[])           <=> write('B'), fail.
phase2 \ in(X,[N])          <=> X = N.

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

% Enum from smallest domain to larger domains. Switching from elements to values.
enum(10) <=> true.
enum_value(10) <=> true.
enum(A) \ enum(B) <=> B >= A | true.
enum_value(A) \ enum_value(B) <=> B >= A | true.
enum(E), in(Val,D) <=> length(D,E) | enum(Val,D), enum_value(2).
enum_value(E), value(Val,Row,J) <=> \+ number(Row), length(Row,E) | enum(X,Row), value(Val,X,J), enum(2).
enum(E) <=> ENew is E + 1 | enum(ENew).
enum_value(E) <=> ENew is E + 1 | enum_value(ENew).

% Enum in order. Switching from elements to values.
enum_value \ value(Val,Row,J) <=> \+ number(Row) |enum(X,Row), value(Val,X,J).
enum, in(Val,D) <=> enum(Val,D), enum_value.

%phase2, value(Val,I1,J1), value(Val,I2,J2) <=> number(I1),number(I2), block(I1,J1,BR,BC), block(I2,J2,BR,BC), I1 \== I2| write('B'),fail.

% Print out solution (and remove elements + phase2 from store)
print_sud(10), phase2 <=> true.
print_sud(I) ==> I =:= 4 ; I =:= 7 | writeln('----------------------').
print_sud(I), element(I,1,_,_,X1), element(I,2,_,_,X2), element(I,3,_,_,X3), element(I,4,_,_,X4), 
    element(I,5,_,_,X5), element(I,6,_,_,X6), element(I,7,_,_,X7), element(I,8,_,_,X8), element(I,9,_,_,X9) 
        <=> I2 is I + 1 | write(X1), write(' '), write(X2), write(' '), write(X3), write(' '), write('|'), write(' '),
            write(X4), write(' '), write(X5), write(' '), write(X6), write(' '), write('|'), write(' '), write(X7), write(' '),
                 write(X8), write(' '), writeln(X9), print_sud(I2).

% Remove values from store
remove_value \ value(_,_,_) <=> true.
remove_value <=> true.
