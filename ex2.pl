%Gal Tfilin and Niv Lipetz

:- use_module(naive_sat).

% PART 1 - NanoGrams

% Task 1 - is_nanogram

is_nonogram(nonogram(N, M, ColData, RowData), Solution) :-
    is_nanogram_by_rows(N, RowData, Solution),
    transpose(Solution, TransposedSolution),
    is_nanogram_by_rows(M, ColData, TransposedSolution).

is_nanogram_by_rows(N, [RowData|RestRowData], [RowSolution|RestRowSolution]) :-
    N > 0,
    N1 is N - 1,
    verif_row_data(RowData, RowSolution),                           %verify row matches the solution
    is_nanogram_by_rows(N1, RestRowData, RestRowSolution).          %check next row

is_nanogram_by_rows(0,[],[]).                                       %iterated over all rows so finish successfully

verif_row_data(RowData, [0|RowSolution]) :-                         %current element in solution is 0, go to next element
    verif_row_data(RowData, RowSolution).

verif_row_data([RowData|RestRowData], [1|RowSolution]) :-           %current element in solution is 1, count the number of 1s and check that it equals the rowdata element
    CountRowData is RowData - 1,
    verif_row_data([CountRowData|RestRowData], RowSolution).

verif_row_data([0|RestRowData],[0|RestRowSolution]) :-              %series of 1s matches the solution so both are 0|_ 0|_
    verif_row_data(RestRowData, RestRowSolution).

verif_row_data([0],[]).                                             %finished going through one row successfully

% Nanogram utils
transpose(Rows, []) :-
    null_rows(Rows).

transpose(Rows, [FirstCol|RestCols]) :-
    make_row(Rows, FirstCol, RestRows),
    transpose(RestRows, RestCols).

make_row([[X|RestRow]|Rows], [X|Col], [RestRow|RestRows]) :-
    make_row(Rows, Col, RestRows).

make_row([],[],[]).

null_rows([[]|Rows]) :-
    null_rows(Rows).

null_rows([]).

%-----------------------------------------------------%
% Task 2 - nonogram_solve

% heuristics => do the largest row first
% row, column, row, column
% fail if not enough space for rest of row

nonogram_solve(nonogram(N, M, ColData, RowData), Solution) :-
    generate_matrix(N, M, Solution),                                %create matrix of N X M free variables
    transpose(Solution, TransposedSolution),
    try_solve_row_simple_boxes(M, ColData, TransposedSolution),     %fill in necessary boxes for columns
    try_solve_row_simple_boxes(N, RowData, Solution),               %fill in necessary boxes for rows
    solve_nanogram().

try_solve_row_simple_boxes(N, [Data|RestData], [Row|RestRows]) :-
    N > 0,
    simple_box(N, Data, Row),
    N1 is N - 1,
    try_solve_row_simple_boxes(N1, RestData, RestRows).

try_solve_row_simple_boxes(0, [], []).

generate_matrix(N, M, [Row|RestRows]) :-                            
    N > 0,
    N1 is N - 1,
    length(Row, M),
    generate_matrix(N1, M, RestRows).

generate_matrix(0, _, []).


% Algorithms for nanogram solver

% Simple box

simple_box(_, [], []).

simple_box(N, [0|RestData], []) :-
    simple_box(N, RestData, []).

simple_box(N, Data, Row) :-
    row_sum(Data, 0, RowSum),
    length(Data, NumberOfDataElements),
    NumberOfDataElements > 0,
    NumberOfMinimalZeros is NumberOfDataElements - 1,
    RowSumWithZeros is RowSum + NumberOfMinimalZeros,
    NumberOfBoxesLeft is N - RowSumWithZeros,
    solve_simple_box(NumberOfBoxesLeft, Data, Row, NumberOfBoxesLeft).

solve_simple_box(_, [], _, _).                                             %all data is put into the row successfully

solve_simple_box(0, [0|RestData], [0|RestRow], 0):-                      %Keep going in the data to fill the rest of the 0s
	solve_simple_box(0, RestData, RestRow, 0).

solve_simple_box(0, [0|RestData], [], 0) :-                                
    solve_simple_box(0, RestData, [], 0).

solve_simple_box(NumberOfBoxesLeft, [CurrentElement|RestData], [1|RestRow], 0) :-
    CurrentElement > 0,
    NumberOfBoxesLeft > 0,
    UpdatedCurrentElement is CurrentElement - 1,
    solve_simple_box(NumberOfBoxesLeft, [UpdatedCurrentElement|RestData], RestRow, 0).

solve_simple_box(NumberOfBoxesLeft, [0|RestData], [_|RestRow], _) :-      %finished a piece
    NumberOfBoxesLeft > 0,
    solve_simple_box(NumberOfBoxesLeft, RestData, RestRow, _).


row_sum([Element|RestData], Init, Sum) :-
    Sum1 is Init + Element,
    row_sum(RestData, Sum1, Sum).

row_sum([], Init, Sum) :-
    Init = Sum.



%-----------------------------------------------------%
% Task 3 - A Ramsey Verifier

% edge(X+, Y+, Graph+, Color+) - true if X and Y vertexes are connected by an edge
edge(X, Y, [Row | _], Color) :- 
    X is 1,
    nth1(Y, Row, Color).

edge(X, Y, [_ | T], Color) :- 
    X > 1,
    Z is X - 1,
    edge(Z, Y, T, Color).

% clique(VertexList+, Graph+, Color+) - true if the vertexes listed in VertexList list are a clique in Graph with edges of color Color
clique([], _, _).
clique([_], _, _).
clique([X1,X2|R], Graph, Color) :- 
    edge(X1, X2, Graph, Color), 
    clique([X1|R], Graph, Color), 
    clique([X2|R], Graph, Color).

% cliqueN(C-, N+, Graph+, Color+) - true exists a C, sublist of the vertexes of size N, which is a clique of edges with color Color
% inspired from https://en.wikibooks.org/wiki/Introduction_to_Programming_Languages/Exhaustive_Searches
cliqueN(C, N, [H | T], Color) :- 
    length(H, Len),
    findall(Num, between(1, Len, Num), VertexList),
    sublist(C, VertexList), 
    length(C, N),
    clique(C, [H | T], Color).

% sublist(SubList-, List) - true is exists SubList which is a sublist of List where the elements order is kept
% copied from https://stackoverflow.com/questions/7051400/prolog-first-list-is-sublist-of-second-list (ДМИТРИЙ МАЛИКОВ's answer)
sublist([], _).
sublist([X|XS], [X|XSS]) :- sublist(XS, XSS).
sublist([X|XS], [_|XSS]) :- sublist([X|XS], XSS).

% find_all_bad_cliques(S+, N+, Solution+, Clique-, Color+) -    true if exists a Clique of size greate of equal to S, 
%                                                               with edeg color of Color, N is the count of vertexes
%                                                               and Solution is the adjacency matrix
find_all_bad_cliques(S, N, Solution, Clique, Color) :-
    N > S,
    N1 is N - 1,
    find_all_bad_cliques(S, N1, Solution, Clique, Color).


find_all_bad_cliques(S, N, Solution, Clique, Color) :-
    N >= S,
    cliqueN(Clique, N, Solution, Color).

        

verify_ramsey(r(S, _, N), Solution, CounterExample) :-
    find_all_bad_cliques(S, N, Solution, CounterExample, 0).

verify_ramsey(r(_, T, N), Solution, CounterExample) :-
    find_all_bad_cliques(T, N, Solution, CounterExample, 1).

verify_ramsey(r(S, T, N), Solution, ramsey) :-
    \+ find_all_bad_cliques(S, N, Solution, _, 0),
    \+ find_all_bad_cliques(T, N, Solution, _, 1).


%-----------------------------------------------------%
% Task 4 - A Ramsey (Brute Force) Solver

% find ramsey(r(S, T, N), Solution)

% make_row_first_element(N+, Matrix-) - generates a random top half of adjacency matrix
make_row_first_element(N, [[0 | Rest] | AccTail]) :-
    N > 1,
    N1 is N - 1,
    make_row_tail(N1, Rest),
    make_row_first_element(N1, AccTail).

make_row_first_element(1, [[0]]).

% make_row_tail(N+, Row-) - generates the tail of the row in the adjacency matrix
make_row_tail(N, [0 | Rest]) :-
    N >= 1,
    N1 is N - 1,
    make_row_tail(N1, Rest).

make_row_tail(N, [1 | Rest]) :-
    N >= 1,
    N1 is N - 1,
    make_row_tail(N1, Rest).

make_row_tail(0, []).

% complete_matrix(N, HalfMatrix, ResultMatrix) completes the top half of the adjacency matrix with values of "_"
complete_matrix(N, [HalfHead | HalfTail], [Head | Tail]) :-
    length(HalfHead, Len),
    Len < N,
    complete_matrix(N, [[_ | HalfHead] | HalfTail], [Head | Tail]).

complete_matrix(N, [Head | HalfTail], [Head | Tail]) :-
    length(Head, N),
    complete_matrix(N, HalfTail, Tail).

complete_matrix(_, [], []).


find_ramsey(r(S, T, N), Solution) :-
    make_row_first_element(N, HalfMatrix),
    complete_matrix(N, HalfMatrix, Solution),
    transpose(Solution, Solution),                  % a valid adjacency is symetric which means that it equal to its transpos 
                                                    % -> will fill the "_" values applied from complete_matrix
    verify_ramsey(r(S, T, N), Solution, ramsey),
    print_mat(Solution).

print_mat([H | T]) :-
    writeln(H),
    print_mat(T).

print_mat([]).



%-----------------------------------------------------%

%choose_n_from_k
create_list_size_n(0, Acc, List) :-
    Acc = List.

create_list_size_n(N, Acc, List) :-
    N1 is N - 1,
    create_list_size_n(N1, [N|Acc], List).

choose_k_from_n(K, N, [First|Rest]) :-
    first(K, N, First),
    last(K, N, Last),
    choose_k_from_n_aux(N, First, Last, Rest).
choose_k_from_n_aux(_, Last, Last, []).
choose_k_from_n_aux(N, Prev, Last, [Next|Rest]) :-
    increment(N, Prev, [], Next),
    choose_k_from_n_aux(N, Next, Last, Rest).

first(K, N, First) :-
    integer(K),
    integer(N),
    K =< N,
    create_list_size_n(N, [], ListOfN),
    !,
    first(K, ListOfN, [], First).
first(K, [Element|RestListOfN], Acc, First) :-
    K > 0,
    K1 is K - 1,
    first(K1, RestListOfN, [Element|Acc], First).
first(0, _, Acc, First) :-
    Acc = First.

last(K, N, Last) :-
    integer(K),
    integer(N),
    K =< N,
    create_list_size_n(N, [], ListOfN),
    NumberOfElementsToCut is N - K,
    cut_first_elements(NumberOfElementsToCut, ListOfN, List),
    !,
    first(K, List, [], Last).
    
cut_first_elements(NumElementsToCut, [_|List], CuttedList) :-
    NumElementsToCut > 0,
    NumElementsToCut1 is NumElementsToCut - 1,
    cut_first_elements(NumElementsToCut1, List, CuttedList).

cut_first_elements(0, List, CuttedList) :-
    CuttedList = List.

increment(N, [Element|RestElements], Acc, Next) :-      %Get to a digit that is less than N.. Add 1 to it and return result
    Element < N,
    Element1 is Element + 1,
    append(Acc, [Element1], List),                      %Add all of the accumulated digits from when Element = N to the digit we just increased by 1
    append(List, RestElements, Next).                   %Add all of the untouched digits (RestElements).

increment(N, [Element|RestElements], Acc, Next) :-
    Element = N,
    Element1 is Element - 1,
    N1 is N - 1,
    increment(N1, RestElements, [Element1|Acc], Next).  %Accumulate all of the digits that are = N.