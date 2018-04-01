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