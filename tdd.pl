/*
Part 1 - Nanograms

Task 1 - is_nanogram

Puzzle = nonogram(4, 4, [[3],[1,1],[1,1],[2]], [[4],[1,1],[2],[1]]),
Solution = [[1, 1, 1, 1], [1, 0, 0, 1],[1, 1, 0, 0],[0, 0, 1, 0]],
is_nonogram(nonogram(4, 4, [[3],[1,1],[1,1],[2]], [[4],[1,1],[2],[1]]), [[1, 1, 1, 1], [1, 0, 0, 1],[1, 1, 0, 0],[0, 0, 1, 0]]).
->true

Puzzle = nonogram(4, 4,[[3],[1,1],[1,1],[2]],[[4],[1,1],[2],[1]]),
Solution = [[1, 1, 1, 1],[1, 0, 0, 1],[1, 1, 0, 0],[1, 0, 1, 0]],
is_nonogram(Puzzle, Solution).
->fail

----------------------------------

Task 2 - nanogram_solve

 nonogram_solve(nonogram(4,4,[[3],[1,1],[1,1],[2]],[[4],[1,1],[2],[1]]),BoardByRows).
-> BoardByRows = [[1, 1, 1, 1],[1, 0, 0, 1],[1, 1, 0, 0],[0, 0, 1, 0]] ; false.

nonogram_solve(nonogram(8,7,[[2],[1,1],[2],[2,4],[1,1,2],[1,1,1,1],[2,2]],[[2],[1,1],[1,1],[2],[2,1],[1,2,2],[4,1],[3]]), BoardByRows).
-> BoardByRows = [[0, 0, 0, 1, 1, 0, 0],
[0, 0, 0, 1, 0, 1, 0],
[0, 0, 0, 0, 1, 0, 1],
[0, 0, 0, 0, 0, 1, 1],
[1, 1, 0, 1, 0, 0, 0],
[1, 0, 1, 1, 0, 1, 1],
[0, 1, 1, 1, 1, 0, 1],
[0, 0, 0, 1, 1, 1, 0]] ;
BoardByRows = [[0, 0, 0, 0, 1, 1, 0],
[0, 0, 0, 1, 0, 0, 1],
[0, 0, 0, 1, 0, 0, 1],
[0, 0, 0, 0, 1, 1, 0],
[1, 1, 0, 1, 0, 0, 0],
[1, 0, 1, 1, 0, 1, 1],
[0, 1, 1, 1, 1, 0, 1],
[0, 0, 0, 1, 1, 1, 0]] ;
false.

*/