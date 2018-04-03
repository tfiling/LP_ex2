% LP 182 - Assigment 2
% Nonogram puzzles

% nonogramPuzzle(+Difficulty,+InstanceID,-ProblemDescription)

nonogramPuzzle(easy,1,nonogram(Rows,Cols,ColsBlocks,RowsBlocks)):-
    Rows=4,
    Cols=4,
    ColsBlocks=[[3],[1,1],[1,1],[2]],
    RowsBlocks=[[4],[1,1],[2],[1]].

nonogramPuzzle(easy,2,nonogram(Rows,Cols,ColsBlocks,RowsBlocks)):-
    Rows=5,
    Cols=5,
    ColsBlocks=[[3],[4],[5],[4],[3]],
    RowsBlocks=[[1],[3],[5],[5],[5]].

nonogramPuzzle(easy,3,nonogram(Rows,Cols,ColsBlocks,RowsBlocks)):-
    Rows=5,
    Cols=5,
    ColsBlocks=[[1,2],[1],[],[1],[5]],
    RowsBlocks=[[1],[1,2],[1,1],[1,1],[1,1]].

nonogramPuzzle(easy,4,nonogram(Rows,Cols,ColsBlocks,RowsBlocks)):- % 2 solutions
    Rows=8,
    Cols=7,
    ColsBlocks=[[2],[1,1],[2],[2,4],[1,1,2],[1,1,1,1],[2,2]],
    RowsBlocks=[[2],[1,1],[1,1],[2],[2,1],[1,2,2],[4,1],[3]].

nonogramPuzzle(easy,5,nonogram(Rows,Cols,ColsBlocks,RowsBlocks)):-
    Rows = 8,
    Cols = 7,
    ColsBlocks = [[5], [1, 2], [1, 2, 1], [1, 1, 1], [1, 2, 1], [1, 2], [5]],
    RowsBlocks = [[5], [1, 1], [1, 1, 1, 1], [1, 1, 1, 1], [1, 1], [2, 1, 2], [1, 1], [3]].
    
nonogramPuzzle(medium,1,nonogram(Rows,Cols,ColsBlocks,RowsBlocks)):-
    Rows=10,
    Cols=10,
    ColsBlocks=[[1],[2,2],[2,2,3],[7,2],[2,3],[2,3],[2,2,3],[7,2],[2,2,3],[2]],
    RowsBlocks=[[1,1],[3,3],[3,3],[1,1],[3,4],[3,4],[1,1],[3,3,2],[9],[7]].

nonogramPuzzle(medium,2,nonogram(Rows,Cols,ColsBlocks,RowsBlocks)):-
    Rows = 9,
    Cols = 9,
    ColsBlocks = [[], [3, 1, 1], [1, 3], [3, 1, 1], [], [1, 1, 3], [3, 1], [1, 1, 3], []],
    RowsBlocks = [[], [1, 1, 3], [3, 1], [1, 1, 3], [], [3, 1, 1], [1, 3], [3, 1, 1], []].

nonogramPuzzle(medium,3,nonogram(Rows,Cols,ColsBlocks,RowsBlocks)):-
    Rows = 10,
    Cols = 10,
    ColsBlocks = [[1, 3, 3], [2, 3, 1], [1, 1, 1], [6], [5, 3], [2, 1, 3], [4, 3], [5, 3], [4, 4], [3, 4]],
    RowsBlocks = [[3, 6], [1, 6], [1, 1, 4], [5, 3], [2, 2, 1], [3, 2, 2], [2, 4], [1, 7], [1, 3, 3], [2, 1]].

nonogramPuzzle(medium,4,nonogram(Rows,Cols,ColsBlocks,RowsBlocks)):-
    Rows = 10,
    Cols = 10,
    ColsBlocks = [[4], [4, 1], [2, 2], [1, 3], [3, 1], [1, 2], [1, 3], [2, 4], [2, 1], [2, 1]],
    RowsBlocks = [[4], [4, 3], [2, 1], [2, 1], [2, 3], [1, 1, 2], [4, 2], [1, 2], [3, 1], [1]].

nonogramPuzzle(medium,5,nonogram(Rows,Cols,ColsBlocks,RowsBlocks)):-
    Rows = 10,
    Cols = 10,
    ColsBlocks = [[5], [1, 1], [1, 1], [1, 1], [1, 4, 1], [1, 1, 1, 1], [1, 1, 1, 1], [1, 4, 1], [1, 1], [5]],
    RowsBlocks = [[], [4], [3, 1], [1, 4, 1], [1, 1, 1, 1], [1, 1, 1, 1], [1, 4, 1], [1, 1], [8], []].

nonogramPuzzle(hard,1,nonogram(Rows,Cols,ColsBlocks,RowsBlocks)):-
    Rows=10,
    Cols=10,
    ColsBlocks=[[3],[2,1],[2,2],[2,1],[1,2,1],[1,1],[1,4,1],[1,1,2],[3,1],[4]],
    RowsBlocks=[[3],[2,1],[1,1],[1,4],[1,1,1,1],[2,1,1,1],[2,1,1],[1,2],[2,3],[3]].
    
nonogramPuzzle(hard,2,nonogram(Rows,Cols,ColsBlocks,RowsBlocks)):- % 10 solutions
    Rows = 10,
    Cols = 10,
    ColsBlocks = [[3], [3, 1], [2, 1], [1, 1], [1, 3, 2, 1], [1, 3, 1], [1, 2, 1], [2, 1], [3, 1], [3]],
    RowsBlocks = [[4], [1, 1], [1, 1, 1], [1, 1, 1], [1, 1, 1], [1, 1, 1], [1, 3, 1], [1, 3, 1], [1, 1], [8]].
    
nonogramPuzzle(hard,3,nonogram(Rows,Cols,ColsBlocks,RowsBlocks)):-
    Rows = 10,
    Cols = 10,
    ColsBlocks = [[3], [1], [1, 3], [1, 3], [2, 2], [2, 1, 1], [2, 1, 2, 1], [1, 1, 1], [1, 1, 1], [4, 2]],
    RowsBlocks = [[2], [1, 4], [1, 1, 1], [1, 1], [1, 3, 1, 1], [1, 1, 1], [2, 3], [1, 2], [2, 1], [6]].
    
nonogramPuzzle(hard,4,nonogram(Rows,Cols,ColsBlocks,RowsBlocks)):- % 5 solutions
    Rows = 10,
    Cols = 10,
    ColsBlocks = [[3], [2, 1], [1, 1, 1], [1, 1, 1, 1], [1, 6], [1, 1, 1, 1], [1, 1, 1], [2, 1], [3], []],
    RowsBlocks = [[1], [1, 1], [1, 1], [1, 1, 1, 1], [2, 1, 2], [1, 1, 1, 1, 1], [1, 3, 1], [1, 1, 1], [5], [1]].
    
nonogramPuzzle(hard,5,nonogram(Rows,Cols,ColsBlocks,RowsBlocks)):-
    Rows = 10,
    Cols = 10,
    ColsBlocks = [[2], [1, 1], [1, 2, 1], [1, 1, 1, 1], [1, 1, 2, 1, 1], [1, 1, 2, 1, 1], [1, 1, 1, 1], [1, 2, 1], [1, 1], [2]],
    RowsBlocks = [[2], [1, 1], [1, 2, 1], [1, 1, 1, 1], [1, 1, 2, 1, 1], [1, 1, 2, 1, 1], [1, 1, 1, 1], [1, 2, 1], [1, 1], [2]].
