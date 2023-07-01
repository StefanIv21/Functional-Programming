
%% Decomentați linia de mai jos pentru testare mai detaliată.
%% ATENȚIE: pe vmchecker linia este comentată.
%detailed_mode_disabled :- !, fail.


% quick ref:
% chk: verifică că este adevărat. ("check")
% uck: verifică că este fals. ("uncheck")
% ech: verifică că fiecare soluție pentru primul argument îndelinește
% condițiile din listă. ("each")
% nsl: verifică numărul de soluții pentru variabila din al doilea
% argument și interogarea din primul argument. ("n solutions")
% nSO: la fel ca mai sus, dar ignoră duplicatele (folosește setof)
% exp: verifică că pentru interogarea din primul argument, toate
% condițiile din listă sunt respectate.
% sSO: verifică că pentru interogarea din primul argument, soluțiile
% pentru variabila din al doilea argument sunt cele din lista dată.
% Soluțiile duplicate sunt ignorate. ("solutions set of")


tt(at, [
       % TODO test ca soluțiile să fie diferite
       nSO('I^(between(1, 16, I), tile(I, T), \\+ var(T))', 'T', 16), % a
       ech('between(1, 12, I), tile(I, T), at(T, n, X)', ['X == c']),
       ech('between(13, 16, I), tile(I, T), at(T, n, X)', ['(X == d; X == p)']),
       ech('member(I, [1,2,3,4,8]), tile(I, T), at(T, e, X)', ['X == c']),
       ech('between(5, 16, I), I \\= 8, tile(I, T), at(T, e, X)', ['(X == d; X == p)']), % e
       ech('between(8, 16, I), \\+ member(I, [11,14]), tile(I, T), at(T, s, X)', ['X == d']),
       ech('between(1, 15, I), tile(I, T), at(T, n, X)', ['(X == c; X == p)']),
       ech('member(I, [5,7,9,13]), tile(I, T), at(T, e, X)', ['X == p']),
       ech('tile(16, T), at(T, D, X)', ['X == d']), % i
       nsl('(tile(16, T), at(T, _, X))', 'X', 4)
    ]).

tt(atL, [
         exp('tile(1, T)', [cond('(at(T, w, X), X == c)'), cond('(at(T, s, X), X == p)'),
                            cond('atL(T, [w,n,e], c)'), cond('atL(T, [s], p)')])
    , 2, exp('tile(4, T)', [cond('atL(T, [n,e], c)'), cond('atL(T, [w,s], p)'),
                            cond('\\+ atL(T, [n,s], p)'), cond('\\+ atL(T, [n,s], d)')])
       , exp('tile(11,T)', [cond('atL(T, [e,w], d)'), cond('\\+ atL(T, [n,s], d)')])
    , 2, ech('(member(I, [5,6]), tile(I, T))', [
                   'atL(T, [n, s], c)', 'atL(T, [s], c)', 'atL(T, [n], c)'
                 , 'atL(T, [e, w], p)', 'atL(T, [e], p)', 'atL(T, [w], p)'
                 , '\\+ atL(T, [n, s, w], c)', '\\+ atL(T, [w, e, n, s], c)'
                 , '\\+ atL(T, [e, w], c)', '\\+ atL(T, [w, e, n, s], p)'
             ])
   ]).

tt(c2, [
         chk((tile(4, A), hasTwoCitadels(A)))
       , chk((tile(5, B), hasTwoCitadels(B)))
       , uck((tile(1, C), hasTwoCitadels(C)))
       , sSO('T^(tile(I, T), hasTwoCitadels(T))', 'I', [4,5])
       , ech('(between(1, 12, I), \\+ member(I, [4, 5]), tile(I, T))', ['\\+ hasTwoCitadels(T)'])
   ]).

tt(ccw, [
    0.25, exp('tile(14, T), ccw(T, 0, T1)', [cond('at(T1, n, X), X == p'), cond('at(T1, e, Y), Y == d'),
                  cond('atL(T1, [n,s], p)'), cond('atL(T1, [e,w], d)')]) % a
  , 0.25, exp('tile(14, T), ccw(T, 1, T1)', [cond('at(T1, n, X), X == d'), cond('at(T1, e, Y), Y == p'),
                  cond('atL(T1, [n,s], d)'), cond('atL(T1, [e,w], p)')])
  , 0.25, exp('tile(14, T), ccw(T, 2, T1)', [cond('at(T1, n, X), X == p'), cond('at(T1, e, Y), Y == d'),
                  cond('atL(T1, [n,s], p)'), cond('atL(T1, [e,w], d)')])
  , 0.25, exp('tile(14, T), ccw(T, 3, T1)', [cond('at(T1, n, X), X == d'), cond('at(T1, e, Y), Y == p'),
                  cond('atL(T1, [n,s], d)'), cond('atL(T1, [e,w], p)')])
       , exp('tile(1, T),  ccw(T, 1, T1)', [cond('at(T1, n, X), X == c'), cond('at(T1, e, X), X == p'),
                 cond('atL(T1, [n,s,w], c)'), cond('at(T1, e, p)')]) % e
       , exp('tile(1, T),  ccw(T, 2, T1)', [cond('at(T1, s, X), X == c'), cond('at(T1, n, X), X == p'),
                 cond('atL(T1, [s,e,w], c)'), cond('at(T1, n, p)')])
       , exp('tile(2, T),  ccw(T, 3, T1)', [cond('at(T1, n, X), X == c'), cond('at(T1, w, X), X == d'),
                 cond('atL(T1, [n,s,e], c)'), cond('at(T1, w, d)')])
       , exp('tile(2, T),  ccw(T, 2, T1)', [cond('at(T1, w, X), X == c'), cond('at(T1, n, X), X == d'),
                 cond('atL(T1, [s,e,w], c)'), cond('at(T1, n, d)')]) % h
       , exp('tile(5, T),  ccw(T, 2, T1)', [cond('at(T1, n, X), X == c'), cond('at(T1, e, X), X == p'),
                 cond('atL(T1, [s,n], c)'), cond('atL(T1, [w,e], p)')])
       , exp('tile(5, T),  ccw(T, 3, T1)', [cond('at(T1, e, X), X == c'), cond('at(T1, s, X), X == p'),
                 cond('atL(T1, [w,e], c)'), cond('atL(T1, [s,n], p)')])
       , ech('between(1, 12, I), tile(I,T), ccw(T, 0, T1)', ['(at(T1, n, X), X == c)']) % k
       , ech('between(1, 12, I), tile(I,T), ccw(T, 1, T1)', ['(at(T1, w, X), X == c)'])
       , ech('between(1, 12, I), tile(I,T), ccw(T, 2, T1)', ['(at(T1, s, X), X == c)'])
       , ech('between(1, 12, I), tile(I,T), ccw(T, 3, T1)', ['(at(T1, e, X), X == c)'])
       , ech('between(8, 16, I), \\+ member(I, [11,14]), tile(I,T), ccw(T, 0, T1)', ['(at(T1, s, X), X == d)']) % o
       , ech('between(8, 16, I), \\+ member(I, [11,14]), tile(I,T), ccw(T, 1, T1)', ['(at(T1, e, X), X == d)'])
       , ech('between(8, 16, I), \\+ member(I, [11,14]), tile(I,T), ccw(T, 2, T1)', ['(at(T1, n, X), X == d)'])
       , ech('between(8, 16, I), \\+ member(I, [11,14]), tile(I,T), ccw(T, 3, T1)', ['(at(T1, w, X), X == d)'])
   ]).

tt(rot, [
         exp('tile(4, T), rotations(T, L), length(L, N)', ['N', 4]) % a
       , exp('tile(5, T), rotations(T, L), length(L, N)', ['N', 2])
       , exp('tile(6, T), rotations(T, L), length(L, N)', ['N', 2])
       , exp('tile(11, T), rotations(T, L), length(L, N)', ['N', 4]) % d
       , exp('tile(14, T), rotations(T, L), length(L, N)', ['N', 2])
       , exp('tile(16, T), rotations(T, L), length(L, N)', ['N', 1])
       , nSO('R^T^L^(tile(9, T), rotations(T, L), member((R, TR), L), at(TR, e, d))', 'TR', 2) % g
       , nSO('R^T^L^(tile(10,T), rotations(T, L), member((R, TR), L), at(TR, e, d))', 'TR', 2)
       , nSO('R^T^L^(tile(1, T), rotations(T, L), member((R, TR), L), at(TR, n, p))', 'TR', 1)
       , nSO('R^T^L^(tile(12,T), rotations(T, L), member((R, TR), L), at(TR, n, d))', 'TR', 3)
   ]).

tt(match, [
         chk((tile(1, Ta1), tile(2, Ta2), match(Ta1, Ta2, e))) % a
   , -1, chk((tile(1, Ta1), tile(2, Ta2), match(Ta1, Ta2, s)))
       , chk((tile(1, Tb1), tile(2, Tb2), match(Tb1, Tb2, w)))
   , -1, chk((tile(1, Tb1), tile(2, Tb2), match(Tb1, Tb2, n)))
       , chk((tile(1, Tc1), tile(2, Tc2), ccw(Tc2, 2, Tc2R), match(Tc1, Tc2R, n)))
   , -1, chk((tile(1, Tc1), tile(2, Tc2), ccw(Tc2, 2, Tc2R), match(Tc1, Tc2R, s)))
       , uck((tile(1, Td1), tile(2, Td2), match(Td1, Td2, n))) % d
       , uck((tile(1, Te1), tile(2, Te2), match(Te1, Te2, s)))
       , sSO('T^TI^(tile(5, T), tile(I, TI), match(T, TI, s))', 'I', [1,2,3,4,5,6,7,8,9,10,11,12]) % f
       , sSO('T^TI^(tile(2, T), ccw(T, 2, TR), tile(I, TI), match(TR, TI, n))', 'I', [2,8,9,10,12,13,15,16])
       , sSO('T^TI^(tile(8, T), tile(I, TI), match(T, TI, n))', 'I', [5, 6])
       , sSO('T^TI^(tile(8, T), tile(I, TI), match(T, TI, w))', 'I', [10,11,12,14,15,16]) % i
       , sSO('T^TI^(tile(10,T), tile(I, TI), match(T, TI, e))', 'I', [8,9,11,12,13,14,15,16])
   ]).

tt(findrot, [
       exp('tile(2,TX), neighbor(5,0,n,N), neighbor(4,2,e,E), L=[N,E],
       findRotation(TX, L, R)', [val('R'), cond('member(R, [0,3])')])
          , sSO('(tile(2,TX), neighbor(5,0,n,N), neighbor(4,2,e,E), L=[N,E],
          findRotation(TX, L, R))', 'R', [0,3])
          , exp('tile(12,TX), neighbor(9,0,e,E), neighbor(10,0,w,W), neighbor(9,1,n,N), L=[W,E,N],
          findRotation(TX, L, R)', ['R', 2])
          , exp('tile(8,TX), neighbor(9,2,n,N), neighbor(13,3,s,S), L=[S,N],
          findRotation(TX, L, R)', [val('R'), cond('member(R, [0,3])')])
          , sSO('(tile(11,TX), neighbor(14,1,n,N), neighbor(2,2,s,S), L=[S,N],
          findRotation(TX, L, R))', 'R', [1,3])
   ]).

% aceste teste folosesc boardSetAll/2 și boardSetTest/2 definite mai jos
% și tablele definite folosind boardTiles/2, tot mai jos.
tt(setget, [
           % emptyBoard leagă ceva și boardSet produce un board diferit de empty
        0.6, exp('emptyBoard(B)', [val('B')])
       ,0.7, exp('emptyBoard(B0), tile(16, T), boardSet(B0, (1, 1), T, B)', [cond('B \\= B0')])
       ,0.7, exp('emptyBoard(B0), tile(16, T), boardSet(B0, (-2, -2), T, B)', [cond('B \\= B0')])
           % nu se admit suprapuneri pe aceeași poziție
       ,0.5, uck((emptyBoard(B0), tile(16, T), tile(1, T1),
                 boardSet(B0, (1, 1), T, B), boardSet(B, (1, 1), T1, _)))
       ,0.5, uck((boardSetAll([(0, 0, 6, 1), (1, 0, 2, 0), (2, 0, 7, 1)], Bd),
                 tile(10, T), boardSet(Bd, (1, 0), T, Bd)))
       , -1, uck((emptyBoard(Ba0), tile(16, Ta), boardSet(Ba0, (1, 1), Ta, _)))
           % boardSet merge pe un test mai mare
           , exp('boardSetTest(2, B)', [val('B')])

           , uck((emptyBoard(Bg), boardGet(Bg, (0, 0), _)))
           , exp('emptyBoard(B0), tile(16, T), boardSet(B0, (1, 1), T, B), boardGet(B, (1, 1), TCk)',
                 [cond('T == TCk')])
           , exp('boardSetTest(2, B), tile(10, T), boardGet(B, (1, 1), TCk)', [cond('T == TCk')])
           , exp('boardSetTest(2, B), tileRot(3, 2, T), boardGet(B, (4, 1), TCk)', [cond('T == TCk')])
           , exp('boardSetTest(1, B), tile(11, T), boardGet(B, (-1, -1), TCk)', [cond('T == TCk')])
       ,0.5, uck((boardSetTest(2, Bx), boardGet(Bx, (0, 1), _)))
       ,0.5, uck((boardSetTest(1, Bx), boardGet(Bx, (2, 1), _)))
       , -1, uck((boardSetTest(1, Bx), boardGet(Bx, (1, 1), _)))
   ]).

tt(limits, [
       uck((emptyBoard(B), boardGetLimits(B, _, _, _, _)))
 , -1, uck((boardSetTest(0, Bb), boardGetLimits(Bb, _, _, _, _)))
     , exp('emptyBoard(B), tile(14, T), boardSet(B, (0, 0), T, B1),
     boardSet(B1, (1, 0), T, B2), boardGetLimits(B2, Xm, Ym, XM, YM)',
           ['Xm', 0, 'Ym', 0, 'XM', 1, 'YM', 0])
     , exp('boardSetTest(0, B), boardGetLimits(B, Xm, Ym, XM, YM)',
           ['Xm', 0, 'Ym', 0, 'XM', 2, 'YM', 2])
     , exp('boardSetTest(1, B), boardGetLimits(B, Xm, Ym, XM, YM)',
           ['Xm', -1, 'Ym', -1, 'XM', 2, 'YM', 1])
     , exp('boardSetTest(2, B), boardGetLimits(B, Xm, Ym, XM, YM)',
           ['Xm', 0, 'Ym', -1, 'XM', 4, 'YM', 2])
   ]).

tt(canPlaceA, [
               nSO('B0^I^YI^(emptyBoard(B0), between(1, 16, I), tile(I, T), YI is I * 2,
               canPlaceTile(B0, (I, YI), T))', 'T', 16)
          , 2, exp('emptyBoard(B0), tile(8, T8), boardSet(B0, (0, 0), T8, B), tile(2, T2), tileRot(2, 2, T2R2)',
                   [cond('canPlaceTile(B, (1, 0), T2)'), cond('canPlaceTile(B, (0, -1), T2R2)'),
                    cond('canPlaceTile(B, (0, 1), T2R2)')])
          , 2, exp('emptyBoard(B0), tile(8, T8), boardSet(B0, (0, 0), T8, B), tile(2, T2), tileRot(2, 2, T2R2)',
                   [cond('\\+ canPlaceTile(B, (-1, 0), T2R2)'), cond('\\+ canPlaceTile(B, (0, -1), T2)'),
                    cond('\\+ canPlaceTile(B, (0, 0), T2)'), cond('\\+ canPlaceTile(B, (1, 1), T2R2)')])
   ]).

tt(canPlace1, [
       exp('boardSetTest(1, B), tile(11, T11), tile(14, T14), tileRot(10, 2, T10R2)',
           [ cond('canPlaceTile(B, (2, -1), T14)')
           , cond('canPlaceTile(B, (-2, -1), T11)')
           , cond('canPlaceTile(B, (1, 2), T10R2)')
           , cond('canPlaceTile(B, (-2, 0), T10R2)')
           , cond('canPlaceTile(B, (-1, 1), T14)')
           ])
     , exp('boardSetTest(1, B), tile(11, T11), tile(14, T14), tile(10, T10)',
           [ cond('\\+ canPlaceTile(B, (0, -1), T11)')
           , cond('\\+ canPlaceTile(B, (2, -1), T11)')
           , cond('\\+ canPlaceTile(B, (0, 1), T10)')
           , cond('\\+ canPlaceTile(B, (1, -2), T10)')
           , cond('\\+ canPlaceTile(B, (-2, 1), T10)')
           ])
   ]).

tt(canPlace2, [
       exp('boardSetTest(2, B), tile(11, T11), tileRot(14, 1, T14R), tile(10, T10)',
           [ cond('canPlaceTile(B, (2, 0), T10)')
           , cond('canPlaceTile(B, (0,-2), T10)')
           , cond('canPlaceTile(B, (0, 3), T10)')
           , cond('canPlaceTile(B, (2, -1), T10)')
           , cond('canPlaceTile(B, (2, 3), T10)')
           , cond('canPlaceTile(B, (0,-2), T11)')
           , cond('canPlaceTile(B, (0, 1), T14R)')
           , cond('canPlaceTile(B, (5, 0), T11)')
           ])
     , exp('boardSetTest(2, B), tile(11, T11), tileRot(14, 1, T14R), tile(10, T10)',
           [ cond('\\+ canPlaceTile(B, (0, 1), T10)')
           , cond('\\+ canPlaceTile(B, (4,-1), T10)')
           , cond('\\+ canPlaceTile(B, (3,-1), T14R)')
           , cond('\\+ canPlaceTile(B, (2, 0), T11)')
           , cond('\\+ canPlaceTile(B, (1,-2), T11)')
           ])
   ]).

tt(avPos, [
         4, uck((emptyBoard(B0), getAvailablePositions(B0, _)))
       ,-1, uck((boardSetTest(0, B), getAvailablePositions(B, _)))
        ,7, exp('boardSetTest(0, B), getAvailablePositions(B, Ps)',
                [ cond('length(Ps, N), N == 8')
                , cond('member((3, 1), Ps)')
                , cond('member((1, -1), Ps)')
                , cond('\\+ member((0, -1), Ps)')
                , cond('\\+ member((1, 1), Ps)')
           ])
        ,7, exp('boardSetTest(1, B), getAvailablePositions(B, Ps)',
                [ cond('length(Ps, N), N == 11')
                , cond('member((0, -1), Ps)')
                , cond('member((1, -2), Ps)')
                , cond('member((1, 2), Ps)')
                , cond('\\+ member((3, 1), Ps)')
                , cond('\\+ member((3, 1), Ps)')
                , cond('\\+ member((0, -2), Ps)')
           ])
        ,7, exp('boardSetTest(2, B), getAvailablePositions(B, Ps)',
                [ cond('length(Ps, N), N == 17')
                , cond('member((0, 1), Ps)')
                , cond('member((2, 0), Ps)')
                , cond('member((4, 2), Ps)')
                , cond('member((5, 0), Ps)')
                , cond('\\+ member((1, 1), Ps)')
                , cond('\\+ member((-1, 1), Ps)')
                , cond('\\+ member((5, 2), Ps)')
           ])
   ]).

tt(findPos, [
              exp('emptyBoard(B0), tile(5, T), findPositionForTile(B0, T, P, R)', ['P', (0, 0), 'R', 0])

            , sSO('(boardSetTest(0, B), tile(8, T), findPositionForTile(B, T, (X,Y), R))', '(X,Y):R',
                 [(3,1):0, (3,1):3])
            , sSO('(boardSetTest(0, B), tile(4, T), findPositionForTile(B, T, (X,Y), R))', '(X,Y):R',
                 [(0,2):0, (-1,1):1, (-1,1):2, (1,-1):2, (1,-1):3, (1,3):0, (1,3):1, (2,2):0])
            , sSO('(boardSetTest(0, B), tile(16, T), findPositionForTile(B, T, (X,Y), R))', '(X,Y):R',
                 [(3,1):0])
            , sSO('(boardSetTest(0, B), tile(14, T), findPositionForTile(B, T, (X,Y), R))', '(X,Y):R',
                 [(3,1):0, (2,0):0, (0,0):0, (-1,1):1, (1,-1):0, (1,3):0])

            , sSO('(boardSetTest(1, B), tile(8, T), findPositionForTile(B, T, (X,Y), R))', '(X,Y):R',
                 [(-2,-1):1, (-2,-1):2, (1,2):2, (1,2):3])
            , sSO('(boardSetTest(1, B), tile(6, T), findPositionForTile(B, T, (X,Y), R))', '(X,Y):R',
                 [(1,2):0, (-2,0):0, (-1,-2):1, (-1,1):1, (1,-2):1, (3,0):0])
            , sSO('(boardSetTest(1, B), tile(2, T), findPositionForTile(B, T, (X,Y), R))', '(X,Y):R',
                 [(-2,-1):1,(1,2):1,(1,2):2,(1,2):3])

            , sSO('(boardSetTest(1, B), tile(10, T), findPositionForTile(B, T, (X,Y), R))', '(X,Y):R',
                 [(-2,-1):0,(-2,-1):1,(-2,0):2,(-1,-2):3,(-1,1):1,(1,-2):3,(1,2):2,(2,-1):3,(3,0):0])
            , sSO('(boardSetTest(1, B), tile(12, T), findPositionForTile(B, T, (X,Y), R))', '(X,Y):R',
                 [(-2,-1):0,(-2,-1):1,(-2,-1):2,(1,2):2])
   ]).


% boardSetAll(Pieces, Board) - leagă Board la reprezentarea unei table
% pe care au fost puse piesele din lista Pieces, care contine tupluri
% (X, Y, IndexPiesa, Rotatie)
% Foloseste emptyBoard/1 si boardSet/5
boardSetAll(L, Board) :- emptyBoard(B0), boardSetAll(L, B0, Board).
boardSetAll([], Board, Board) :- !.
boardSetAll([(X, Y, Index, Rot) | R], Board, BoardOut) :-
    tileRot(Index, Rot, T),
%    format("Try placing ~w at ~w,~w~n", [T, X, Y]),
    boardSet(Board, (X, Y), T, Board1),
    boardSetAll(R, Board1, BoardOut).

% Teste de tipul IndexTest, ListaPiese, unde ListaPiese urmează
% specificatia de la boardSetAll/2
boardTiles(0, [ (1, 2, 3, 2), (1, 1, 2, 1), (0, 1, 7, 3), (2, 1, 14, 0),
                (1, 0, 11, 0)
           ]).
boardTiles(1, [
               (1, 1, 5, 0),
               (1, 0, 2, 0), (0, 0, 6, 1), (-1, 0, 3, 3), (2, 0, 7, 1),
               (1, -1, 13, 2), (-1, -1, 11, 0)
           ]).
boardTiles(2, [
               (0, 2, 12, 3), (1, 2, 1, 2), (2, 2, 8, 2), (3, 2, 11, 0),
                              (1, 1, 10, 0),(2, 1, 2, 3), (3, 1, 5, 1), (4, 1, 3, 2),
                              (1, 0, 14, 1),              (3, 0, 14, 0),(4, 0, 11,0),  (0, 0, 14, 1),
                              (1, -1, 13, 3),  (0, -1, 12, 2)
           ]).

% boardSetTest/2 leagă Board la tabla descrisă de testul Index (vezi
% boardTiles/2).
boardSetTest(Index, Board) :- boardTiles(Index, List), boardSetAll(List, Board).











