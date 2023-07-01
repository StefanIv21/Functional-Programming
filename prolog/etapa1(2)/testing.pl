
%% Decomentați linia de mai jos pentru testare mai detaliată.
%% ATENȚIE: pe vmchecker linia este comentată.



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
       nSO('I^(between(1, 16, I), tile(I, T))', 'T', 16), % a
       ech('between(1, 12, I), tile(I, T), at(T, n, X)', ['X == c']),
       ech('between(13, 16, I), tile(I, T), at(T, n, X)', ['X \\== c']),
       ech('member(I, [1,2,3,4,8]), tile(I, T), at(T, e, X)', ['X == c']),
       ech('between(5, 16, I), I \\= 8, tile(I, T), at(T, e, X)', ['X \\== c']), % e
       ech('between(8, 16, I), \\+ member(I, [11,14]), tile(I, T), at(T, s, X)', ['X == d']),
       ech('between(1, 15, I), tile(I, T), at(T, n, X)', ['X \\== d']),
       ech('member(I, [5,7,9,13]), tile(I, T), at(T, e, X)', ['X == p']),
       ech('tile(16, T), at(T, D, X)', ['X == d']), % i
       nsl('(tile(16, T), at(T, _, X))', 'X', 4)
    ]).

tt(atL, [
         exp('tile(1, T)', [cond('atL(T, [w,n,e], c)'), cond('atL(T, [w,n,e], c)')])
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
    0.25, exp('tile(14, T), ccw(T, 0, T1)', [cond('atL(T1, [n,s], p)'), cond('atL(T1, [e,w], d)')]) % a
  , 0.25, exp('tile(14, T), ccw(T, 1, T1)', [cond('atL(T1, [n,s], d)'), cond('atL(T1, [e,w], p)')])
  , 0.25, exp('tile(14, T), ccw(T, 2, T1)', [cond('atL(T1, [n,s], p)'), cond('atL(T1, [e,w], d)')])
  , 0.25, exp('tile(14, T), ccw(T, 3, T1)', [cond('atL(T1, [n,s], d)'), cond('atL(T1, [e,w], p)')])
       , exp('tile(1, T),  ccw(T, 1, T1)', [cond('atL(T1, [n,s,w], c)'), cond('atL(T1, [e], p)')]) % e
       , exp('tile(1, T),  ccw(T, 2, T1)', [cond('atL(T1, [s,e,w], c)'), cond('atL(T1, [n], p)')])
       , exp('tile(2, T),  ccw(T, 3, T1)', [cond('atL(T1, [n,s,e], c)'), cond('atL(T1, [w], d)')])
       , exp('tile(2, T),  ccw(T, 2, T1)', [cond('atL(T1, [s,e,w], c)'), cond('atL(T1, [n], d)')]) % h
       , exp('tile(5, T),  ccw(T, 2, T1)', [cond('atL(T1, [s,n], c)'), cond('atL(T1, [w,e], p)')])
       , exp('tile(5, T),  ccw(T, 3, T1)', [cond('atL(T1, [w,e], c)'), cond('atL(T1, [s,n], p)')])
       , ech('between(1, 12, I), tile(I,T), ccw(T, 0, T1)', ['at(T1, n, c)']) % k
       , ech('between(1, 12, I), tile(I,T), ccw(T, 1, T1)', ['at(T1, w, c)'])
       , ech('between(1, 12, I), tile(I,T), ccw(T, 2, T1)', ['at(T1, s, c)'])
       , ech('between(1, 12, I), tile(I,T), ccw(T, 3, T1)', ['at(T1, e, c)'])
       , ech('between(8, 16, I), \\+ member(I, [11,14]), tile(I,T), ccw(T, 0, T1)', ['at(T1, s, d)']) % o
       , ech('between(8, 16, I), \\+ member(I, [11,14]), tile(I,T), ccw(T, 1, T1)', ['at(T1, e, d)'])
       , ech('between(8, 16, I), \\+ member(I, [11,14]), tile(I,T), ccw(T, 2, T1)', ['at(T1, n, d)'])
       , ech('between(8, 16, I), \\+ member(I, [11,14]), tile(I,T), ccw(T, 3, T1)', ['at(T1, w, d)'])
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
       , chk((tile(1, Tb1), tile(2, Tb2), match(Tb1, Tb2, w)))
       , chk((tile(1, Tc1), tile(2, Tc2), ccw(Tc2, 2, Tc2R), match(Tc1, Tc2R, n)))
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
       findRotation(TX, L, R)', [cond('member(R, [0,3])')])
          , sSO('(tile(2,TX), neighbor(5,0,n,N), neighbor(4,2,e,E), L=[N,E],
          findRotation(TX, L, R))', 'R', [0,3])
          , exp('tile(12,TX), neighbor(9,0,e,E), neighbor(10,0,w,W), neighbor(9,1,n,N), L=[W,E,N],
          findRotation(TX, L, R)', ['R', 2])
          , exp('tile(8,TX), neighbor(9,2,n,N), neighbor(13,3,s,S), L=[S,N],
          findRotation(TX, L, R)', [cond('member(R, [0,3])')])
          , sSO('(tile(11,TX), neighbor(14,1,n,N), neighbor(2,2,s,S), L=[S,N],
          findRotation(TX, L, R))', 'R', [1,3])
   ]).
%  tile(2, T2), tile(5, T5), tile(4, T4), ccw(T4, 2, T4R),
% printTiles([none, T5, none, none, xtile, T4R, none, none, none], 3),
% findRotation(T2, [(T5, n), (T4R, e)], X).
