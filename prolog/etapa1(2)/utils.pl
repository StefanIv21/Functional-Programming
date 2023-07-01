


editall :- forall(member(File, [
                             ccs, utils, testing, inputs, 'checker.pl', points1
                         ]), edit(File)).


% . | . ++++. .++++ +++++
%   | + +++     +++ +++++
% --|++ ++  - -  ++ +++++
%   | + +  /    +++ +++++
% . | . . | . .++++ +++++
%


% directions/1 - leagă argumentul la lista de direcții.
directions([n,e,s,w]).

% opposite/2 - este adevărat pentru direcții opuse
opposite(D1, D2) :- member((D1, D2), [(n, s), (s, n), (e, w) , (w, e)]).

% neighbor/3 - construiește perechea de coordonate vecină cu X,Y, pe
% direcția Dir
neighbor((X, Y), Dir, (XN, YN)) :- delta(Dir, DX, DY), XN is X + DX, YN is Y + DY.
delta(Dir, XN, YN) :- member((Dir, XN, YN), [(n, 0, 1), (s, 0, -1), (e, 1, 0), (w, -1, 0)]).

% tileRot/3 - tile/2 + ccw/3
tileRot(Index, Rotation, Tile) :- tile(Index, T), ccw(T, Rotation, Tile).
% neighbor/4 - tile/2 + ccw/3 + create pair for neighbors argument in
% findRotation
neighbor(Index, Rotation, Dir, (Tile, Dir)) :- tileRot(Index, Rotation, Tile).

% printTile/1 - afișează piesa dată în argument
printTile(P) :- printableS(P, S), write(S).

% printTiles/2 - afișează piesele din lista Tiles, începând un rând nou
% după fiecare W piese
printTiles(Tiles, W) :- group(Tiles, W, W, Rows),
    findall(Lines,
            (
                member(R, Rows),
                findall(Print, (member(T, R), printable(T, Print)), Prints),
                findall(Line,
                        (   member(Y, [0,1,2,3,4]),
                            findall(P, (member(Print, Prints), nth0(Y, Print, P)), Parts),
                            reverse(Parts, RP), foldl(append, RP, [], Flat1),
                            reverse(Flat1, Flat), foldl(atom_concat, ['\n' | Flat], '', Line)
                        ),
                       Lines)
            ),
           AllLines),
    reverse(AllLines, LR), foldl(append, LR, [], AL),
    reverse(AL, ALR), foldl(atom_concat, ALR, '', S),
    write(S).
group([], _, _, [[]]) :- !.
group(Tiles, 0, W0, [[]|RR]) :- !, group(Tiles, W0, W0, RR).
group([E|Tiles], W, W0, [[E|R]|RR]) :- W > 0, W1 is W - 1,
    group(Tiles, W1, W0, [R|RR]).

printableS(P, S) :- printable(P, PP),
    findall(['\n'|RR], (member(Row, PP), reverse(Row, RR)), RPrint),
    foldl(append, RPrint, [], Flat), foldl(atom_concat, Flat, '', S).
printable(none, PP) :- !, findall([' ',' ',' ',' ',' '], member(_, [1,2,3,4,5]), PP).
printable(xtile, PP) :- !, findall([' ',' ',X,' ',' '], (member(R, [1,2,3,4,5]),
                                                        (   (R == 3) -> X = 'X'; X = ' ')), PP).
printable(P, PP) :-
    findall(RowP,
            (member(Row, [1,2,3,4,5]),
             member(Col, [1,2,3,4,5]),
%             format("print for ~w: ~w ~w ~n", [P, Row, Col]),
             printable(P, Row, Col, RowP)
%             , format("~t result: ~w~n", [RowP])
            ),
            PPAll), grp(PPAll, PP).
grp([], []).
grp([A,B,C,D,E|Rest], [[A,B,C,D,E]|Rest1]) :- grp(Rest, Rest1).
%printable(P, R, C, _) :- format("~t ~w : ~w ~w ~n", [P, R, C]), fail.
printable(P, 3, 3, '+') :- directions(Ds), atL(P, Ds, c), !.
printable(P, 3, 3, '+') :- \+ hasTwoCitadels(P), directions(Ds),
    findall(Dir, (member(Dir, Ds), at(P, Dir, c)), A), length(A, C), C > 1, !.
printable(P, 3, 3, 'O') :-  directions(Ds),
    findall(Dir, (member(Dir, Ds), at(P, Dir, d)), A), length(A, C), C > 2, !.
printable(P, 3, 3, '-') :- atL(P, [e,w], d), !.
printable(P, R, C, X) :- quadrant(R, C, Q, R1, C1), Q > 0, !,
    ccw(P, Q, QP), printable(QP, R1, C1, QX), rotate(Q, QX, X).
printable(P, 1, C, '+') :- at(P, n, c), between(2, 4, C), !.
printable(P, 2, 3, '+') :- at(P, n, c), !.
printable(P, R, C, '+') :- \+ hasTwoCitadels(P), atL(P, [n,w], c), R =< 2, C =< 2, !.
printable(P, 2, 1, '+') :- at(P, w, c), !.
printable(P, 1, 3, '|') :- at(P, n, d), !.
printable(P, _, 3, '|') :- atL(P, [n,s], d), !.
printable(P, _, 3, '|') :- at(P, n, d),
    findall(Dir, (member(Dir, [e,s,w]), at(P, Dir, d)), A), length(A, C), C > 1, !.
printable(P, 2, 2, '/') :- atL(P, [n, w], d),
    findall(Dir, (member(Dir, [n,e,s,w]), at(P, Dir, d)), A), length(A, C), C < 3, !.
printable(_, R, C, '.') :- P is R*C, member(P, [1, 5, 25]), !.
printable(_, _, _, ' ').
rotate(Q, '|', '-') :- member(Q, [1, 3]), !.
rotate(Q, '/', '\\') :- member(Q, [1, 3]), !.
rotate(_, X, X).
quadrant(R, C, 0, R, C) :- R < 3, C =< 3, !.
quadrant(R, C, 1, Rt, Ct) :- R =< 3, C > 3, !, Ct = R, Rt is 6 - C.
quadrant(R, C, 2, Rt, Ct) :- R > 3, C >= 3, !, Ct is 6 - C, Rt is 6 - R.
quadrant(R, C, 3, Rt, Ct) :- R >= 3, C < 3, !, Ct is 6 - R, Rt = C.
