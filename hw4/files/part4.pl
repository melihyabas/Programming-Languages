element(E, [E|_]).
element(E, [_|S]):- element(E, S).

union(S1,S2,S3) :- unionX(S1,S2,X), equivalent(X,S3).
unionX([], S2, S2).
unionX([E|S1], S2, S3) :- element(E, S2), !, unionX(S1, S2, S3). 
unionX([E|S1], S2, [E|S3]) :- unionX(S1, S2, S3).

equivalent(S1, S2) :- equivalentX(S1,S2), equivalentX(S2,S1).
equivalentX([],_).
equivalentX([E|S1],S2):- element(E,S2), equivalentX(S1,S2).

intersect(S1,S2,S3) :- intersectX(S1,S2,X), equivalent(X,S3).
intersectX([], _, []).
intersectX([E|S1], S2, [E|S3]):- element(E, S2), !, intersectX(S1, S2, S3).
intersectX([_|S1], S2, S3):- intersectX(S1, S2, S3).
