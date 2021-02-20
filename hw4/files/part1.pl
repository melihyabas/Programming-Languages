% knowledge base

flight(istanbul,izmir).
flight(istanbul,antalya).
flight(istanbul,gaziantep).
flight(istanbul,ankara).
flight(istanbul,van).
flight(istanbul,rize).

flight(edirne,edremit).

flight(edremit,edirne).
flight(edremit,erzincan).

flight(erzincan,edremit).

flight(burdur,ısparta).

flight(ısparta,burdur).
flight(ısparta,izmir).

flight(izmir,ısparta).
flight(izmir,istanbul).

flight(antalya,istanbul).
flight(antalya,konya).
flight(antalya,gaziantep).

flight(gaziantep,istanbul).
flight(gaziantep,antalya).

flight(konya,antalya).
flight(konya,ankara).

flight(ankara,konya).
flight(ankara,istanbul).
flight(ankara,van).

flight(van,ankara).
flight(van,istanbul).
flight(van,rize).

flight(rize,van).
flight(rize,istanbul).


%rules..
route(X, Y) :- allRoutes(X, Y, []).

allRoutes(X, Y, Visited) :- flight(X, Z), not(member(Z, Visited))
						, (Y = Z; allRoutes(Z, Y, [X | Visited])).