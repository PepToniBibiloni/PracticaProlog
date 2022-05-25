cls:-write('\e[2J'), gotoXY(0,0).
gotoXY(X,Y):-write('\e['),write(X),write(";"),write(Y),write("H").

codi(negre,"\e[1;90m").
codi(vermell,"\e[1;91m").
codi(verd,"\e[1;92m").
codi(groc,"\e[1;93m").
codi(blau,"\e[1;94m").
codi(lila,"\e[1;95m").
codi(cel,"\e[1;96m").
codi(gris,"\e[1;97m").
codi(reset,"\e[m").

escriu(X,Color):-codi(Color,C),write(C),write(X),codi(reset,R),write(R).

diccionari(P):-paraula(M,S,P,nom,T,M,_,_,_,N,G,_,_,_,_),!,write(M),
               write(" "),write(S),
               write(" "),write(nom),
               write(" "),write(T),
               write(" "),write(N),
               write(" "),write(G),
               nl.

diccionari(P):-paraula(M,S,P,verb,_,_,Alt,Mode,Per,N,_,_,_,_,_),!,write(M),
               write(" "),write(verb),
               write(" "),write(S),
               write(" "),write(Per),write(" persona"),
               write(" "),write(N),
               write(" "),write(Mode),
               write(" "),write(Alt),
               nl.

diccionari(P):-paraula(M,S,P,F,T,_,_,_,_,N,G,_,_,_,_),write(M),
               write(" "),write(S),
               write(" "),write(F),
               write(" "),write(T),
               write(" "),write(N),
               write(" "),write(G),
               nl.

paraula([d, e, m, o, c, r, a, c, i, a]).
paraula([e, n, c, o, n, t, r, a, r, s, e]).
paraula([e, m, b, o, s, c, a, r]).
paraula([a, b, o, r, d, a, j, e]).
paraula([c, o, n, v, e, x, o]).
paraula([e, v, a, d, i, r, s, e]).
paraula([e, l, e, v, a, r, s, e]).
paraula([e, s, c, u, e, l, a]).
paraula([c, u, e, r, p, o]).
paraula([j, u, g, a, r]).
paraula([j, u, i, c, i, o]).
paraula([e, r, r, o, r]).
paraula([v, i, c, i, o]).
paraula([r, e, a]).

repetides([]) :- fail.
repetides([X|Xs]) :-
    member(X,Xs),!;
    repetides(Xs).

mostra([],_,_,_).
mostra([X|Y],F,C,O) :-
    O = vertical,gotoXY(F,C),escriu(X,vermell),F1 is F+1,gotoXY(F1,C),escriu(' ',negre),F2 is F1+1,mostra(Y,F2,C,O),nl,!;
    O = horitzontal,gotoXY(F,C),escriu(X,blau),C1 is C+1,gotoXY(F,C1),escriu('   ',negre),C2 is C1+3,mostra(Y,F,C2,O),nl.


.creuats :- 