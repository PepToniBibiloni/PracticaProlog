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

par("democracia").
par("encontrarse").
par("emboscar").
par("abordaje").
par("convexo").
par("evadirse").
par("elevarse").
par("escuela").
par("cuerpo").
par("jugar").
par("juicio").
par("error").
par("vicio").
par("rea").


paraula(X) :- 
    par(P),
    atom_chars(P,X);
    par(P),
    atom_chars(P,L),
    reverse(L,X).

repetides([]) :- fail.
repetides([X|Xs]) :-
    member(X,Xs),!;
    repetides(Xs).

mostra([],_,_,_).
mostra([X|Y],F,C,O) :-
    O = vertical,C1 is C*4,F1 is F*2,gotoXY(F1,C1),escriu(X,vermell),F2 is F1+1,gotoXY(F2,C1),escriu(' ',negre),F3 is F+1,mostra(Y,F3,C,O),nl,!;
    O = horitzontal,C1 is C*4,F1 is F*2,gotoXY(F,C1),escriu(X,blau),C2 is C1+1,gotoXY(F1,C2),escriu('   ',negre),C3 is C+1,mostra(Y,F,C3,O),nl.

conte(X,Y,N1,N2):-
    nth1(N1,X,L1),nth1(N2,Y,L2),
    L1 = L2,!.

creuats :- 
    paraula(X0),length(X0,N0),N0 is 7,
    paraula(X1),length(X1,N1),N1 is 11,conte(X0,X1,5,1),append([X0],[X1],LL0),
    paraula(X2),length(X2,N2),N2 is 5,conte(X1,X2,3,3),append(LL0,[X2],LL1),
    paraula(X3),length(X3,N3),N3 is 7,conte(X1,X3,11,5),append(LL1,[X3],LL2),not(repetides(LL2)),
    paraula(X4),length(X4,N4),N4 is 3,conte(X3,X4,7,3),append(LL2,[X4],LL3),
    paraula(X5),length(X5,N5),N5 is 8,conte(X3,X5,3,6),append(LL3,[X5],LL4),
    paraula(X6),length(X6,N6),N6 is 10,conte(X5,X6,8,6),append(LL4,[X6],LL5),
    paraula(X7),length(X7,N7),N7 is 8,conte(X5,X7,1,8),append(LL5,[X7],LL6),not(repetides(LL6)),
    paraula(X8),length(X8,N8),N8 is 6,conte(X7,X8,6,4),append(LL6,[X8],LL7),
    paraula(X9),length(X9,N9),N9 is 8,conte(X7,X9,4,5),append(LL7,[X9],LL8),not(repetides(LL8)),
    paraula(X10),length(X10,N10),N10 is 6,conte(X9,X10,3,6),append(LL8,[X10],LL9),not(repetides(LL9)),
    paraula(X11),length(X11,N11),N11 is 5,conte(X10,X11,1,1),append(LL9,[X11],LL10),not(repetides(LL10)),
    paraula(X12),length(X12,N12),N12 is 8,conte(X7,X12,1,1),append(LL10,[X12],LL11),not(repetides(LL11)),
    paraula(X13),length(X13,N13),N13 is 5,conte(X12,X13,6,2),append(LL11,[X13],LL12),not(repetides(LL12)),
    cls,mostra(X0,1,8,horitzontal),mostra(X1,1,12,vertical),mostra(X2,3,10,horitzontal).
    

