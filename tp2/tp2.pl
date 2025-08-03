%%%%%%%%%%%%%%%%%%%%%%%%
%% Predicados básicos %%
%%%%%%%%%%%%%%%%%%%%%%%%

%% Ejercicio 1
%% proceso(+P)
proceso(computar).
proceso(escribir(_, _)).
proceso(leer(_)). 
proceso(secuencia(P, Q)) :- proceso(P), proceso(Q).
proceso(paralelo(P, Q)) :- proceso(P), proceso(Q).


%% Ejercicio 2
%% buffersUsados(+P,-BS)
buffersUsados(computar, []).
buffersUsados(escribir(B, _), [B]).
buffersUsados(leer(B), [B]).
buffersUsados(secuencia(P, Q), BS) :- buffersUsados(P, BP), buffersUsados(Q, BQ), union(BP, BQ, BS).
buffersUsados(paralelo(P, Q), BS) :- buffersUsados(P, BP), buffersUsados(Q, BQ), union(BP, BQ, BS). 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Organización de procesos %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Ejercicio 3
%% intercalar(+XS,+YS,?ZS)
intercalar([], YS, YS).
intercalar(XS, [], XS).
intercalar([X|XS], [Y|YS], [X|ZS]) :- intercalar(XS, [Y|YS], ZS).
intercalar([X|XS], [Y|YS], [Y|ZS]) :- intercalar([X|XS], YS, ZS).


%% Ejercicio 4
%% serializar(+P,?XS)
serializar(computar, [computar]).
serializar(escribir(B, X), [escribir(B, X)]).
serializar(leer(B), [leer(B)]).
serializar(secuencia(P, Q), XS) :- serializar(P, PS), serializar(Q, QS), append(PS, QS, XS).
serializar(paralelo(P, Q), XS) :- serializar(P, PS), serializar(Q, QS), intercalar(PS, QS, XS). 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Contenido de los buffers %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Ejercicio 5
%% contenidoBuffer(+B,+ProcesoOLista,?Contenidos)
contenidoBuffer(B, Proceso, Contenidos) :- proceso(Proceso), serializar(Proceso, Lista), contenidoBuffer(B, Lista, Contenidos).
contenidoBuffer(B, Lista, Contenidos) :- not(proceso(Lista)), reverse(Lista, ListaRev), esContenido(B, ListaRev, Contenidos).

%esContenido(+B, +L, ?C)
esContenido(_, [], []).
esContenido(B, [Accion|L], C) :- not(esAccionB(B, Accion)), esContenido(B, L, C).
esContenido(B, [leer(B)|L], C) :- esContenido(B, L, [_|C]).
esContenido(B, [escribir(B, X)|L], C) :- esContenido(B, L, SinUltimo), append(SinUltimo, [X], C).

%esAccionB(?B, ?X)
esAccionB(B, escribir(B, _)).
esAccionB(B, leer(B)).


%% Ejercicio 6
%% contenidoLeido(+ProcesoOLista,?Contenidos)
contenidoLeido(Proceso, Contenidos) :- proceso(Proceso), serializar(Proceso, Lista), contenidoLeido(Lista, Contenidos).
contenidoLeido([], []).
contenidoLeido([computar|Lista], C) :- contenidoLeido(Lista, C).
contenidoLeido([escribir(B, _)|Lista], C) :- not(member(leer(B), Lista)), contenidoLeido(Lista, C).
contenidoLeido([escribir(B, X)|Lista], [X|SinPrimero]) :- sinLectura(B, Lista, SinLeer), contenidoLeido(SinLeer, SinPrimero).

%sinLectura(+B, +L, ?YS)
sinLectura(B, [leer(B)|SinLeer], SinLeer).
sinLectura(B, [X|L], [X|SinLeer]) :- X \= leer(B), sinLectura(B, L, SinLeer).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Contenido de los buffers %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Ejercicio 7
%% esSeguro(+P)
esSeguro(P) :- sinCompartidos(P), buffersUsados(P, PS), forall((member(B, PS), serializar(P, PX)), contenidoBuffer(B, PX, _)).

%sinCompartidos(+P)
sinCompartidos(computar).
sinCompartidos(escribir(_, _)).
sinCompartidos(leer(_)).
sinCompartidos(secuencia(P, Q)) :- sinCompartidos(P), sinCompartidos(Q).
sinCompartidos(paralelo(P, Q)) :- sinCompartidos(P), sinCompartidos(Q), buffersUsados(P, PS), buffersUsados(Q, QS), intersection(PS, QS, []).


%% Ejercicio 8
%ejecucionSegura(-XS, +BS, +CS) 
ejecucionSegura(XS, BS, CS) :- desde(0, L), generadorEjecuciones(XS, BS, CS, L), forall(member(B, BS), contenidoBuffer(B, XS, _)).

%% generadorEjecuciones(?XS, +BS, +CS, +L) 
generadorEjecuciones([], _, _, 0).
generadorEjecuciones([computar], _, _, 1).
generadorEjecuciones([escribir(B, C)], BS, CS, 1) :- member(B, BS), member(C, CS).
generadorEjecuciones([leer(B)], BS, _, 1) :- member(B, BS).
generadorEjecuciones(XS, BS, CS, L) :- 1 < L, generadorEjecuciones(X, BS, CS, 1), L1 is L-1, generadorEjecuciones(Rec, BS, CS, L1), append(X, Rec, XS).

%desde(+X, -Y)
desde(X,X).
desde(X,Y) :- N is X+1, desde(N,Y).

%% 8.1. Analizar la reversibilidad de XS, justificando adecuadamente por qué el predicado se comporta como lo hace.

% XS no es reversible en "ejecucionSegura":
% Al consultar al predicado teniendo a XS instanciado, el predicado "generadorEjecuciones" se ejecuta para cada número generado por el predicado "desde", los cuales son infinitos. 
% En caso de que XS sea efectivamente una ejecución segura, el predicado va a unificar una única vez cuando el número generado por el "desde" sea igual a la longitud de XS y devolverá true, 
% pero luego se colgará ya que seguirá buscando posibles soluciones para los infinitos números generados. Si XS no es una ejecución segura se cuelga directamente sin devolver true porque
% cuando el número generado por el "desde" es igual al largo de XS, el predicado "forall" va a fallar.


%%%%%%%%%%%
%% TESTS %%
%%%%%%%%%%%

cantidadTestsBasicos(12). 

testBasico(1) :- proceso(computar).
testBasico(2) :- proceso(secuencia(escribir(1,pepe),escribir(2,pipo))).
testBasico(3) :- buffersUsados(escribir(1, hola),[1]).

%proceso(+P)
testBasico(4) :- proceso(escribir(1,hello)).
testBasico(5) :- proceso(secuencia(paralelo(leer(1),leer(2)),escribir(2,hello))).
testBasico(6) :- not(proceso(zapallo)). 
testBasico(7) :- not(proceso(secuencia(paralelo(secuencia(hola,computar),leer(2)),escribir(2,hello)))). 

%buffersUsados(+P,?BS)
testBasico(8) :- buffersUsados(secuencia(leer(3), paralelo(escribir(3,chau), escribir(1,hola))), [3,1]). 
testBasico(9) :- buffersUsados(paralelo(secuencia(escribir(1, hola), leer(2)), secuencia(leer(3),escribir(4,chau))), [1,2,3,4]).
testBasico(10) :- not(buffersUsados(secuencia(escribir(1, hola), leer(3)), [1])). 
testBasico(11) :- not(buffersUsados(paralelo(secuencia(escribir(1, hola), hola),leer(2)), [1,2])).
testBasico(12) :- not(buffersUsados(secuencia(leer(3), paralelo(computar, escribir(1,hola))), [1,3])).

%---
cantidadTestsProcesos(12). 

%intercalar(+XS,+YS,?ZS)
testProcesos(1) :- intercalar([1,2,3],[4,5,6],[1,4,2,5,3,6]).
testProcesos(2) :- intercalar([1,2,3,4,5,6],[7,8,9,10,11,12], [1,7,2,8,3,9,4,10,5,11,6,12]).
testProcesos(3) :- intercalar([1],[3,4,5],[3,1,4,5]).
testProcesos(4) :- not(intercalar([],[3,4],[1,3,4])).
testProcesos(5) :- not(intercalar([1,2,3],[4,5,6],[2,4,1,5,3,6])).
testProcesos(6) :- not(intercalar([1,2,3,4,5,6],[7,8,9,10,11,12], [1,3,2,7,3,9,4,10,5,11,6,12])).

%serializar(+P,?XS)
testProcesos(7) :- serializar(secuencia(computar,leer(2)),[computar,leer(2)]).
testProcesos(8) :- serializar(paralelo(paralelo(leer(1),leer(2)),secuencia(leer(3),leer(4))),[leer(1),leer(3),leer(2),leer(4)]).
testProcesos(9) :- serializar(secuencia(paralelo(leer(1),computar),secuencia(escribir(2,hola),leer(2))), [computar, leer(1), escribir(2, hola), leer(2)]).
testProcesos(10) :- serializar(secuencia(paralelo(secuencia(leer(3),computar),paralelo(secuencia(escribir(1,hola), leer(2)),computar)),secuencia(computar,leer(4))), [computar, leer(3), computar, escribir(1, hola), leer(2), computar, leer(4)]).
testProcesos(11) :- not(serializar(paralelo(paralelo(leer(1),leer(2)),secuencia(leer(3),leer(4))),[leer(1),leer(4),leer(2),leer(3)])).
testProcesos(12) :- serializar(escribir(1, arbol), [escribir(1, arbol)]).

%---
cantidadTestsBuffers(16). 

%contenidoBuffer(+B,+ProcesoOLista,?L)
testBuffers(1) :- contenidoBuffer(2,[escribir(1,hola),escribir(2,zapallo),escribir(1,chau),computar,escribir(1,depende),leer(1)],[zapallo]).
testBuffers(2) :- contenidoBuffer(1,[escribir(1,hola),escribir(2,chau),escribir(1,hello),computar,escribir(1,priviet),leer(1)],[hello,priviet]).
testBuffers(3) :- contenidoBuffer(2,paralelo(escribir(2,tierra),secuencia(escribir(2,agua),leer(2))),[agua]).
testBuffers(4) :- contenidoBuffer(3, paralelo(secuencia(escribir(3, a), leer(3)), paralelo(secuencia(escribir(3, b), escribir(2, x)), secuencia(leer(3), secuencia(escribir(3, c), leer(2))))), [c]).
testBuffers(5) :- not(contenidoBuffer(1, [leer(1),escribir(1,hola)],[hola])).
testBuffers(6) :- not(contenidoBuffer(2, paralelo(escribir(2,tierra),secuencia(escribir(2,agua),leer(2))),[agua,tierra])).
testBuffers(7) :- not(contenidoBuffer(2, secuencia(escribir(2,hola),paralelo(escribir(2,tierra),leer(2))),[hola,tierra])).
testBuffers(8) :- not(contenidoBuffer(2, secuencia(escribir(1, hola), paralelo(leer(2), escribir(2, chau))), [_])).

%% contenidoLeido(+ProcesoOLista,?Contenidos)
testBuffers(9) :- contenidoLeido([escribir(1, casa), escribir(3, perro), escribir(1, camion)], []).
testBuffers(10) :- contenidoLeido([escribir(1, perro), escribir(1, gato), escribir(1, vaca), leer(1)], [perro]).
testBuffers(11) :- contenidoLeido(secuencia(paralelo(secuencia(escribir(1, verde), escribir(2, azul)), leer(1)), paralelo(secuencia(leer(2), escribir(3, negro)), escribir(1, amarillo))), [verde, azul]).
testBuffers(12) :- contenidoLeido(paralelo(secuencia(secuencia(paralelo(escribir(2, casa), leer(1)), escribir(3, verde)), leer(3)), paralelo(escribir(1, naranja), leer(2))), [naranja, casa, verde]).
testBuffers(13) :- not(contenidoLeido([escribir(1, arbol), escribir(3, palmera), leer(2)], [_])).
testBuffers(14) :- not(contenidoLeido([escribir(1, tierra), leer(1), leer(2)], [tierra])). 
testBuffers(15) :- not(contenidoLeido(paralelo(secuencia(leer(2), escribir(2, casa)), secuencia(escribir(3, alfombra), leer(3))), [_])).
testBuffers(16) :- not(contenidoLeido(paralelo(secuencia(secuencia(leer(3), escribir(2, abrigo)), escribir(1, heladera)), secuencia(leer(2), secuencia(escribir(3, computadora), escribir(1, living)))), [_])).

%---
cantidadTestsSeguros(9). 

%% esSeguro(+P)
testSeguros(1) :- esSeguro(secuencia(paralelo(secuencia(escribir(1,hesse),leer(1)),secuencia(escribir(2,cilantro),leer(2))),escribir(3,fuego))).
testSeguros(2) :- esSeguro(secuencia(paralelo(secuencia(escribir(1,kafka),leer(1)),secuencia(escribir(2,sandia),leer(2))),secuencia(escribir(3,lenteja),leer(3)))).
testSeguros(3) :- esSeguro(secuencia(paralelo(secuencia(escribir(1,mar),paralelo(leer(1),escribir(2,oceano))),secuencia(escribir(3,arena),paralelo(escribir(4,ciudad),leer(3)))),secuencia(leer(2),secuencia(escribir(5,nube),leer(5))))).
testSeguros(4) :- not(esSeguro(paralelo(paralelo(escribir(1,hamlet),escribir(2,raskolnikov)),paralelo(leer(2),leer(3))))).
testSeguros(5) :- not(esSeguro(secuencia(paralelo(secuencia(escribir(1,milena),leer(2)),secuencia(escribir(2,storni),leer(1))),secuencia(escribir(3,kubrick),leer(3))))).
testSeguros(6) :- not(esSeguro(secuencia(paralelo(secuencia(escribir(1,amelie),paralelo(leer(1),escribir(2,tren))),secuencia(escribir(1,libro),paralelo(escribir(4,viento),escribir(4,fujimoto)))),secuencia(leer(2),secuencia(escribir(5,luz),leer(5)))))).


%% ejecucionSegura(-XS, +BS, +CS) 
% Al no ser reversible "ejecucionSegura" por el "desde(0,Y)", lo testeamos con un predicado auxiliar.

%testerEjecucionSegura(+XS, +BS, +CS)
testerEjecucionSegura(XS, CS, BS) :- ejecucionSegura(YS, CS, BS), YS = XS, !. 

testSeguros(7) :- forall(ejecucionQueEsSegura1(XS), testerEjecucionSegura(XS, [1, 2, 5, 6], [avion, billar])).
ejecucionQueEsSegura1([]).
ejecucionQueEsSegura1([computar]).
ejecucionQueEsSegura1([escribir(1, billar)]).
ejecucionQueEsSegura1([escribir(2, avion), leer(2), escribir(1, avion)]).

testSeguros(8) :- forall(ejecucionQueEsSegura2(XS), testerEjecucionSegura(XS, [4, 3, 1, 2], [mate, bio, compu, datos])).
ejecucionQueEsSegura2([]).
ejecucionQueEsSegura2([escribir(1, mate), computar]).
ejecucionQueEsSegura2([escribir(3, datos), leer(3), escribir(4, mate), leer(4)]).
ejecucionQueEsSegura2([escribir(3, datos), escribir(4, mate), computar]).

testSeguros(9) :- forall(ejecucionQueEsSegura3(XS), testerEjecucionSegura(XS, [], [argentina, chile, paraguay])).
ejecucionQueEsSegura3([]).
ejecucionQueEsSegura3([computar]).
ejecucionQueEsSegura3([computar, computar, computar, computar]).


%%%%%%%%%%

tests(basico) :- cantidadTestsBasicos(M), forall(between(1,M,N), testBasico(N)).
tests(procesos) :- cantidadTestsProcesos(M), forall(between(1,M,N), testProcesos(N)).
tests(buffers) :- cantidadTestsBuffers(M), forall(between(1,M,N), testBuffers(N)).
tests(seguros) :- cantidadTestsSeguros(M), forall(between(1,M,N), testSeguros(N)).

tests(todos) :-
  tests(basico),
  tests(procesos),
  tests(buffers),
  tests(seguros).

tests :- tests(todos).