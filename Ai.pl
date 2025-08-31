:- module(server,[]).

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/websocket)).

:- initialization
    http_server(http_dispatch, [port(12345)]).

:- http_handler(root(.),          http_reply_file('index.html', []), []).
:- http_handler(root('board.html'), http_reply_file('board.html', []), []).
:- http_handler(root('Board.js'),  http_reply_file('Board.js',    []), []).
:- http_handler(root(ws),
    http_upgrade_to_websocket(check_board, []),
    [spawn([])]).


check_board(WebSocket) :-
    ws_receive(WebSocket, Message, [format(json)]),  % This waits until a message is received.
    (
        Message.opcode == close
    ->
        true

    ;
        check_board_response(Response_dict, Message),
        ws_send(WebSocket, json(Response_dict)),
        check_board(WebSocket)
    ).

check_board_response(Response_dict, Message) :-
    %%process_board(BoardList, N, NewBoardList),
    process_board(Message.data.board, Message.data.index, NewBoardList),
    write('Jogada do X2: '), write(NewBoardList), nl,
    Response_dict = _{ board: NewBoardList }. 


process_board(BoardList, N, NewBoardList) :-
    length(BoardList, Len),
    monta_tab(NumList, Len),
    move_x(NumList, N, TempBoardList),
    write('Jogada do X: '), write(TempBoardList), nl,
    joga(TempBoardList,NewBoardList),
    write('Jogada do O: '), 
    write(NewBoardList), nl, 
    write('4').
    

joga(Jogada,K) :- vence(Jogada, x) -> K = Jogada.
joga(Jogada,K) :- vence(Jogada, o) -> K = Jogada.
joga(Jogada,K) :- draw(Jogada) -> K = Jogada.
joga(Jogada,K) :- responde_o(Jogada,K), write('4').


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

vence(Jogada, Posicao) :- vencenalinha(Jogada,Posicao).
vence(Jogada, Posicao) :- vencenacoluna(Jogada,Posicao).
vence(Jogada, Posicao) :- vencenadiagonal(Jogada,Posicao).

% regras para vencer

vencenalinha(Jogada,Posicao):-
    length(Jogada,X),
   	N is truncate(sqrt(X)),
    teste_linha(Jogada,N,X,Posicao).

teste_linha(Jogada,N,X,Posicao):-  
    length(Jogada,L),
    length(R,L),
   	N1 is truncate(sqrt(L)),
    plus(N1,X0,X),
    X0=0 ->( length(Jogada,L),
    length(R,L),
   	N1 is truncate(sqrt(L)),
    g_l(R,Posicao,N1,N1,X,R0),
    same_c(R0,Jogada,K)->(    
    K=0->(false );K=1->(   true )) );
    length(Jogada,L),
    length(R,L),
   	N1 is truncate(sqrt(L)),
    g_l(R,Posicao,N1,N1,X,R0),
    same_c(R0,Jogada,K)->(    
    K=0->(plus(N1,X0,X),teste_linha(Jogada,N,X0,Posicao));K=1->(   true )).

g_l(Jogada_m,Posicao,I,N,X,J):-
    I>0->(   
    move_o(Jogada_m,X,J,Posicao),
    succ(I0,I),
    succ(X0,X),   
    g_l(J,Posicao,I0,N,X0,J));!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

vencenacoluna(Jogada,Posicao):-
    length(Jogada,X),
   	N is truncate(sqrt(X)),
    teste_coluna(Jogada,N,X,Posicao).

teste_coluna(Jogada,N,X,Posicao):-  
    length(Jogada,L),
    length(R,L),
   	N1 is truncate(sqrt(L)),
    succ(X1,X),
    plus(N1,X0,L),
    X1 = X0->( length(Jogada,L),
    length(R,L),
   	N1 is truncate(sqrt(L)),
    g_c(R,Posicao,N1,N1,X,R0),
    same_c(R0,Jogada,K)->(    
    K=0->(false );K=1->(   true )) );
    length(Jogada,L),
    length(R,L),
   	N1 is truncate(sqrt(L)),
    g_c(R,Posicao,N1,N1,X,R0),
    same_c(R0,Jogada,K)->(    
    K=0->(succ(X1,X),teste_coluna(Jogada,N,X1,Posicao));K=1->(   true )).

g_c(Jogada_m,Posicao,I,N,X,J):-
    I>0->(   
    move_o(Jogada_m,X,J,Posicao),
    succ(I0,I),
    plus(N,X0,X),   
    g_c(J,Posicao,I0,N,X0,J));!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

vencenadiagonal(Jogada,Posicao):-
    length(Jogada,X),
   	N is truncate(sqrt(X)),
    teste_diagonal(Jogada,N,X,Posicao).

teste_diagonal(Jogada,N,X,Posicao):-  
    length(Jogada,L),
    length(R,L),
   	N1 is truncate(sqrt(L)),
    succ(X1,X),
    plus(N1,X0,L),
    X1 = X0->( length(Jogada,L),
    length(R,L),
   	N1 is truncate(sqrt(L)),
    succ(N2,N1),
    g_d(R,Posicao,N1,N2,X,R0),
    same_c(R0,Jogada,K)->(    
    K=0->(false );K=1->(   true )) );
    length(Jogada,L),
    length(R,L),
   	N1 is truncate(sqrt(L)),
    succ(N1,N2),
    g_d1(R,Posicao,N1,N2,X,R0),
    same_c(R0,Jogada,K)->(    
    K=0->(succ(X1,X),teste_diagonal(Jogada,N,X1,Posicao));K=1->(   true )).

g_d(Jogada_m,Posicao,I,N,X,J):-
    I>0->(   
    move_o(Jogada_m,X,J,Posicao),
    succ(I0,I),
    plus(N,X0,X),   
    g_d(J,Posicao,I0,N,X0,J));!.

g_d1(Jogada_m,Posicao,I,N,X,J):-
    X>0->  (   
    I>0->(
    plus(N,X0,X),       
    move_o(Jogada_m,X,J,Posicao),
    succ(I0,I), 
    g_d1(J,Posicao,I0,N,X0,J)));!.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
x_pode_ganhar_em_uma(_Jogada,0,_):- false,!.

x_pode_ganhar_em_uma(Jogada,N,G) :- 
   	N>0,
    move_o1(Jogada, N, Novajogada,x),
    vence(Novajogada,x)->(G = N,true,!);(succ(N0,N),x_pode_ganhar_em_uma(Jogada,N0,G)).

rr_o(_Jogada,_Novajogada,0).
rr_o(Jogada,_,N):-
    move_o1(Jogada,N, Novajogada,o)->(
                                     vence(Novajogada,o)->  
                                     ( !,false 
                                     );( succ(N0,N),rr_o(Jogada,_Novajogada,N0) )
                                     );(succ(N0,N),rr_o(Jogada,_Novajogada,N0)).

responde_o(Jogada,Novajogada) :-
   	length(Jogada,N),
    qq_move(Jogada,NNovajogada,N)->(  
   vence(NNovajogada,o)->(!);
 	( length(Jogada,N),
     x_pode_ganhar_em_uma(NNovajogada,N,G)->
    (move_o1(Jogada,G, Novajogada,o),!,true
    );
    (false))
           );(  false ).


responde_o(Jogada,Novajogada) :- 
 	length(Jogada,N),
    rr_o(Jogada,Novajogada,N),
    N>0 ->(   
          (move_o1(Jogada,N, Novajogada,o),!,true);
          ( !)
          );( false ).


responde_o(Jogada,Novajogada) :-
   	draw(Jogada)->
    (writeln('Empate'),!
    );
    (length(Jogada,N),
     qq_move1(Jogada,_,Novajogada,N),!).


qq_move1([H|T],R,Novajogada,N):-
    integer(H)->
    (R = H->  ( move_o1([H|T],N, Novajogada,o)   
              );( insert_end(R,H,H1),insert_end(H1,T,Jogada),move_o1(Jogada,N,Novajogada,o),!    )
    );(R=H->  (succ(N0,N),qq_move1(T,R,_Novajogada,N0)
              );
      (  length(R,1)->  ( insert_end(R,H,R1) ,succ(N0,N),qq_move1(T,R1,_Novajogada,N0)
                        );(insert_end(R,H,R1) ,succ(N0,N),qq_move1(T,R1,_Novajogada,N0)  )  )).
    
qq_move(_Jogada,_Novajogada,0):-false.
qq_move(Jogada,Novajogada,N):-
  move_o1(Jogada,N,Novajogada,o)->(true);(succ(N0,N), qq_move(Jogada,Novajogada,N0)  )   .
draw([]).
draw([H|T]):-
    integer(H)->  (!,false );( draw(T)).



fdisplay(Jogada):-
    length(Jogada,X),
    nl,
    while(Jogada,X).

while(J,X):-
    length(J,Q),
    N is truncate(sqrt(Q)),
    plus(N,X0,X),
    write(J,X0,_,N,_).

write([H|T],X,R,N,I):-
    length(T,J), 
   	J>=X->  (  
           length(R,1) -> (
                          length(R,N)->(write(R),nl); (   
                         test_R(R,H,I)->  (
                                succ(I0,I),
           						write(T,X,R,N,I0)
                                        )
                                                      );
           length(R,N)->(write(R),nl);(   
           insert_end(R,H,R0),
           write(T,X,R0,N,I)
						)
                          );
           insert_end(R,H,R0),
           write(T,X,R0,N,I)
           );  
    write(R),nl,
    length(T,J),
    succ(J,J0),
    plus(N,X0,J0),
    X0=0->  (wr([H|T]),nl );
    length(T,J),
    succ(J,J0),
    plus(N,X0,J0),
    write([H|T],X0,_R0,N,_I0),
    !.

wr(T):- 
    write(T).

test_R([R|_],H,I):-
    I = 1->  (   R = H ->  true ; false); false.

monta_tab(T,A) :-
   numlist(1,A,T).

move_monta([H|L],X,R,E,K,B,Posicao):-  
    length(L,I),
   	I=X ->   (
     integer(H) ->  (   
     length(U,1),
     maplist(=(Posicao),U),
     append([U,L],E),
    I=X * f_member(Posicao,B)-> ( clone(E,K) );
            insert_end(R,Posicao,R0),
            move_monta(L,X,R0,E,K,K,Posicao)
            ); write('movimento errado.'), nl,
             insert_end(R,H,K1),    
             append([K1,L],K),!
    );   
    length(L,I),
    I>0->(   
    insert_end(R,H,R0),
    move_monta(L,X,R0,E,K,K,Posicao)
    );
    length(L,I),
    I=0,
    insert_end(R,H,K),
    !.
    
move_x(T,J,NT):-  
    length(T,A),
    J=A-> 
    del_last(T,T0),
    insert_end(T0,x,NT)
    ; 
    length(T,A),
    J>A ->  
    write('movimento errado.'),nl, 
    false,!
    ;
    length(T,A),
    plus(J,X,A),
    move_monta(T,X,_,_,NT,_,x).



move_o(T,J,NT,Posicao):-  
    length(T,A),
    J>A->  (false,!);(
    length(T,A),
    J=A-> 
    del_last(T,T0),
    insert_end(T0,Posicao,NT)
    ;
    length(T,A),
    plus(J,X,A),
    move_monta_g(T,X,_,_,NT,_,Posicao)).

move_o1(T,J,NT,Posicao):-  
    length(T,A),
    J>A->  ( succ(J0,J ),move_o1(T,J0,NT,Posicao));(
    length(T,A),
    J=A-> ( last(T,L),
            integer(L)->  (   
        	del_last(T,T0),
    		insert_end(T0,Posicao,NT)  );(  succ(J0,J ),move_o1(T,J0,NT,Posicao))
    );
    length(T,A),
    plus(J,X,A),
    move_monta_g1(T,X,_,_,NT,_,Posicao)).

move_monta_g([H|L],X,R,E,K,B,Posicao):-
    length(L,I),
   	I=X ->   (
    var(H)->(            
     length(U,1),
     maplist(=(Posicao),U),
     append([U,L],E),
    I=X * f_member(Posicao,B)-> ( clone(E,K) );
            insert_end(R,Posicao,R0),
            move_monta_g(L,X,R0,E,K,K,Posicao)
     );integer(H)->( 
     length(U,1),
     maplist(=(Posicao),U),
     append([U,L],E),
    	I=X * f_member(Posicao,B)-> ( clone(E,K) );
            insert_end(R,Posicao,R0),
            move_monta_g(L,X,R0,E,K,K,Posicao)  );!  
             );   
    length(L,I),
    I>0->(   
    insert_end(R,H,R0),
    move_monta_g(L,X,R0,E,K,K,Posicao)
    );
    length(L,I),
    I=0,
    insert_end(R,H,K),
    !.

move_monta_g1([H|L],X,R,E,K,B,Posicao):-  
    length(L,I),
   	I=X ->   (
     integer(H) ->  (   
     length(U,1),
     maplist(=(Posicao),U),
     append([U,L],E),
    I=X * f_member(Posicao,B)-> ( clone(E,K) );
            insert_end(R,Posicao,R0),
            move_monta_g1(L,X,R0,E,K,K,Posicao),!
            );
             insert_end(R,H,K1),    
             append([K1,L],K0),
             succ(X,X0),
             move_monta_g1(K0,X0,_,_,K,_,Posicao),!
    );   
    length(L,I),
    I>0->(   
    insert_end(R,H,R0),
    move_monta_g1(L,X,R0,E,K,K,Posicao),!
    );
    length(L,I),
    I=0,
    insert_end(R,H,K),
    !.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
same_c([], [],1).

same_c([H1|R1], [H2|R2],K):-
    same_c(H1,H2,K),     
    same_c(R1, R2,K).
same_c(H1,H1,1).
same_c(_H1,_H2,0).

clone([],[]).
clone([H|T],[H|Z]):- clone(T,Z).
clone(T,T).

del_last([], []).
del_last([H|T], List) :- del_last1(T, H, List).

del_last1([], _, []).
del_last1([H|T], Prev, [Prev|List]) :- del_last1(T, H, List).

insert_end(L, X, NewL) :-
append_list(L, [X], NewL).

append_list([], L2, L2).    
append_list([X | L1], L2, [X | L3]) :-
    append_list(L1, L2, L3).