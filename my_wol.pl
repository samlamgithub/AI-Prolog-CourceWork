
% version for SICStus 4.x

:- use_module(library(lists)).
:- use_module(library(ordsets)).
:- use_module(library(random)).



%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%  QUESTION 3:
%%%%%%%%%%%%%%%%%%%%  test_strategy/3


time_stamp(X):- statistics(runtime, [Z| _]), X is Z/1000.

test_strategy(N,S1,S2) :- 
play_help(ND,NB,NR,TOTAL,N,S1,S2,Short,Long,TimeTotal),
write('Number of draws: '),write(ND),
nl,
write('Number of blue winning: '),write(NB),
nl,
write('Number of red winning: '),write(NR),
nl,
write('Total Moves: '),write(TOTAL),
nl,
write('Average Game Length: '),(N\=0->Ave is TOTAL/N;Ave is 0),write(Ave),
nl,
write('Shortest Game Moves: '),write(Short),
nl,
write('Longest Game Moves(non exhaustive): '),write(Long),
nl,
write('Total Game Time '),write(TimeTotal),
nl,
write('Average Game Time: '),(N\=0->AveTime is TimeTotal/N;AveTime is 0),write(AveTime).

play_help(0,0,0,0,0,_,_,0,0,0).

play_help2(X,Y,Z,NumMoves,1,S1,S2,NumMoves,Long,Time):-
play2(quiet, S1, S2, NumMoves, RESULTT,Time),decide(X,Y,Z,RESULTT),
((NumMoves==250)->Long is 0;Long is NumMoves).

play_help(X,Y,Z,T,Remain,S1,S2,Short,Long,Total_Time):- 
Remain \= 0,
((Remain > 1) -> 
  (
    play2(quiet, S1, S2, NumMoves, RESULTT,Time),decide(D,B,R,RESULTT),
    R2 is Remain-1,
 play_help(N_d,N_b,N_r,T_Move,R2,S1,S2,Short2,Long2,Sub_Total),
X is N_d+D,Y is N_b+B,Z is N_r+R,T is T_Move+NumMoves,Short is min(NumMoves, Short2),Total_Time is Sub_Total+Time,
((NumMoves==250)->Long is Long2;Long is max(Long2,NumMoves))
)
  ; 
  (play_help2(X,Y,Z,T,Remain,S1,S2,Short,Long,Total_Time))
  )
.

play2(quiet, S1, S2, NumMoves, WinningPlayer,Time):- time_stamp(X),
play(quiet, S1, S2, NumMoves, WinningPlayer),
time_stamp(Y),Time is Y-X.

decide(0,0,1,'r').
decide(0,1,0,'b').
decide(1,0,0,'draw').
decide(0,0,0,'exhaust').
decide(0,0,0,'stalemate').






%---------- PART 2 ------------





%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%% BLOODLUST STRATEGIES

bloodlust(PlayerColour, CurrentBoardState, NewBoardState, Move) :-
   move_piece(PlayerColour, bloodlust, CurrentBoardState, NewBoardState, Move).

minimo([(A,X)], (A,X)).
minimo([(A,X),(B,Y)|Tail], N):-
    ( X > Y ->
        minimo([(B,Y)|Tail], N)
    ;
        minimo([(A,X)|Tail], N)
    ).

bloodlust_red(Alive, OtherPlayerAlive, Move) :-
 findall(
  ([A,B,MA,MB],X),
  (member([A,B], Alive),
                      neighbour_position(A,B,[MA,MB]),
                \+member([MA,MB],Alive),
                \+member([MA,MB],OtherPlayerAlive),
             alter_board([A,B,MA,MB], Alive, NewAlive),
              next_generation([OtherPlayerAlive,NewAlive], [NBLUE,NRED]),
              length(NBLUE,X)),
   PossMoves
   ),
minimo(PossMoves,(Move,Minimum)).

bloodlust_blue(Alive, OtherPlayerAlive, Move) :-
 findall(
  ([A,B,MA,MB],X),
  (
  member([A,B], Alive),
                      neighbour_position(A,B,[MA,MB]),
             \+member([MA,MB],Alive),
              \+member([MA,MB],OtherPlayerAlive),
             alter_board([A,B,MA,MB], Alive, NewAlive),
             next_generation([NewAlive,OtherPlayerAlive], [NBLUE,NRED]),
              length(NRED,X)),
   PossMoves),
minimo(PossMoves,(Move,Minimum)).

bloodlust(PieceColour, Board, NewBoard, Move):-
bloodlust2(PieceColour, ,bloodlust,Board, NewBoard, Move).

bloodlust2('b', bloodlust, [AliveBlues, AliveReds], [NewAliveBlues, AliveReds], Move) :-
 bloodlust_blue(AliveBlues, AliveReds, Move),
 alter_board(Move, AliveBlues, NewAliveBlues).

bloodlust2('r', bloodlust, [AliveBlues, AliveReds], [AliveBlues, NewAliveReds], Move) :-
 bloodlust_red(AliveReds, AliveBlues, Move),
 alter_board(Move, AliveReds, NewAliveReds). 

%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%% Self Preservation STRATEGY



maxx([(A,X)], (A,X)).
maxx([(A,X),(B,Y)|Tail], N):-
    ( X < Y ->
        maxx([(B,Y)|Tail], N)
    ;
        maxx([(A,X)|Tail], N)
    ).


self_preservation_blue(Alive, OtherPlayerAlive, Move):-
findall(([A,B,MA,MB],X),(member([A,B], Alive),
                      neighbour_position(A,B,[MA,MB]),
                \+member([MA,MB],Alive),
                \+member([MA,MB],OtherPlayerAlive),
alter_board([A,B,MA,MB], Alive, NewAlive),
  next_generation([NewAlive,OtherPlayerAlive], [NBLUE,NRED]),
  length(NBLUE,X)),
   PossMoves),
 maxx(PossMoves,(Move,Maximum)).

 self_preservation_red(Alive, OtherPlayerAlive, Move):-
findall(([A,B,MA,MB],X),(member([A,B], Alive),
                      neighbour_position(A,B,[MA,MB]),
                \+member([MA,MB],Alive),
                \+member([MA,MB],OtherPlayerAlive),
alter_board([A,B,MA,MB], Alive, NewAlive),
  next_generation([OtherPlayerAlive, NewAlive], [NBLUE,NRED]),
  length(NRED,X)),
   PossMoves),
 maxx(PossMoves,(Move,Maximum)).

 self_preservation(PieceColour, Board, NewBoard, Move):-
  self_preservation2(PieceColour, self_preservation,Board, NewBoard, Move).

self_preservation2('b', self_preservation, [AliveBlues, AliveReds], [NewAliveBlues, AliveReds], Move) :-
 self_preservation_blue(AliveBlues, AliveReds, Move),
 alter_board(Move, AliveBlues, NewAliveBlues).

self_preservation2('r', self_preservation, [AliveBlues, AliveReds], [AliveBlues, NewAliveReds], Move) :-
 self_preservation_red(AliveReds, AliveBlues, Move),
 alter_board(Move, AliveReds, NewAliveReds). 



%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%% Land Grab STRATEGY


land_grab_blue(Alive, OtherPlayerAlive, Move):-
findall(([A,B,MA,MB],X),(member([A,B], Alive),
                      neighbour_position(A,B,[MA,MB]),
                \+member([MA,MB],Alive),
                \+member([MA,MB],OtherPlayerAlive),
alter_board([A,B,MA,MB], Alive, NewAlive),
  next_generation([NewAlive,OtherPlayerAlive], [NBLUE,NRED]),
  length(NBLUE,Y),length(NRED,Z),X is Y-Z),
   PossMoves),
 maxx(PossMoves,(Move,Maximum)).

 land_grab_red(Alive, OtherPlayerAlive, Move):-
findall(([A,B,MA,MB],X),(member([A,B], Alive),
                      neighbour_position(A,B,[MA,MB]),
                \+member([MA,MB],Alive),
                \+member([MA,MB],OtherPlayerAlive),
alter_board([A,B,MA,MB], Alive, NewAlive),
  next_generation([OtherPlayerAlive, NewAlive], [NBLUE,NRED]),
  length(NBLUE,Y),length(NRED,Z),X is Z-Y),
   PossMoves),
 maxx(PossMoves,(Move,Maximum)).

land_grab(PieceColour, Board, NewBoard, Move):- 
land_grab2(PieceColour,land_grab, Board, NewBoard, Move).


land_grab2('b', land_grab, [AliveBlues, AliveReds], [NewAliveBlues, AliveReds], Move) :-
 land_grab_blue(AliveBlues, AliveReds, Move),
 alter_board(Move, AliveBlues, NewAliveBlues).

land_grab2('r', land_grab, [AliveBlues, AliveReds], [AliveBlues, NewAliveReds], Move) :-
 land_grab_red(AliveReds, AliveBlues, Move),
 alter_board(Move, AliveReds, NewAliveReds). 


%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%% MiniMax  STRATEGY

minimax_blue(Alive, OtherPlayerAlive, Move):-
findall(([A,B,MA,MB],X),(member([A,B], Alive),
                      neighbour_position(A,B,[MA,MB]),
                \+member([MA,MB],Alive),
                \+member([MA,MB],OtherPlayerAlive),
  alter_board([A,B,MA,MB], Alive, NewAlive),
  next_generation([NewAlive,OtherPlayerAlive], [NBLUE,NRED]),
  test_if_blue_already_wins(NBLUE,NRED,X)),
   PossMoves),
 maxx(PossMoves,(Move,Maximum)).

test_if_blue_already_wins(NBLUE,NRED,X) :-
  length(NRED,0) -> X is 64;
  land_grab_red(NRED, NBLUE, RedMove),
  alter_board(RedMove, NRED, NNRED),
  next_generation([NBLUE,NNRED], [NewBlue,NewRed]),
  length(NewBlue,Y),length(NewRed,Z),X is Y-Z.


minimax_red(Alive, OtherPlayerAlive, Move):-
findall(([A,B,MA,MB],X),(member([A,B], Alive),
                      neighbour_position(A,B,[MA,MB]),
                \+member([MA,MB],Alive),
                \+member([MA,MB],OtherPlayerAlive),
  alter_board([A,B,MA,MB], Alive, NewAlive),
  next_generation([OtherPlayerAlive, NewAlive], [NBLUE,NRED]),
  test_if_red_already_wins(NBLUE,NRED,X)),
   PossMoves),
 maxx(PossMoves,(Move,Maximum)).

test_if_red_already_wins(NBLUE,NRED,X) :-
  length(NBLUE,0) -> X is 64;
  land_grab_blue(NBLUE,NRED, BlueMove),
  alter_board(BlueMove, NBLUE, NNBLUE),
  next_generation([NNBLUE,NRED], [NewBlue,NewRed]),
  length(NewBlue,Y),length(NewRed,Z),X is Z-Y.

minimax(PieceColour, Board, NewBoard, Move):-
minimax2(PieceColour,minimax, Board, NewBoard, Move).

minimax2('b', minimax, [AliveBlues, AliveReds], [NewAliveBlues, AliveReds], Move) :-
 minimax_blue(AliveBlues, AliveReds, Move),
 minimax(Move, AliveBlues, NewAliveBlues).

minimax2('r', minimax, [AliveBlues, AliveReds], [AliveBlues, NewAliveReds], Move) :-
 minimax_red(AliveReds, AliveBlues, Move),
 alter_board(Move, AliveReds, NewAliveReds). 

