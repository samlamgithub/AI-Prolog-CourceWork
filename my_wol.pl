
% version for SICStus 4.x

:- use_module(library(lists)).
:- use_module(library(ordsets)).
:- use_module(library(random)).





%---------- PART 1 -------------

time_stamp(X):- statistics(runtime, [Z| _]), X is Z*1000.

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

decide(0,0,1,r).
decide(0,1,0,b).
decide(1,0,0,draw).


%---------- PART 1 -------------

%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%% PLAYING THE GAME: BASIC CONTROL

%%%%% play/5

play(ShowFlag, FirstPlayerStrategy, SecondPlayerStrategy, TotalMoves, Winner) :-
 start_config(random, Board),
 (
  ShowFlag == verbose,
  format('~nInitial State:~n~n', []),
  draw_board(Board),
  show_score(verbose, Board)
  ;
  ShowFlag == quiet
 ),
 !,
 make_move(Board, ShowFlag, _, 'b', FirstPlayerStrategy, 'r', SecondPlayerStrategy, 0, TotalMoves, Winner).

%%%%% make_move/10
%
% Arguments are as follows:
%
% make_move(Board, ShowBoard, FinalBoard, Player, PlayerStrat, NextPlayer, NextPlayerStrat, Moves, TotalMoves, Winner)

make_move([[],[]], ShowFlag, [[],[]], _, _, _, _, NumMoves, NumMoves, 'draw') :-
 !,
 show_winner(ShowFlag, 'draw', NumMoves).

make_move(_, ShowFlag, _, _, _, _, _, 250, 250, 'exhaust') :-
 !,
 show_winner(ShowFlag, 'exhaust', 250).

make_move([[],Reds], ShowFlag, [[],Reds], _, _, _, _, NumMoves, NumMoves, 'r') :-
 !,
 show_winner(ShowFlag, 'red', NumMoves).

make_move([Blues,[]], ShowFlag, [Blues,[]], _, _, _, _, NumMoves, NumMoves, 'b') :-
 !,
 show_winner(ShowFlag, 'blue', NumMoves).

make_move(Board, ShowFlag, FinalBoard, Player, Strategy, NextPlayer, NextStrategy, NumMoves, TotalMoves, Winner) :-
 NewNumMoves is NumMoves + 1,
 move_piece(Player, Strategy, Board, NewBoard, Move),
 show_move(ShowFlag, NewNumMoves, Player, Move),
 draw_board(ShowFlag, NewBoard),
 next_generation(NewBoard, CrankedNewBoard),
 draw_board(ShowFlag, CrankedNewBoard),
 show_score(ShowFlag, CrankedNewBoard),
 !,
 make_move(CrankedNewBoard, ShowFlag, FinalBoard, NextPlayer, NextStrategy, Player, Strategy, NewNumMoves, TotalMoves, Winner).

make_move(_, ShowFlag, _, _, _, _, _, TotalMoves, TotalMoves, 'stalemate') :-
  show_winner(ShowFlag, 'Stalemate', TotalMoves).

%%%%% alter_board/3
%
% replaces a pair [A,B] with [MA,MB] in Alives; result is NewAlives
% Alives must be ordered; NewAlives will be too.

alter_board([A,B,MA,MB], Alives, NewAlives) :-
 ord_del_element(Alives, [A,B], AlivesMinus),
 ord_add_element(AlivesMinus, [MA,MB], NewAlives).

%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%% RANDOM MOVE STRATEGY

random_move(Alive, OtherPlayerAlive, Move) :-
 findall([A,B,MA,MB],(member([A,B], Alive),
                      neighbour_position(A,B,[MA,MB]),
	              \+member([MA,MB],Alive),
	              \+member([MA,MB],OtherPlayerAlive)),
	 PossMoves),
 length(PossMoves,L),
 LP1 is L + 1,
 random(1, LP1, Pos),
 nth1(Pos, PossMoves, Move).

move_piece('b', random, [AliveBlues, AliveReds], [NewAliveBlues, AliveReds], Move) :-
 random_move(AliveBlues, AliveReds, Move),
 alter_board(Move, AliveBlues, NewAliveBlues).

move_piece('r', random, [AliveBlues, AliveReds], [AliveBlues, NewAliveReds], Move) :-
 random_move(AliveReds, AliveBlues, Move),
 alter_board(Move, AliveReds, NewAliveReds).

%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%% BLOODLUST STRATEGIES

maxx([X], X) :- !.
maxx([X,Y|Tail], N):-
    ( X > Y ->
        maxx([X|Tail], N)
    ;
        maxx([Y|Tail], N)
    ).

bloodlust_red(Alive, OtherPlayerAlive, Move) :-
 findall([A,B,MA,MB],(member([A,B], Alive),
                      neighbour_position(A,B,[MA,MB]),
                \+member([MA,MB],Alive),
                \+member([MA,MB],OtherPlayerAlive)),
   PossMoves),
 findall(X,(member(Mov,PossMoves),alter_board(Mov, Alive, NewAlive),
next_generation([OtherPlayerAlive,NewAlive],  [NBLUE,NRED]),
  length(NBLUE,X)
  ),
 Boards),
maxx(Boards,Maximum),
member(Move,PossMoves),
alter_board(Move, Alive, NewAlive2),
next_generation([OtherPlayerAlive,NewAlive2],[NB2,NR2]),
  length(NB2,Maximum).


bloodlust_blue(Alive, OtherPlayerAlive, Move) :-
 findall([A,B,MA,MB],(member([A,B], Alive),
                      neighbour_position(A,B,[MA,MB]),
                \+member([MA,MB],Alive),
                \+member([MA,MB],OtherPlayerAlive)),
   PossMoves),
 findall(X,(member(Mov,PossMoves),alter_board(Mov, Alive, NewAlive),
next_generation([NewAlive,OtherPlayerAlive], [NBLUE,NRED]),
  length(NRED,X)
  ),
 Boards),
maxx(Boards,Maximum),
member(Move,PossMoves),
alter_board(Move, Alive, NewAlive2),
next_generation([NewAlive2,OtherPlayerAlive],[NB2,NR2]),
  length(NR2,Maximum).

move_piece('b', bloodlust, [AliveBlues, AliveReds], [NewAliveBlues, AliveReds], Move) :-
 bloodlust_blue(AliveBlues, AliveReds, Move),
 alter_board(Move, AliveBlues, NewAliveBlues).

move_piece_red('r', bloodlust, [AliveBlues, AliveReds], [AliveBlues, NewAliveReds], Move) :-
 bloodlust_red(AliveReds, AliveBlues, Move),
 alter_board(Move, AliveReds, NewAliveReds). 

%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%Self Preservation STRATEGY
minimo([X], X) :- !.
minimo([X,Y|Tail], N):-
    ( X > Y ->
        minimo([Y|Tail], N)
    ;
        minimo([X|Tail], N)
    ).


self_preservation(Alive, OtherPlayerAlive, Move):-
findall([A,B,MA,MB],(member([A,B], Alive),
                      neighbour_position(A,B,[MA,MB]),
                \+member([MA,MB],Alive),
                \+member([MA,MB],OtherPlayerAlive)),
   PossMoves),
 findall(X,(member(Mov,PossMoves),alter_board(Mov, Alive, NewAlive),
  length(NewAlive,X)
  ),
 Boards),
minimo(Boards,Minimum),
member(Move,PossMoves),
alter_board(Move, Alive, NewAlive2),
  length(NewAlive2,Minimum).



move_piece('b', self_preservation, [AliveBlues, AliveReds], [NewAliveBlues, AliveReds], Move) :-
 self_preservation(AliveBlues, AliveReds, Move),
 alter_board(Move, AliveBlues, NewAliveBlues).

move_piece('r', self_preservation, [AliveBlues, AliveReds], [AliveBlues, NewAliveReds], Move) :-
 self_preservation(AliveReds, AliveBlues, Move),
 alter_board(Move, AliveReds, NewAliveReds). 



%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%% Land Grab STRATEGY


land_grab_red(Alive, OtherPlayerAlive, Move) :-
 findall([A,B,MA,MB],(member([A,B], Alive),
                      neighbour_position(A,B,[MA,MB]),
                \+member([MA,MB],Alive),
                \+member([MA,MB],OtherPlayerAlive)),
   PossMoves),
 findall(X,(member(Mov,PossMoves),alter_board(Mov, Alive, NewAlive),
next_generation([OtherPlayerAlive,NewAlive],  [NBLUE,NRED]),
  length(NRED,Y),length(NBLUE,Z),X is Y-Z
  ),
 Boards),
maxx(Boards,Maximum),
member(Move,PossMoves),
alter_board(Move, Alive, NewAlive2),
next_generation([OtherPlayerAlive,NewAlive2],[NB2,NR2]),
 length(NB2,LB), length(NR2,LR), LR-LB = Maximum.


land_grab_blue(Alive, OtherPlayerAlive, Move) :-
 findall([A,B,MA,MB],(member([A,B], Alive),
                      neighbour_position(A,B,[MA,MB]),
                \+member([MA,MB],Alive),
                \+member([MA,MB],OtherPlayerAlive)),
   PossMoves),
 findall(X,(member(Mov,PossMoves),alter_board(Mov, Alive, NewAlive),
next_generation([NewAlive,OtherPlayerAlive], [NBLUE,NRED]),
  length(NRED,Y),length(NBLUE,Z),X is Z-Y
  ),
 Boards),
maxx(Boards,Maximum),
member(Move,PossMoves),
alter_board(Move, Alive, NewAlive2),
next_generation([NewAlive2,OtherPlayerAlive],[NB2,NR2]),
  length(NB2,LB), length(NR2,LR), LB-LR = Maximum.

move_piece('b', land_grab, [AliveBlues, AliveReds], [NewAliveBlues, AliveReds], Move) :-
 land_grab_blue(AliveBlues, AliveReds, Move),
 alter_board(Move, AliveBlues, NewAliveBlues).

move_piece('r', land_grab, [AliveBlues, AliveReds], [AliveBlues, NewAliveReds], Move) :-
 land_grab_red(AliveReds, AliveBlues, Move),
 alter_board(Move, AliveReds, NewAliveReds). 


%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%% MiniMax  STRATEGY

minimax(+PlayerColour, +CurrentBoardState, -NewBoardState, -Move).


move_piece(PieceColour, minimax, Board, NewBoard, Move) :-
 minimax(PieceColour, Board, NewBoard, Move).


%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%

%---------- PART 2 ------------




%---------- PART 2 ------------

%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%% STARTING CONFIGURATIONS

%%%%% start_config/2

start_config(random, [OrdBlues,OrdReds]) :-
 !,
 findall([R,C], cell(R,C), Cells),
 pick(12, Cells, Blues, Rest),
 pick(12, Rest, Reds, _),
 list_to_ord_set(Blues, OrdBlues),
 list_to_ord_set(Reds, OrdReds).

start_config(cross, [[[1,1],[2,2],[3,3],[4,4],[5,5],[6,6],[7,7],[8,8]],
		     [[1,8],[2,7],[3,6],[4,5],[5,4],[6,3],[7,2],[8,1]]]) :-
 !.

start_config(checkers, [[[3,1],[3,3],[3,5],[3,7],[4,2],[4,4],[4,6],[4,8]],
			[[5,1],[5,3],[5,5],[5,7],[6,2],[6,4],[6,6],[6,8]]]) :-
 !.

start_config(gliders, [[[1,3],[2,1],[2,3],[3,2],[3,3]],
		       [[6,6],[6,7],[6,8],[7,6],[8,7]]]) :-
 !.

start_config(X,X) :-
 ground(X).

%%%%% cell/2
%
% backtracks to find all cells

cell(A, B) :-
 member(A, [1,2,3,4,5,6,7,8]),
 member(B, [1,2,3,4,5,6,7,8]).

%%%%% pick/4

pick(Total, From, Picked, Rest) :-
 pick_aux(0, Total, From, Picked, Rest).

%%%%% pick_aux/5

pick_aux(Total, Total, Rest, [], Rest) :-
 !.

pick_aux(N, Total, From, [E|Picked], Rest) :-
 random_select(E, From, NewFrom),
 N1 is N + 1,
 pick_aux(N1, Total, NewFrom, Picked, Rest).

%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%% CONWAY CRANK (NEXT GENERATION)

%%%%% next_generation/2
%
% basc control for Conway next generation

next_generation(Board, [NewAliveBlues, NewAliveReds]) :-
 findall([A,B,NewW], (cell(A,B), 
                      what_in_cell(Board, A, B, W), 
                      change_cell(Board, A, B, W, NewW)),
         ABWs),
 findall([A,B], member([A,B,b], ABWs), NewAliveBlues),
 findall([A,B], member([A,B,r], ABWs), NewAliveReds).

%%%%% what_in_cell/4

what_in_cell([AliveBlues, _], A, B, 'b') :-
 member([A,B], AliveBlues).

what_in_cell([_, AliveReds], A, B, 'r') :-
 member([A,B], AliveReds).

what_in_cell([AliveBlues, AliveReds], A, B, ' ') :-
 \+ member([A,B], AliveBlues), 
 \+ member([A,B], AliveReds).

%%%%% cchange_cell/5

change_cell([AliveBlues, AliveReds], A, B, W, NewW) :-
 findall(b, (neighbour_position(A,B,[NA,NB]),
             member([NA,NB], AliveBlues)),
         Bs),
 findall(r, (neighbour_position(A,B,[NA,NB]),
             member([NA,NB], AliveReds)),
         Rs),
 length(Bs, BL),
 length(Rs, RL),
 populate_cell(BL,RL,W,NewW),
 !.

%%%%% neighbour_position/3

neighbour_position(A,B,[I,J]) :-
 AM1 is A - 1,
 AP1 is A + 1,
 BM1 is B - 1,
 BP1 is B + 1,
 L = [AM1,A,AP1],
 K = [BM1,B,BP1],
 member(I,L),
 member(J,K),
 \+ (I == A, J == B),
 \+ I == 0,
 \+ J == 0,
 \+ I > 8,
 \+ J > 8.

%%%%% populate_cell/4

populate_cell(3,0,' ',b).

populate_cell(0,3,' ',r).

populate_cell(2,1,' ',b).

populate_cell(1,2,' ',r).

populate_cell(NumBlues,NumReds,X,X) :-
 2 is NumBlues + NumReds.

populate_cell(NumBlues,NumReds,X,X) :-
 3 is NumBlues + NumReds.

populate_cell(_,_,_,' ').

%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%% DRAWING THE BOARD

%%%%% draw_board/1
%
% wrapper for draw_board/2

draw_board(Board) :-
 draw_board(verbose, Board).

%%%%% draw_board/2
%
% draws a board, if needed

draw_board(quiet, _) :-
 !.

draw_board(verbose, Board) :-
 format('  12345678~n +--------+', []),
 draw_cells(1, 1, Board),
 format('~n +--------+~n~n', []).

%%%%% draw_cells/3
%
% draw the right colour in cells

% beginning of row

draw_cells(A, 1, ColouredCells) :-
 !,
 format('~n~w|', [A]),
 draw_cell(A, 1, ColouredCells, NewColouredCells),
 draw_cells(A, 2, NewColouredCells).

% end of row

draw_cells(A, 8, ColouredCells) :-
 !,
 draw_cell(A, 8, ColouredCells, NewColouredCells),
 format('|', []),
 (
  A = 8
  -> true
  ;  A1 is A + 1,
     draw_cells(A1, 1, NewColouredCells)
 ).

% middle of row

draw_cells(A, B, ColouredCells) :-
 draw_cell(A, B, ColouredCells, NewColouredCells),
 B1 is B + 1,
 draw_cells(A, B1, NewColouredCells).

%%%%% draw_cell/4
%
% draw the right colour in a cell

draw_cell(A, B, [[[A,B]|RestBlues],Reds], [RestBlues,Reds]) :-
 !,
 format('b', []).

draw_cell(A, B, [Blues,[[A,B]|RestReds]], [Blues,RestReds]) :-
 !,
 format('r', []).

draw_cell(_, _, ColouredCells, ColouredCells) :-
 format(' ', []).

%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%% SHOWING THE SCORE

%%%%% show_score/2

show_score(quiet, _) :-
 !.

show_score(verbose, [AliveBlues, AliveReds]) :-
 length(AliveBlues, BL),
 length(AliveReds, RL),
 format('~nblue score = ~w~nredscore = ~w~n~n', [BL,RL]).

%%%%% show_move/4

show_move(quiet, _, _, _) :-
 !.

show_move(verbose, Num, Player, Move) :-
 format('~w. ~w moves ~w~n~n', [Num,Player,Move]).

%%%%% show_winner/3

show_winner(quiet, _, _) :-
 !.

show_winner(verbose, 'Exhaust', Num) :-
 format('Game is drawn due to exhaustion after ~w moves!~n~n', [Num]).

show_winner(verbose, 'Draw', Num) :-
 format('Game is drawn after ~w moves!~n~n', [Num]).

show_winner(verbose, Winner, Num) :-
 format('~w wins after ~w moves!~n~n', [Winner,Num]).


