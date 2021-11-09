% Copyright 2016 Ramon Viñas, Marc Roig
%
% Licensed under the Apache License, Version 2.0 (the "License");
% you may not use this file except in compliance with the License.
% You may obtain a copy of the License at
%
%     http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS,
% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
% See the License for the specific language governing permissions and
% limitations under the License.

%%%%%%%%%%%%%%%%%%
%%%%% BOARD %%%%%%
%%%%%%%%%%%%%%%%%%
%Initialize empty board (matrix of dimensions [columns=7, rows=6]. This board representation will make gameplay easier than if we used [rows, columns])
initial(board([['-','-','-','-','-','-'],
	       ['-','-','-','-','-','-'],
	       ['-','-','-','-','-','-'],
	       ['-','-','-','-','-','-'],
	       ['-','-','-','-','-','-'],
	       ['-','-','-','-','-','-'],
	       ['-','-','-','-','-','-']])).


incremnt_score:-nb_getval(winCounter, C), CNew is C + 1, nb_setval(winCounter, CNew).
incremnt_draw:-nb_getval(drawCounter, C), CNew is C + 1, nb_setval(drawCounter, CNew).

%%%%%%%%%%%%%%%%%%
%%% SHOW BOARD %%%
%%%%%%%%%%%%%%%%%%
%show(X) shows board X
show(board(X)):- write('  A B C D E F G'), nl,
		 iShow(X,6).

show([Board | RestBoard]):-
	show(Board),
	show(RestBoard).


%show(X,N) shows lines [N .. 1] of board X
iShow(_,0).
iShow(X,N):- showLine(X,N,X2),
	     Ns is N-1,
	     iShow(X2,Ns).

%showLine(X,N,X2) writes N and shows first line of board X (first element of every column). X2 is X without the shown line.
showLine(X,N,X2):- write(N), write(' '),
		   iShowLine(X,X2), nl.

%iShowLine(X,X2) writes first element of every column. X2 is X without the shown line.
iShowLine([],_).
iShowLine([[X|X2]|XS],[X2|XS2]):- write(X), write(' '),
			          iShowLine(XS,XS2).

%%%%%%%%%%%%%%%%%%
%%%% GAMEPLAY %%%%
%%%%%%%%%%%%%%%%%%
% Initializes board and starts the game
connect4:- initial(X),
	   show(X),
	   nextMove('X',X), !.



stat_r(0):-
	nb_getval(winCounter, Win),
			nb_getval(drawCounter, Draw),
			nb_getval(totalCounter, Total),
			nb_getval(winCounter, Win),
			print('Total :'),
			print(Total),
			nl,
			print('Win :'),
			print(Win),
			nl,
			print('Draw :'),
			print(Draw),
			nl,
			print('Proba :'),
			Proba is Win/Total,
			print(Proba).

stat_r(Max):-
	initial(X),
	show(X),
	nextMove('X',X), !,
	NewMax is Max-1,
	stat_r(NewMax).

statistique(Max):-
			nb_setval(winCounter, 0),
			nb_setval(drawCounter, 0),
			nb_setval(totalCounter, Max),
			stat_r(Max).

			



%nextMove(J,X) J is the player that needs to move ('O' or 'X') and X is the board. Checks if the game has finished. If it hasn't finished, performs next move.
nextMove('X',X):- wins('O',X),
		  write('Machine wins!'),
		  incremnt_score.

nextMove('O',X):- wins('X',X),
		  write('You win!').
		  

nextMove(_,X):- full(X),
		write('Draw').
nextMove('X',X):- repeat, %repeats in case a column is full
		  readColumn(C),
		  play('X',C,X,X2), !,
		  show(X2),
		  nextMove('O',X2).
nextMove('O',X):-
	machine(X,X2),!,
		  show(X2),
		  nextMove('X',X2).

%play(X,P,T,T2) is satisfied if T2 is the board T after player X moves in column P
play(X,P,board(T),board(T2)):- append(I,[C|F],T),
			       length(I,P),
		               playColumn(X,C,C2),
			       append(I,[C2|F],T2).

%playColumn(X,C,C2) is satisfied if column C2 is column C after player X plays there
playColumn(X,['-'],[X]):- !. % last spot in column
playColumn(X,['-',A|AS],[X,A|AS]):- A \== ('-'), !. % play above someone's piece
playColumn(X,['-'|AS],['-'|AS2]):- playColumn(X,AS,AS2). % descend column

%wins(X,T) is satisfied if player X has won in board T
%check if there's a column in T with 4 connected pieces of player X
wins(X,board(T)):- append(_, [C|_], T), % check if there's a column...
	           append(_,[X,X,X,X|_],C). % ...which has 4 connected pieces of player X
%check if there's a row in T with 4 connected pieces of player X
wins(X,board(T)):- append(_,[C1,C2,C3,C4|_],T), 
		   append(I1,[X|_],C1), 
		   append(I2,[X|_],C2),
		   append(I3,[X|_],C3),
		   append(I4,[X|_],C4),
		   length(I1,M), length(I2,M), length(I3,M), length(I4,M). %...and every piece is in the same height
%check if there's a diagonal (type \) in T with 4 connected pieces of player X
wins(X,board(T)):- append(_,[C1,C2,C3,C4|_],T), 
		   append(I1,[X|_],C1), 
		   append(I2,[X|_],C2),
		   append(I3,[X|_],C3),
		   append(I4,[X|_],C4),
		   length(I1,M1), length(I2,M2), length(I3,M3), length(I4,M4),
		   M2 is M1+1, M3 is M2+1, M4 is M3+1. 
%check if there's a diagonal (type /) in T with 4 connected pieces of player X
wins(X,board(T)):- append(_,[C1,C2,C3,C4|_],T), 
		   append(I1,[X|_],C1), 
		   append(I2,[X|_],C2),
		   append(I3,[X|_],C3),
		   append(I4,[X|_],C4),
		   length(I1,M1), length(I2,M2), length(I3,M3), length(I4,M4),
		   M2 is M1-1, M3 is M2-1, M4 is M3-1. 

%full(T) is satisfied if there isn't any free spot ('-')
full(board(T)):- \+ (append(_,[C|_],T),
		 append(_,['-'|_],C)).

%%%%%%%%%%%%%%%%%%
%%% READ MOVES %%%
%%%%%%%%%%%%%%%%%%
%reads a column
readColumn(C):- nl, write('Column: '),
		repeat,
		get_char(L),
		associateColumn(L,C),
		col(C), !.

%associateColumn(L,C) column C is the column associated with char L
associateColumn(L,C):- atom_codes(L,[La|_]),
		       C is La - 65.

%associateChar(L, C) char L is the char associated with column C
associateChar(L, C):- Ln is 65+C,
		      atom_codes(L,[Ln]).

%valid columns
col(0).
col(1).
col(2).
col(3).
col(4).
col(5).
col(6).

%%%%%%%%%%%%%%%%%%
%%%%% MACHINE %%%%
%%%%%%%%%%%%%%%%%%
%
% source : https://github.com/jaunerc/minimax-prolog/blob/master/minimax.pl
% CompareMoves(+MinMax, +MoveA, +ValueA, +MoveB, +ValueB, -BetterMove, -BetterValue)
% Chooses the move with the higher value.

compare_moves('O', MoveA, ValueA, MoveB, ValueB, MoveA, ValueA) :-
	ValueA >= ValueB.
compare_moves('O', MoveA, ValueA, MoveB, ValueB, MoveB, ValueB) :-
	ValueA < ValueB.
compare_moves('X', MoveA, ValueA, MoveB, ValueB, MoveA, ValueA) :-
	ValueA =< ValueB.
compare_moves('X', MoveA, ValueA, MoveB, ValueB, MoveB, ValueB) :-
	ValueA > ValueB.

% change_max_min(+MinOrMax, TheOther)
% Changes the MinMax atom.
change_player('O', 'X').
change_player('X', 'O').

all_possible_moves(X,T1,Moves):- findall(I,play(X,L,T1,I),Moves).

% From https://stackoverflow.com/questions/27304954/prolog-sum-of-numbers-in-a-list
somme_liste([], 0).
somme_liste([Nb|Reste], Somme) :-
    somme_liste(Reste, TempSomme),
    Somme is Nb + TempSomme.

% -----------------------------------------------------------------------
% ICI C'EST DU CODE RECYCLE D'AU-DESSUS (EN PARTICULIER LA FONCTION "WINS") (Fait maison)
% -----------------------------------------------------------------------

% -------------- VERTICAL CHAIN -----------------------------------------
vertical_chain(X, board(T), 3):-
	append(_, [C|_], T),
	append(_,['-',X,X,X], C).
vertical_chain(X, board(T), 2) :-
	append(_, [C|_], T),
	append(_,['-',X,X], C).

horizontal_chain(X, board(T), 3):-
	append(_,[C1,C2,C3,C4|_],T), 
	append(I1,[X|_],C1), 
	append(I2,[X|_],C2),
	append(I3,[X|_],C3),
	append(I4,['-'|_],C4),
	length(I1,M), length(I2,M), length(I3,M), length(I4,M).
horizontal_chain(X, board(T), 2) :-
	append(_,[C1,C2,C3|_],T), 
	append(I1,[X|_],C1), 
	append(I2,[X|_],C2),
	append(I3,['-'|_],C3),
	length(I1,M), length(I2,M), length(I3,M).

horizontal_chain(X, board(T), 3):-
	append(_,[C1,C2,C3,C4|_],T), 
	append(I1,['-'|_],C1), 
	append(I2,[X|_],C2),
	append(I3,[X|_],C3),
	append(I4,[X|_],C4),
	length(I1,M), length(I2,M), length(I3,M), length(I4,M).
horizontal_chain(X, board(T), 2) :-
	append(_,[C1,C2,C3|_],T), 
	append(I1,['-'|_],C1), 
	append(I2,[X|_],C2),
	append(I3,[X|_],C3),
	length(I1,M), length(I2,M), length(I3,M).

diagonal_chain_1(X,board(T), 3):- append(_,[C1,C2,C3,C4|_],T), 
	append(I1,[X|_],C1), 
	append(I2,[X|_],C2),
	append(I3,[X|_],C3),
	append(I4,['-'|_],C4),
	length(I1,M1), length(I2,M2), length(I3,M3), length(I4,M4),
	M2 is M1-1, M3 is M2-1, M4 is M3-1. 
diagonal_chain_1(X,board(T), 2):- append(_,[C1,C2,C3|_],T), 
	append(I1,[X|_],C1), 
	append(I2,[X|_],C2),
	append(I3,['-'|_],C3),
	length(I1,M1), length(I2,M2), length(I3,M3),
	M2 is M1-1, M3 is M2-1. 

diagonal_chain_1(X,board(T), 3):- append(_,[C1,C2,C3,C4|_],T), 
	append(I1,['-'|_],C1), 
	append(I2,[X|_],C2),
	append(I3,[X|_],C3),
	append(I4,[X|_],C4),
	length(I1,M1), length(I2,M2), length(I3,M3), length(I4,M4),
	M2 is M1-1, M3 is M2-1, M4 is M3-1. 
diagonal_chain_1(X,board(T), 2):- append(_,[C1,C2,C3|_],T), 
	append(I1,['-'|_],C1), 
	append(I2,[X|_],C2),
	append(I3,[X|_],C3),
	length(I1,M1), length(I2,M2), length(I3,M3),
	M2 is M1-1, M3 is M2-1. 

diagonal_chain_2(X,board(T), 3):- append(_,[C1,C2,C3,C4|_],T), 
	append(I1,[X|_],C1), 
	append(I2,[X|_],C2),
	append(I3,[X|_],C3),
	append(I4,['-'|_],C4),
	length(I1,M1), length(I2,M2), length(I3,M3), length(I4,M4),
	M2 is M1+1, M3 is M2+1, M4 is M3+1. 
diagonal_chain_2(X,board(T), 2):- append(_,[C1,C2,C3|_],T), 
	append(I1,[X|_],C1), 
	append(I2,[X|_],C2),
	append(I3,['-'|_],C3),
	length(I1,M1), length(I2,M2), length(I3,M3),
	M2 is M1+1, M3 is M2+1. 

diagonal_chain_2(X,board(T), 3):- append(_,[C1,C2,C3,C4|_],T), 
	append(I1,['-'|_],C1), 
	append(I2,[X|_],C2),
	append(I3,[X|_],C3),
	append(I4,[X|_],C4),
	length(I1,M1), length(I2,M2), length(I3,M3), length(I4,M4),
	M2 is M1+1, M3 is M2+1, M4 is M3+1. 

diagonal_chain_2(X,board(T), 2):- append(_,[C1,C2,C3|_],T), 
	append(I1,['-'|_],C1), 
	append(I2,[X|_],C2),
	append(I3,[X|_],C3),
	length(I1,M1), length(I2,M2), length(I3,M3),
	M2 is M1+1, M3 is M2+1. 


% -----------------------------------------------------------------------
% ICI C'EST DU FAIT MAISON
% -----------------------------------------------------------------------

chain_score(Player, Board, Score) :-
	findall(I1, vertical_chain(Player, Board, I1), L1),
	somme_liste(L1, S1),
	findall(I2, horizontal_chain(Player, Board, I2), L2),
	somme_liste(L2, S2),
	findall(I3, diagonal_chain_1(Player, Board, I3), L3),
	somme_liste(L3, S3),
	findall(I4, diagonal_chain_2(Player, Board, I4), L4),
	somme_liste(L4, S4),
	S is S1 + S2 + S3 + S4,
	Score is S1 + S2 + S3 + S4.

eval_board(first,Player, Board, Value) :-
	chain_score(Player, Board, Value).
	%write_ln(Value).
	%Value is random().	

eval_board(rand,Player, Board, Value) :-
		random(Value).	
	


print_liste([]).
print_liste([A|Z]) :-
    show(A), nl, print_liste(Z).

best_move(_, [], _, _).



best_move(Method, Player, [Move | []], Move, Value) :-
	eval_board(Method, Player,Move, Value).
	%writeln(Value).

best_move(Method,Player, [Move | RestMoves], BestMove, BestValue) :-
	%trace(),
	eval_board(Player,Move, Value),
	best_move(Player, RestMoves, CurrentBestM, CurrentBestV),
	compare_moves(Player,Move, Value, CurrentBestM, CurrentBestV, BestMove, BestValue).
	



minimax(Player, AllMoves, BestMove, BestValue, 1) :-
	best_move(Player, AllMoves, BestMove, BestValue).


/*
minimax('X', [Move | RestMoves], BestMove, BestValue, CurrentDepth) :-
	wins('X',Move),
	BestMove = Move,
	BestValue is -100000.
	%show(Move),
	%print('X'),
	%writeln(' Wins !').
minimax('O', [Move | RestMoves], BestMove, BestValue, CurrentDepth) :-
	wins('O',Move),
	BestMove = Move,
	BestValue is 100000.
	%show(Move),
	%print('O'),
	%writeln(' Wins !').
*/
minimax(Player, [Move | []], Move, BestValue, CurrentDepth) :-
	change_player(Player, Other),
	all_possible_moves(Other, Move, AllMoves),
	NewDepth is CurrentDepth - 1,
	minimax(Other, AllMoves, _, BestValue, NewDepth).

minimax(Player, [Move | RestMoves], BestMove, BestValue, CurrentDepth) :-
	minimax(Player, RestMoves, CurrentBestM, CurrentBestV, CurrentDepth),
	change_player(Player, Other),
	all_possible_moves(Other, Move, AllMoves),
	NewDepth is CurrentDepth - 1,
	minimax(Other, AllMoves, _, PossibleBestV, NewDepth),
	compare_moves(Player,Move, PossibleBestV, CurrentBestM, CurrentBestV, BestMove, BestValue).

% minimax(+Board, -BestMove)
% Matches the next move based on the current board.
machine(Board, BestMove) :-
        all_possible_moves('O', Board, AllMoves),
	minimax('O', AllMoves, BestMove, BestValue, 4),

	writeln('Value  :'),
	%show(BestMove),
	write_ln(BestValue).


