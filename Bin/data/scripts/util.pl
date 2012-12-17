:- module(util, [useUpForJump/0,
		 updateShakeAndRumble/0,
		 updateShake/0,
		 updateRumble/0,
		 doRumble/1,
		 doShake/1,
		 getKey/1,
		 getKeyHit/1,
		 setKey/1,
		 resetKey/1,
		 clearKeys/0,
		 objIsPickup/1,
		 objIsAction/1,
		 isUpdate/1,
		 member/2,
		 nth0/3,
		 append/3,
		 musicStore/0,
		 musicRestore/0]).

:-use_module(def).

member(El, [H|T]) :-
    member_(T, El, H).

member_(_, El, El).
member_([H|T], El, _) :-
    member_(T, El, H).


%%	append(?List1, ?List2, ?List1AndList2)
%
%	List1AndList2 is the concatination of List1 and List2

append([], L, L).
append([H|T], L, [H|R]) :-
	append(T, L, R).

% IN: int; delay; number of frames
% OUT: int; 0/1
% Test if it's time for an object's update, once each delay frames.
% Usually delay is object's O_DELAY.
isUpdate(0) :- !.

isUpdate(Delay) :-
	game:frame(Frame),
	Frame mod Delay =:= 0.

useUpForJumpKeys(Up, Jump) :-
	game:keys(Keys),
	keyPressed(Keys, Up),
	NewKeys is Keys \/ Jump,
	game:setKeys(NewKeys).
useUpForJumpKeys(_, _).

useUpForJumpKeysHit(Up, Jump) :-
	game:keysHit(Keys),
	keyPressed(Keys, Up),
	NewKeys is Keys \/ Jump,
	game:setKeysHit(NewKeys).
useUpForJumpKeysHit(_, _).

keyPressed(Keys, Mask) :-
	Keys /\ Mask =\= 0.

useUpForJump :-
	key(jump, Jump),
	key(up, Up),
	useUpForJumpKeys(Up, Jump),
	useUpForJumpKeysHit(Up, Jump).


updateShakeAndRumble :-
	updateShake,
	updateRumble.

stopShake :-
	game:setShake(0, 0).

applyShake(MagnitudeX, MagnitudeY, FrequencyX, FrequencyY) :-
	core:ticktime(T),
	SX is integer(cos(T / FrequencyX) * (MagnitudeX + 0.1)),
	SY is integer(sin(T / FrequencyY) * (MagnitudeY + 0.1)),
	game:setShake(SX, SY).

updateShake :-
	game:shake(Shake),
	Shake > 0,
	NewShake is Shake - 1,
	(NewShake > 0 -> applyShake(4, 2, 30, 10); stopShake),
	game:shake(NewShake).
updateShake.

rumbleMagnitude(Rumble, 100) :- Rumble > 0.
rumbleMagnitude(_, 0).

updateRumble :-
	game:rumble(Rumble),
	Rumble > 0,
	NewRumble is Rumble - 1,
	game:setFFPeriod(100),
	rumbleMagnitude(NewRumble, Magnitude),
	game:setFFMagnitude(Magnitude),
	game:rumble(NewRumble).
updateRumble.

doRumble(Frames) :-
	game:rumble(Rumble),
	Frames > Rumble,
	game:rumble(Frames).
doRumble(_).

doShake(Frames) :-
	game:shake(Shake),
	Frames > Shake,
	game:shake(Frames).
doShake(_).


% IN: int; key; key index [0..6]
% OUT: int; 0/1
% Tests if a key is pressed down.
% KEY_LEFT, KEY_RIGHT, KEY_UP, KEY_DOWN, KEY_JUMP, KEY_ACTION, KEY_MENU

getKey(Key) :-
	getKey(Key, _).
getKey(Key, Keys) :-
	key(Key, Mask),
	game:keys(Keys),
	keyPressed(Keys, Mask).

% IN: int; key; key index [0..6]
% OUT: int; 0/1
% Tests if a key has just been pressed (hit).
% The hit test is compares the key state with the state from the previous game cycle.
% KEY_LEFT, KEY_RIGHT, KEY_UP, KEY_DOWN, KEY_JUMP, KEY_ACTION, KEY_MENU

getKeyHit(Key) :-
	key(Key, Mask),
	game:keysHit(Keys),
	keyPressed(Keys, Mask).



% IN: int; key; key index [0..6]
% Overwrites a key value.
% Can be used to simulate player's input.
% KEY_LEFT, KEY_RIGHT, KEY_UP, KEY_DOWN, KEY_JUMP, KEY_ACTION, KEY_MENU

setKey(Key) :-
	setKey(Key, Keys \/ Mask, Keys, Mask).
resetKey(Key) :-
	setKey(Key, Keys /\ \Mask, Keys, Mask).

setKey(Key, Exp, Keys, Mask) :-
	key(Key, Mask),
	game:keys(Keys),
	NewKeys is Exp,
	game:setKeys(NewKeys), !.

% Clears key values.
% Can be used after closing menus, to prevent them from reopening.
clearKeys :-
	game:setKeys(0),
	game:setKeysHit(0).

% IN: int; idx; object index
% OUT: int; 0/1
% Tests if object is pickable, by class.
objIsPickup(Idx) :-
	obj:class(Idx, Class),
	core:dl(objIsPickup(Idx, Class)),
	(   def:class(item, Class);
	def:class(coin, Class);
	def:class(food, Class);
	def:class(life, Class)).

% IN: int; idx; object index
% OUT: int; 0/1
% Tests if object is action, by class.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%/
objIsAction(Idx) :-
	obj:class(Idx, Class),
	def:class(action, Class).





%%	nth0(?Index, ?List, ?Elem)
%
%	True if Elem is the Index'th element of List. Counting starts at
%	0.  This  is  a  faster  version   of  the  original  SWI-Prolog
%	predicate.

nth0(Index, List, Elem) :-
        integer(Index), !,
        Index >= 0,
        nth0_det(Index, List, Elem).    % take nth deterministically
nth0(Index, List, Elem) :-
        var(Index), !,
        nth_gen(List, Elem, 0, Index).  % match

nth0_det(0, [Elem|_], Elem) :- !.
nth0_det(1, [_,Elem|_], Elem) :- !.
nth0_det(2, [_,_,Elem|_], Elem) :- !.
nth0_det(3, [_,_,_,Elem|_], Elem) :- !.
nth0_det(4, [_,_,_,_,Elem|_], Elem) :- !.
nth0_det(5, [_,_,_,_,_,Elem|_], Elem) :- !.
nth0_det(N, [_,_,_,_,_,_   |Tail], Elem) :-
        M is N - 6,
	M >= 0,
        nth0_det(M, Tail, Elem).

nth_gen([Elem|_], Elem, Base, Base).
nth_gen([_|Tail], Elem, N, Base) :-
        succ(N, M),
        nth_gen(Tail, Elem, M, Base).


% Stores music id and music position to use when player dies and gets respawned
musicStore :-
	core:musicPlaying(Safe),
	core:musicPosition(Pos),
	game:musicSafe(Safe),
	game:musicPosSafe(Pos).

% Restores and play music, used when player gets respawned
musicRestore :-
	core:musicStop,
	game:musicSafe(Safe),
	game:musicPosSafe(Pos),
	core:musicPlay(Safe, Pos).













