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
		 musicStore/0,
		 musicRestore/0]).

:-use_module(def).

% +delay; number of frames
% Test if it's time for an object's update, once each delay frames.
% Usually delay is object's delay.
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

updateRumble :-
	game:rumble(Rumble),
	Rumble > 0,
	NewRumble is Rumble - 1,
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
objIsPickup(Obj) :-
	brush:get(Obj, class, Class),
	member(Class, [item, coin, food, life]).

% IN: int; idx; object index
% OUT: int; 0/1
% Tests if object is action, by class.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%/
objIsAction(Obj) :-
	brush:get(Obj, class, action).

% Stores music id and music position to use when player dies and gets respawned
musicStore :-
	music:playing(Safe),
	music:position(Pos),
	game:musicSafe(Safe),
	game:musicPosSafe(Pos).

% Restores and play music, used when player gets respawned
musicRestore :-
	music:stop,
	game:musicSafe(Safe),
	game:musicPosSafe(Pos),
	music:play(Safe, Pos).















