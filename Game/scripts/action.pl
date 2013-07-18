:- module(action, [action/0]).




% First it tries "pickup" on a pickable object, if found.
% If not, it tries "action" on an action object, if found.
% If not, it opens the inventory and let you use/drop an item.

action :- findPickupObject(Id), !, pickupObject(Id).
action :- findActionObject(Id), !, actionObject(Id).
action :- !, update:regPop(ui, action:useObject), menu:openDialogInventory.


findObject(Kind, Id) :-
	core:objPresent(Obj),
	\+ brush:disabled(Obj),
	testCall(Kind, Obj),
	player:touchObjectInRoom(Obj),
	brush:getID(Obj, Id).

testCall(Module:Kind, Id) :-
	F1 =.. [Kind, Id],
	Functor =.. [:, Module, F1], !,
	call(Functor).
testCall(Kind, Id) :-
	Functor =.. [Kind, Id],
	call(Functor).

% Finds the first pickable object that the player stands in front of.
findPickupObject(Id) :-
	findObject(util:objIsPickup, Id).

% Finds the first action object that the player stands in front of.
findActionObject(Id) :-
	findObject(util:objIsAction, Id).

% Picks up an object considering the object class (item, coin, food, life).
% If available, it calls the pickup callback game:pickupObject/1 insteed.
pickupObject(Id) :-
	catch(game:pickupObject(Id), _, doPickupObject(Id)).

% Does pick up an object considering the object class (item, coin, food, life),
% without calling the pickup callback, so it may be used from it.
doPickupObject(Id) :-
	brush:find(Id, Obj),
	brush:get(Obj, class, Class),
	doPickupObject(Obj, Class).
doPickupObject(Obj, item) :- % items are to be picked up in the inventory
	brush:getID(Obj, Id),
	(   \+ inventory:add(Id)
	->  dialog:openMessage('YOUR HANDS\nARE FULL!')
	;   sample:play(beep1),
	    brush:disable(Obj),
	    update:regPop(ui, action:useObject),
	    menu:openDialogInventory(exit)).

doPickupObject(Obj, coin) :- % coins are to be collected
	player:coins(Coins),
	Coins1 is Coins + 1,
	brush:disable(Obj), % make disabled (picked up)
	player:coins(Coins1), % store coins counter
	sample:play(coin),
	gamedef:maxCoins(Max),
	(   Coins1 = Max
	->  Text = 'YOU HAVE FOUND\nALL THE COINS'
	;   Text = 'YOU HAVE FOUND\nA COIN'),
	dialog:openMessage(Text).

doPickupObject(Obj, food) :- % food gives energy
	player:life(Life),
	(   Life >= 100
	->  dialog:openMessage('MAYBE LATER...')
	;   brush:disable(Obj), % make disabled (picked up)
	    sample:play(coin),
	    playEat(0, Life)).

doPickupObject(Obj, life) :- % gives one credit
	player:credits(Credits),
	gamedef:maxCredits(Max),
	(   Credits >= Max
	->  dialog:openMessage('MAYBE LATER...')
	;   brush:disable(Obj), % make disabled (picked up)
	    sample:play(coin),
	    Credits1 is Credits + 1,
	    player:credits(Credits1),
	    dialog:openMessage('YOU HAVE FOUND\nONE LIFE')).
	% users can also create other classes ...


playEat(Count, Life) :-
	Life >= 100,
	Count >= 10,
	player:enterIdle, !.
playEat(Count, Life) :-
	player:life(Life),
	Count1 is Count + 1,
	Life1 is Life + 10;
	(   Life1 > 100
	->  Life2 = 100
	;   Life2 = Life1),
	update:register(player, action:playEat(Count1, Life2)),
	player:playAnimFrames(eat, [0, 1]).



% Use an object from the inventory.
% If player stands in front of an action object (action class), the
% game:useObject/2 callback is called. If not, the item is simply
% dropped down.

useObject(-1) :- !.
useObject(Id) :-
	findActionObject(Id2)
	-> catch(game:useObject(Id2, Id), _, true)
	;  dropObject(Id).

% Drops an object from the inventory.
% The object is placed on the player's current position.
% If available, it calls the drop object callback game:dropObject/1 insteed.
dropObject(Id) :-
	(   \+ player:safe; \+ player:stable)
	->  dialog:openMessage('YOUR CAN''T DROP\nTHIS NOW!')
	;   catch(game:dropObject(Id), _, doDropObject(Id)).

% Does drop the object, enableing and placing it on the player's current position,
% without calling the drop object callback, so it can be used inside this callback.
doDropObject(Id) :-
	sample:play(beep2),
	brush:find(Id, Obj),
	inventory:sub(Id),
	brush:enable(Obj),
	player:pos(Px, Py),
	player:h(Ph),
	brush:getW(Obj, W),
	brush:getH(Obj, H),
	X is Px - W // 2,
	Y is Py + Ph // 2 - H,
	brush:setX(Obj, X),
	brush:setY(Obj, Y),
	core:setObjPresent(Obj). % force it to be present in current room

% Action performed on the object the player stands in front of.
% If available, it calls the action callback game:actionObject/1.
% If not, opens inventory and allows use /drop of the selected item.
actionObject(Id) :-
	catch(game:actionObject(Id), _, false);
	update:regPop(ui, action:useObject),
	menu:openDialogInventory.














