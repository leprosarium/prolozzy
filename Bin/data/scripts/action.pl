:- module(action, [action/0]).




% This is the main latent action function requested by the action handler.
% First it tries "pickup" on a pickable object, if found.
% If not, it tries "action" on an action object, if found.
% If not, it opens the inventory and let you use/drop an item.
% Called by HandlerAction(), see PickupObject(), ActionObject(), UseObject().

action :-
	findPickupObject(Idx)
	->  pickupObject(Idx)
	;   (   findActionObject(Idx2)
	    ->  actionObject(Idx2)
	    ;   (update:register(ui, pop(InvIdx, action:useObject(InvIdx))),
		menu:openDialogInventory)).


findObject(Kind, Idx) :-
	core:objPresentCount(Pcount),
	core:dl(pcount(Pcount)),
	Pidx is Pcount - 1, !,
	findObject(Kind, Pidx, Idx).
findObject(_, I, -1) :- I < 0, !, fail.
findObject(Kind, I, Idx) :-
	core:objPresentIdx(I, OIdx),
	I2 is I - 1,
	(   obj:disable(OIdx)
	->  findObject(Kind, I2, Idx)
	;   (   testCall(Kind, OIdx),
	        player:touchObjectInRoom(OIdx)
	    ->	Idx = OIdx
	    ;	findObject(Kind, I2, Idx))).

testCall(Module:Kind, Idx) :-
	F1 =.. [Kind, Idx],
	Functor =.. [:, Module, F1], !,
	call(Functor).
testCall(Kind, Idx) :-
	Functor =.. [Kind, Idx],
	call(Functor).

% OUT: int; object index or -1 if not found
% Finds the first pickable object that the player stands in front of.
% Called by Action(), see PlayerTouchObject().
findPickupObject(Idx) :-
	findObject(util:objIsPickup, Idx).

% OUT: int; object index or -1 if not found
% Finds the first action object that the player stands in front of.
% Called by Action(), see PlayerTouchObject().
findActionObject(Idx) :-
	findObject(util:objIsAction, Idx).

% IN: int; idx; object index
% Picks up an object considering the object class (item, coin, food, life).
% If available, it calls the pickup callback "PickupObject_ID" insteed.
% Called by Action(), see DoPickupObject().
pickupObject(Idx) :-
	obj:id(Idx, Id),
	catch(game:pickupObject(Id), _, doPickupObject(Idx)).

% IN: int; idx; object index
% Does pick up an object considering the object class (item, coin, food, life),
% without calling the pickup callback, so it may be used from it.
% Called by PickupObject().
doPickupObject(Idx) :-
	obj:class(Idx, ClassIdx),
	def:class(Class, ClassIdx),
	doPickupObject(Idx, Class).
doPickupObject(Idx, item) :- % items are to be picked up in the inventory
	(   \+ inventory:add(Idx)
	->  dialog:openMessage('YOUR HANDS\nARE FULL!')
	;   core:samplePlay(beep1),
	    obj:disable(Idx, 1),
	    update:register(ui, pop(InvIdx, action:useObject(InvIdx))),
	    menu:openDialogInventory(exit)).

doPickupObject(Idx, coin) :- % coins are to be collected
	player:coins(Coins),
	Coins1 is Coins + 1,
	obj:disable(Idx, 1), % make disabled (picked up)
	player:coins(Coins1), % store coins counter
	core:samplePlay(coin),
	gamedef:maxCoins(Max),
	(   Coins1 = Max
	->  Text = 'YOU HAVE FOUND\nALL THE COINS'
	;   Text = 'YOU HAVE FOUND\nA COIN'),
	dialog:openMessage(Text).

doPickupObject(Idx, food) :- % food gives energy
	player:life(Life),
	(   Life >= 100
	->  dialog:openMessage('MAYBE LATER...')
	;   obj:disable(Idx, 1), % make disabled (picked up)
	    core:samplePlay(coin),
	    playEat(0, Life)).

doPickupObject(Idx, life) :- % gives one credit
	player:credits(Credits),
	gamedef:maxCredits(Max),
	(   Credits >= Max
	->  dialog:openMessage('MAYBE LATER...')
	;   obj:disable(Idx, 1), % make disabled (picked up)
	    core:samplePlay(coin),
	    Credits1 is Credits + 1,
	    player:credits(Credits1),
	    dialog:openMessage('YOU HAVE FOUND\nONE LIFE')).
	% users can also create other classes ...


useObject(-1). % Do nothing.


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



% IN: int; idx; object index
% Use an object from the inventory.
% If player stands in front of an action object (action class), the "UseObject_ID" callback is called.
% If not, the item is simply dropped down.
% Called by Action() and ActionObject(), see DropObject().

useObject(Idx) :-
	findActionObject(Idx2) % find action object with UseObject_ID function
	-> (obj:id(Idx2, Id),
	   catch(game:useObject(Id, Idx), _, true))
	;  dropObject(Idx).	% just drop it

% IN: int; idx; object index
% Drops an object from the inventory.
% The object is placed on the player's current position.
% If available, it calls the drop object callback "DropObject_ID" insteed.
% Do not call it back, from inside the drop object callback!
% Called by UseObject(), see DoDropObject().
dropObject(Idx) :-
	(   \+ player:safe; \+ player:stable)
	->  dialog:openMessage('YOUR CAN''T DROP\nTHIS NOW!')
	;   obj:id(Idx, Id),
	catch(game:dropObject(Id), _, doDropObject(Idx)).

% IN: int; idx; object index
% Does drop the object, enableing and placing it on the player's current position,
% without calling the drop object callback, so it can be used inside this callback.
% Called by DropObject().
doDropObject(Idx) :-
	core:samplePlay(beep2),
	inventory:sub(Idx),
	obj:disable(Idx, 0),
	player:pos(Px, Py),
	player:h(Ph),
	obj:w(Idx, W),
	obj:h(Idx, H),
	X is Px - W // 2,
	Y is Py + Ph // 2 - H,
	obj:x(Idx, X),
	obj:y(Idx, Y),
	core:objPresent(Idx). % force it to be present in current room

% IN: int; idx; object index
% Action performed on the object the player stands in front of.
% If available, it calls the action callback "ActionObject_ID".
% If not, opens inventory and allows use /drop of the selected item.
% Called by Action(), see UseObject().

actionObject( Idx ) :-
	obj:id(Idx, Id),
	catch(game:actionObject(Id), _, false);
	update:register(ui, pop(InvIdx,action:useObject(InvIdx))),
	menu:openDialogInventory.














