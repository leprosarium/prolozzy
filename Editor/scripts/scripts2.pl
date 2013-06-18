:-module(scripts2, [checkTile/0,
		    checkId/0,
		    checkOverlapping/0,
		    checkDynamicBrushId/0,
		    checkStaticBrushId/0,
		    countRooms/0]).


:-use_module(gui, [waitCall/1]).

checkTile :-
	core:dl('Check Brush Tile:'),
	scripts:forallBrush(scripts2:checkTile(_, _), Select),
	map:setSelect(Select),
	(   Select == 0
	->  Msg = 'All brush tiles are valid.'
	;   format(string(Msg), 'There are ~d missing tiles.', [Select])
	),
	gui:msgBoxOk('Message', Msg, icon_info).

checkTile(Br, Sel) :-
	brush:getTile(Br, Tile),
	(   edi:tileFind(Tile, _IDX)
	->  Sel = 0
	;   Sel = 1,
	    format(string(Msg), 'missing tile ~d on ~p', [Tile, Br]),
	    core:dl(Msg)),
	brush:setSelect(Br, Sel).

checkId :-
	core:dl('Check Brush Id:'),
	waitCall((checkIdCollect(Ids),
		  checkIdProcess(Ids, Count))),
	map:setSelect(Count),
	(   Count == 0
	->  gui:msgBoxOk('Message', 'No dulicate ids', icon_info)
	;   format(string(Msg), '~d duplicate ids', [Count]),
	    gui:msgBoxOk('Warning', Msg, icon_warning )).


checkIdCollect(Ids) :-
	findall(B, map:brush(B), Brs),
	empty_assoc(Emp),
	checkIdCollect(Brs, Emp, Ids).

checkIdCollect([], Ids, Ids).
checkIdCollect([Br|Brs], CurIds, Ids) :-
	brush:getID(Br, Id),
	(   Id == 0
	->  NewIds = CurIds
	;   brush:setSelect(Br, 0),
	    (get_assoc(Id, CurIds, Idxs); Idxs = []),
	    put_assoc(Id, CurIds, [Br|Idxs], NewIds)
	),
	checkIdCollect(Brs, NewIds, Ids).

checkIdProcess(Ids, Count) :-
	findall(C, (gen_assoc(Id, Ids, [Idx1, Idx2|Idxs]), checkIdProcess(Id, [Idx1, Idx2|Idxs], C)), CC),
	sum_list(CC, Count).

checkIdProcess(Id, Brs, C) :-
	length(Brs, C),
	core:dl( duplicate(Id, Brs)),
	forall(member(Br, Brs), brush:setSelect(Br, 1)).


checkOverlapping :-
	core:dl('Check Brush Overlapping:'),
	waitCall(
		(checkOverCollect(Ids),
		 checkOverProcess(Ids, Count))),
	map:setSelect(Count),
	(   Count == 0
	->  gui:msgBoxOk('Message', 'No overlapping brushes found.', icon_info)
	;   format(string(Msg), '~d overlapping brushes found.', [Count]),
	    gui:msgBoxOk('Warning', Msg, icon_warning )).


checkOverCollect(Ids) :-
	findall(B, map:brush(B), Brs),
	empty_assoc(Emp),
	checkOverCollect(Brs, Emp, Ids).

checkOverCollect([], Ids, Ids).
checkOverCollect([Br|Brs], CurIds, Ids) :-
	brush:getX(Br, X),
	brush:getY(Br, Y),
	brush:getW(Br, W),
	brush:getH(Br, H),
	brush:getTile(Br, Tile),
	brush:getMapX1(Br, MX1),
	brush:getMapY1(Br, MY1),
	brush:getMapX2(Br, MX2),
	brush:getMapY2(Br, MY2),
	Key = b(pos(X, Y), size(W, H), tile(Tile), map(MX1, MY1, MX2, MY2)),
	brush:setSelect(Br, 0),
	(get_assoc(Key, CurIds, Idxs); Idxs = []),
	put_assoc(Key, CurIds, [Br|Idxs], NewIds),
	checkOverCollect(Brs, NewIds, Ids).

checkOverProcess(Ids, Count) :-
	findall(C, (gen_assoc(Key, Ids, [Idx1, Idx2|Idxs]), checkOverProcess(Key, [Idx1, Idx2|Idxs], C)), CC),
	sum_list(CC, Count).

checkOverProcess(Key, Brs, C) :-
	length(Brs, C),
	core:dl( overlap(Key, Brs)),
	forall(member(Br, Brs), brush:setSelect(Br, 1)).

checkDynamicBrushId :-
	core:dl('Check Dynamic Brush Id:'),
	scripts:forallBrush(scripts2:checkDynamicBrushId(_, _), Count),
	map:setSelect(Count),
	(   Count =\= 0
	->  format(string(Msg), 'There are ~d dynamic brushes without ids.\nThey will not be accessible in script.\nSet them ids or make them static brushes.', [Count]),
	    gui:msgBoxOk('Warning', Msg, icon_warning)
	;   gui:msgBoxOk('Info', 'There are no dynamic brushes without ids.\nThats good.', icon_info)).

checkDynamicBrushId(Br, C) :-
	def:brushType(dynamic, Type),
	(   brush:getType(Br, Type),
	    brush:getID(Br, 0)
	->  C = 1
	;   C = 0),
	brush:setSelect(Br, C).


checkStaticBrushId :-
	core:dl('Check Static Brush Id:'),
	scripts:forallBrush(scripts2:checkStaticBrushId(_, _), Count),
	map:setSelect(Count),
	(   Count =\= 0
	->  format(string(Msg), 'There are ~d static brushes with ids.\nMake sure they need ids indeed.', [Count]),
	    gui:msgBoxOk('Warning', Msg, icon_warning)
	;   gui:msgBoxOk('Info', 'There are no static brushes with ids.', icon_info)).

checkStaticBrushId(Br, C) :-
	def:brushType(static, Type),
	(   brush:getType(Br, Type), brush:getID(Br, Id), Id =\= 0
	->  C = 1
	;   C = 0),
	brush:setSelect(Br, C).

countRooms :-
	gui:msgBox('Question', 'Count rooms with brush content.\n\nWARNING\nSave your map BEFORE this operation!\nGuide green brushes are going to be inserted\nto help you verify the counting.\nProceed?\n', icon_question, [btn('YES', scripts2:countRoomsApply), btn('NO', true)]).

countRoomsApply :-
	map:getRoomW(RW),
	map:getRoomH(RH),
	Params = params(32, RW, RH),
	waitCall((
	countRoomsCollect(Params, Rooms),
	countRoomsMark(Rooms, Count, Params))),
	format(string(Msg), '~d rooms found with content.\nMap has been altered with guide green brushes.\nCheck the counting, but DON\'T save the map.', [Count]),
	gui:msgBoxOk('Message', Msg, icon_info),
	map:repartition,
	map:refresh.


countRoomsCollect(Params, Rooms) :-
	empty_nb_set(Rooms),
	forall(map:brush(B), countRoomsCollect(B, Params, Rooms)).
countRoomsCollect(B, Params, Rooms) :-
	brush:getX(B, X),
	brush:getY(B, Y),
	brush:getW(B, W),
	brush:getH(B, H),
	X2 is X + W,
	Y2 is Y + H,
	countRoomsCollectAdd(X, Y, Params, Rooms),
	countRoomsCollectAdd(X2, Y2, Params, Rooms).

countRoomsCollectAdd(X, Y, params(C, RW, RH), Rooms) :-
	DX is X mod RW,
	DY is Y mod RH,
	DX > C,
	DY > C,
	DX < RW - C,
	DY < RH - C,
	XX is X // RW,
	YY is Y // RH,
	add_nb_set(p(XX, YY), Rooms);true.

countRoomsMark(Rooms, Count, Params) :-
	size_nb_set(Rooms, Count),
	forall(gen_nb_set(Rooms, p(X, Y)), countRoomsMarkAdd(X, Y, Params)).
countRoomsMarkAdd(X, Y, params(C, RW, RH)) :-
	XX is X * RW + C,
	YY is Y * RH + C,
	W is RW - C - C,
	H is RH - C - C,
	def:shader(blend, Shader),
	map:brushNew(B),
	brush:set(B, [layer=7, x=XX, y=YY, w=W, h=H, tile=0, x2=8, y2=8, color=0x8000ff00, shader=Shader, id=1234567]),
	brush:setSelect(B, 1).











