:-module(scripts2, [checkTile/0,
		    checkId/0,
		    checkOverlapping/0,
		    checkDynamicBrushId/0,
		    checkStaticBrushId/0,
		    countRooms/0]).


:-use_module(gui, [waitCall/1]).

checkTile :-
	core:dl('Check Brush Tile:'),
	map:brushCount(BC),
	waitCall(checkTile(0, BC, 0, Select)),
	map:setSelect(Select),
	(   Select == 0
	->  Msg = 'All brush tiles are valid.'
	;   format(string(Msg), 'There are ~d missing tiles.', [Select])
	),
	gui:msgBoxOk('Message', Msg, icon_info).

checkTile(BC, BC, S, S).
checkTile(Br, BC, C, S) :-
	map:brushGetTile(Br, Tile),
	(   edi:tileFind(Tile, _IDX)
	->  Sel = 0
	;   Sel = 1,
	    format(string(Msg), 'missing tile ~d on brush #~d', [Tile, Br]),
	    core:dl(Msg)),
	map:brushSetSelect(Br, Sel),
	CN is C + Sel,
	Br2 is Br + 1,
	checkTile(Br2, BC, CN, S).

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
	map:brushCount(BC),
	empty_assoc(Emp),
	checkIdCollect(0, BC, Emp, Ids).

checkIdCollect(BC, BC, Ids, Ids).
checkIdCollect(Br, BC, CurIds, Ids) :-
	map:brushGetID(Br, Id),
	(   Id == 0
	->  NewIds = CurIds
	;   map:brushSetSelect(Br, 0),
	    (get_assoc(Id, CurIds, Idxs); Idxs = []),
	    put_assoc(Id, CurIds, [Br|Idxs], NewIds)
	),
	Br2 is Br + 1,
	checkIdCollect(Br2, BC, NewIds, Ids).

checkIdProcess(Ids, Count) :-
	findall(C, (gen_assoc(Id, Ids, [Idx1, Idx2|Idxs]), checkIdProcess(Id, [Idx1, Idx2|Idxs], C)), CC),
	sum_list(CC, Count).

checkIdProcess(Id, Idxs, C) :-
	length(Idxs, C),
	core:dl( duplicate(Id, Idxs)),
	forall(member(Idx, Idxs), map:brushSetSelect(Idx, 1)).


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
	map:brushCount(BC),
	empty_assoc(Emp),
	checkOverCollect(0, BC, Emp, Ids).

checkOverCollect(BC, BC, Ids, Ids).
checkOverCollect(Br, BC, CurIds, Ids) :-
	map:brushGetX(Br, X),
	map:brushGetY(Br, Y),
	map:brushGetW(Br, W),
	map:brushGetH(Br, H),
	map:brushGetTile(Br, Tile),
	map:brushGetMapX1(Br, MX1),
	map:brushGetMapY1(Br, MY1),
	map:brushGetMapX2(Br, MX2),
	map:brushGetMapY2(Br, MY2),
	Key = b(X, Y, W, H, Tile, MX1, MY1, MX2, MY2),
	map:brushSetSelect(Br, 0),
	(get_assoc(Key, CurIds, Idxs); Idxs = []),
	put_assoc(Key, CurIds, [Br|Idxs], NewIds),
	Br2 is Br + 1,
	checkOverCollect(Br2, BC, NewIds, Ids).

checkOverProcess(Ids, Count) :-
	findall(C, (gen_assoc(Key, Ids, [Idx1, Idx2|Idxs]), checkOverProcess(Key, [Idx1, Idx2|Idxs], C)), CC),
	sum_list(CC, Count).

checkOverProcess(Key, Idxs, C) :-
	length(Idxs, C),
	core:dl( overlap(Key, Idxs)),
	forall(member(Idx, Idxs), map:brushSetSelect(Idx, 1)).

checkDynamicBrushId :-
	core:dl('Check Dynamic Brush Id:'),
	map:brushCount(BC),
	waitCall(checkDynamicBrushId(0, BC, 0, Count)),
	map:setSelect(Count),
	(   Count =\= 0
	->  format(string(Msg), 'There are ~d dynamic brushes without ids.\nThey will not be accessible in script.\nSet them ids or make them static brushes.', [Count]),
	    gui:msgBoxOk('Warning', Msg, icon_warning)
	;   gui:msgBoxOk('Info', 'There are no dynamic brushes without ids.\nThats good.', icon_info)).

checkDynamicBrushId(BC, BC, Count, Count).
checkDynamicBrushId(Br, BC, Cnt, Total) :-
	map:brushSetSelect(Br, 0),
	def:brushType(dynamic, Type),
	(   map:brushGetType(Br, Type),	map:brushGetID(Br, 0)
	->  map:brushSetSelect(Br, 1),
	    C = 1
	;   C = 0),
	Br2 is Br + 1,
	Cnt2 is Cnt + C,
	checkDynamicBrushId(Br2, BC, Cnt2, Total).



checkStaticBrushId :-
	core:dl('Check Static Brush Id:'),
	map:brushCount(BC),
	waitCall(checkStaticBrushId(0, BC, 0, Count)),
	map:setSelect(Count),
	(   Count =\= 0
	->  format(string(Msg), 'There are ~d static brushes with ids.\nMake sure they need ids indeed.', [Count]),
	    gui:msgBoxOk('Warning', Msg, icon_warning)
	;   gui:msgBoxOk('Info', 'There are no static brushes with ids.', icon_info)).

checkStaticBrushId(BC, BC, Count, Count).
checkStaticBrushId(Br, BC, Cnt, Total) :-
	map:brushSetSelect(Br, 0),
	def:brushType(static, Type),
	(   map:brushGetType(Br, Type), map:brushGetID(Br, Id), Id =\= 0
	->  map:brushSetSelect(Br, 1),
	    C = 1
	;   C = 0),
	Br2 is Br + 1,
	Cnt2 is Cnt + C,
	checkStaticBrushId(Br2, BC, Cnt2, Total).

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
	map:brushCount(BC),
	empty_nb_set(Rooms),
	countRoomsCollect(0, BC, Params, Rooms).

countRoomsCollect(BC, BC, _, _).
countRoomsCollect(Br, BC, Params, Rooms) :-
	countRoomsCollect(Br, Params, Rooms),
	Br2 is Br + 1,
	countRoomsCollect(Br2, BC, Params, Rooms).

countRoomsCollect(B, Params, Rooms) :-
	map:brushGetX(B, X),
	map:brushGetY(B, Y),
	map:brushGetW(B, W),
	map:brushGetH(B, H),
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
	add_nb_set(p(XX, YY), Rooms).
countRoomsCollectAdd(_, _, _, _).

countRoomsMark(Rooms, Count, Params) :-
	size_nb_set(Rooms, Count),
	forall(gen_nb_set(Rooms, p(X, Y)), countRoomsMarkAdd(X, Y, Params)).
countRoomsMarkAdd(X, Y, params(C, RW, RH)) :-
	XX is X * RW + C,
	YY is Y * RH + C,
	W is RW - C - C,
	H is RH - C - C,
	map:brushNew(B),
	map:brushSetLayer(B, 7),
	map:brushSetX(B, XX),
	map:brushSetY(B, YY),
	map:brushSetW(B, W),
	map:brushSetH(B, H),
	map:brushSetTile(B, 0),
	map:brushSetMapX2(B, 8),
	map:brushSetMapY2(B, 8),
	map:brushSetColor(B, 0x8000ff00),
	def:shader(blend, Shader),
	map:brushSetShader(B, Shader),
	map:brushSetID(B, 1234567),
	map:brushSetSelect(B, 1).











