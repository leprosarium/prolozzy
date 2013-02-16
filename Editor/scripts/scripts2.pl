:-module(scripts2, [checkTile/0,
		    checkId/0,
		    checkOverlapping/0]).

checkTile :-
	core:dl('Check Brush Tile:'),
	select = 0;
	edi:waitCursor(1),
	map:brushCount(BC),
	checkTile(0, BC, 0, Select),
	edi:waitCursor(0),
	edi:setSelect(Select),
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
	edi:waitCursor(1),
	checkIdCollect(Ids),
	checkIdProcess(Ids, Count),
	edi:waitCursor(0),
	edi:setSelect(Count),
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
	edi:waitCursor(1),
	checkOverCollect(Ids),
	checkOverProcess(Ids, Count),
	edi:waitCursor(0),
	edi:setSelect(Count),
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









