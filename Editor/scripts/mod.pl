:-module(mod, [view/1,
	       setView/1,
	       viewMode/2,
	       roomInfo/1,
	       setRoomInfo/1,
	       roomInfoName/2,
	       brushNew/1,
	       brushProp/2,
	       brushProp/5,
	       brushDraw/0,
	       brushDraw/1,
	       userUpdate/0]).

brushProp(user, 32).
brushProp(max, 48).

brushProp(layer, 0, layer,	layer, common).
brushProp(x, 1, 'x*',		'x readonly', common).
brushProp(y, 2, 'y*',		'y readonly', common).
brushProp(w, 3, 'w*',		'w readonly', common).
brushProp(h, 4, 'h*',		'h readonly', common).
brushProp(title, 5, 'tile',	'tile id', common).
brushProp(frame, 6, 'frame',	'tile frame', common).
brushProp(maxX1, 7, 'map x1',	'map left', common).
brushProp(maxY1, 8, 'map y1',	'map top', common).
brushProp(maxX2, 9, 'map x2',	'map right', common).
brushProp(maxY2, 10, 'map y2',	'map bottom', common).
brushProp(flip,  11, 'flip',	'flip', select(Flips)):- findall(Id-Name, def:flip(_, Id, Name), Flips).
brushProp(color, 12, 'color',	'color', color).
brushProp(shader, 13, shader,	shader, select(ShaderNames)) :- findall(Id-Name, def:shader(Name, Id), ShaderNames).
brushProp(scale, 14, 'scale',	'scale', common).
brushProp(select, 15, 'select*',	'select', common).

		% 16
brushProp(type, 16, type,		'brush type', select(Types)):- findall(Id-Name, def:brushType(Name, Id), Types).
brushProp(id, 17, id,		'object id', common).
brushProp(material, 18, material,	'material', select(MatNames)):- findall(Id-Name, def:material(Name, Id, _, _), MatNames).
brushProp(draw, 19,  draw,		'draw mode', select(DrawNames)):- findall(Id-Name, def:drawMode(_, Id, Name), DrawNames).
brushProp(disable, 20, disable,	'no update no draw', select([0-no,1-yes])).
brushProp(delay, 21, delay,	'frame delay', common).
brushProp(anim, 22, anim,		'animation mode', select([0-stop,1-play,2-loop])).
brushProp(collider, 23, collider,	'collider mode', select([0-none,1-'call handler',1-'hard collision'])).
brushProp(class, 24, class,	'generic class', select(ClassNames)) :- findall(Id-Name, def:class(Name, Id), ClassNames).

brushProp(status, 25, 'status',	'generic status', common).
brushProp(target, 26, 'target',	'target id', common).
brushProp(death, 27, 'death',	'death cause', common).
brushProp(reserved, 28, 'reserved',	'reserved', common).
brushProp(reserved, 29, 'reserved',	'reserved', common).
brushProp(reserved, 30, 'reserved',	'reserved', common).
brushProp(reserved, 31, 'reserved',	'reserved', common).% collision

		% 32
brushProp(user, 32, 'user',		'user', custom).
brushProp(user, 33, 'user',		'user',	custom).
brushProp(user, 34, 'user',		'user', custom).
brushProp(user, 35, 'user',		'user',	custom).
brushProp(user, 36, 'user',		'user', custom).
brushProp(user, 37, 'user',		'user',	custom).
brushProp(user, 38, 'user',		'user', custom).
brushProp(user, 39, 'user',		'user',	custom).
brushProp(user, 40, 'user',		'user', custom).
brushProp(user, 41, 'user',		'user',	custom).
brushProp(user, 42, 'user',		'user', custom).
brushProp(user, 43, 'user',		'user',	custom).
brushProp(user, 44, 'user',		'user', custom).
brushProp(user, 45, 'user',		'user',	custom).
brushProp(user, 46, 'user',		'user', custom).
brushProp(user, 47, 'user',		'user',	custom).

propGet(Key, Val) :-
	recorded(Key, Val).
propSet(Key, Val) :-
	recorded(Key, _, Ref),
	erase(Ref),
	recorda(Key, Val).

viewMode(default, 'default view').
viewMode(select, 'select view').
viewMode(game, 'game view').
viewMode(matrial, 'matrial view').
viewMode(density, 'density view').

property(roomInfo, 0).
property(view, Def) :- viewMode(Def, _), !.


:- forall(property(Prop, Def), recorda(Prop, Def)).


view(V) :- propGet(view, V).
setView(V) :- viewMode(V, _), propSet(view, V).
roomInfo(I):- propGet(roomInfo, I).
setRoomInfo(I) :- propSet(roomInfo, I).

% Initialize props of the current tool brush, specific to the current type
% Called from Brush Type button in the menu bar

brushNew(Type) :-
	def:shader(blend, SHADER_BLEND),
	edi:toolBrushSetShader(SHADER_BLEND),
	edi:toolBrushSetScale(100),
	edi:toolBrushSetID(0),
	edi:toolBrushSetMaterial(0),
	edi:toolBrushSetDisable(0),
	edi:toolBrushSetDelay(3),
	edi:toolBrushSetAnim(2),
	edi:toolBrushSetCollider(0),
	edi:toolBrushSetClass(0),
	edi:toolBrushSetStatus(0),
	edi:toolBrushSetTarget(0),
	edi:toolBrushSetDeath(0),
	(   Type = static
	->  Draw = 3   % draw in img+mat
	;   Draw = 1), % visible
	edi:toolBrushSetDraw(Draw),
	brushProp(user, USER),
	brushProp(max, MX),
	UserMax is MX - USER - 1,
	forall(between(0, UserMax, Idx), edi:toolBrushSetUser(Idx,0)).


brushDraw :-
	view(V),
	brushDraw(V).

brushDraw(default).
brushDraw(select) :-
	edi:toolBrushGetSelect(Select),
	Select \= 0.
brushDraw(game) :-
	edi:toolBrushGetDraw(DrawCode),
	(def:drawMode(img, DrawCode, _); def:drawMode(imgmat, DrawCode, _)),
	edi:toolBrushGetType(TypeCode),
	(def:brushType(static, TypeCode);
	def:brushType(dynamic, TypeCode),
	edi:toolBrushGetDisable(0)).


brushDraw(matrial) :-
	brushDrawStaticAlpha(MatCode),
	def:material(_, MatCode, _, Color),
	edi:toolBrushSetColor(Color).

brushDraw(density) :-
	brushDrawStaticAlpha(MatCode),
	def:material(_, MatCode, Density, _),
	def:density(Density, Color),
	edi:toolBrushSetColor(Color).

brushDrawStaticAlpha(MatCode) :-
	edi:toolBrushGetType(TypeCode),
	def:brushType(static, TypeCode),
	edi:toolBrushGetDraw(DrawCode),
	(def:drawMode(mat, DrawCode, _); def:drawMode(imgmat, DrawCode, _)),
	edi:toolBrushGetMaterial(MatCode),
	def:internalShader(alpharep, Shader),
	edi:toolBrushSetShader(Shader).

userUpdate :-
	edi:getTool(Tool),
	userUpdate(Tool),
	view(V),
	viewMode(V, View),
	format(string(Bar2), '~s  ', [View]),
	dlgStatusBar:set(2, Bar2).

userUpdate(0) :-
	edi:toolBrushGetID(ID),
	formatID(ID, IDt),
	edi:toolBrushGetType(TypeCode),
	edi:toolBrushGetDraw(DrawCode),
	def:drawMode(Dr, DrawCode, Draw),
	def:brushType(Type, TypeCode),
	(   Type == static
	->  edi:toolBrushGetMaterial(MatCode),
	    def:material(Mat, MatCode, _, _),
	    format(string(Bar1), 'brush~s  ~s  ~a', [IDt, Draw, Mat])
	;   (   (Dr == img; Dr == imgmat)
	    ->	Vis = visible
	    ;	Vis = hidden),
	    edi:toolBrushGetClass(ClassCode),
	    def:class(Class, ClassCode),
	    format(string(Bar1), 'object~s  ~a  ~s', [IDt, Vis, Class])
	),
	dlgStatusBar:set(1, Bar1),

	edi:toolBrushGetFlip(FlipCode),
	edi:toolBrushGetColor(Color),
	def:flip(_, FlipCode, Flip),
	edi:toolBrushGetX(X),
	edi:toolBrushGetY(Y),
	edi:toolBrushGetW(W),
	edi:toolBrushGetH(H),
	format(string(Bar3), '~16r  ~s  ~d, ~d  ~dx~d  ', [Color, Flip, X, Y, W, H]),
	dlgStatusBar:set(3, Bar3),
	updateRoomInfo(X, Y).

userUpdate(1) :-
	edi:getSelect(Select),
	format(string(Bar1), '  selected ~d', [Select]),
	dlgStatusBar:set(1, Bar1),
	dlgStatusBar:set(3, ''),
	edi:getAxeX(AxeX),
	edi:getAxeY(AxeY),
	updateRoomInfo(AxeX, AxeY).

formatID(0, '').
formatID(ID, Text) :-
	format(string(Text), '  ~d', [ID]).


updateRoomInfo(X, Y) :-
	edi:getRoomW(RoomW),
	edi:getRoomH(RoomH),
	RX is X // RoomW,
	RY is Y // RoomH,
	roomInfo(RX, RY, Info),
	format(string(Bar4), '"~s" room ~d, ~d', [Info, RX, RY]),
	dlgStatusBar:set(4, Bar4).

roomInfo(_RX, _RY, "<Info>").

roomInfoName(0, "show room name").
roomInfoName(5, "show room props 0-3").
roomInfoName(6, "show room props 4-7").
roomInfoName(X, Text) :-
	between(1, 4, X),
	format(string(Text), 'show room text ~d', [X-1]).













