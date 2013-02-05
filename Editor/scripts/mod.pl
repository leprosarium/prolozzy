:-module(mod, [view/1,
	       setView/1,
	       viewMode/2,
	       roomInfo/1,
	       setRoomInfo/1,
	       roomInfoName/2,
	       brushNew/1,
	       brushProp/2,
	       brushProp/3,
	       brushProp/4,
	       brushDraw/0,
	       brushDraw/1,
	       userUpdate/0]).

brushProp(user, 32).
brushProp(max, 48).

brushProp(layer, "layer",	"layer").
brushProp(x, "x*",		"x readonly").
brushProp(y, "y*",		"y readonly").
brushProp(w, "w*",		"w readonly").
brushProp(h, "h*",		"h readonly").
brushProp(title, "tile",	"tile id").
brushProp(frame, "frame",	"tile frame").
brushProp(maxX1, "map x1",	"map left").
brushProp(maxY1, "map y1",	"map top").
brushProp(maxX2, "map x2",	"map right").
brushProp(maxY2, "map y2",	"map bottom").
brushProp(flip,  "flip",	"flip", select(Flips)):- findall(Id-Name, def:flip(Id, _, Name), Flips).
brushProp(color, "color",	"color", color).
brushProp(shader, "shader",	"shader", select(ShaderNames)) :- findall(Id-Name, def:shader(Id, _, Name), ShaderNames).
brushProp(scale, "scale",	"scale").
brushProp(select, "select*",	"select").

		% 16
brushProp(type, "type",		"brush type", select(Types)):- findall(Id-Name, def:brushType(Id, _, Name), Types).
brushProp(id, "id",		"object id").
brushProp(material, "material",	"material", select(MatNames)):- findall(Name-Name, def:material(Name, _, _, _), MatNames).
brushProp(draw, "draw",		"draw mode", select(DrawNames)):- findall(Id-Name, def:drawMode(Id, _, Name), DrawNames).
brushProp(disable, "disable",	"no update no draw", select(["no","yes"])).
brushProp(delay, "delay",	"frame delay").
brushProp(anim, "anim",		"animation mode", select(["stop","play","loop"])).
brushProp(collider, "collider",	"collider mode", select(["none","call handler","hard collision"])).
brushProp(class, "class",	"generic class", select(ClassNames)) :- findall(Id-Name, def:class(Id, _, Name), ClassNames).

brushProp(status, "status",	"generic status").
brushProp(target, "target",	"target id").
brushProp(death, "death",	"death cause").
brushProp(reserved, "reserved",	"reserved").
brushProp(reserved, "reserved",	"reserved").
brushProp(reserved, "reserved",	"reserved").
brushProp(reserved, "reserved",	"reserved").% collision

		% 32
brushProp(user, "user",		"user", custom).
brushProp(user, "user",		"user",	custom).
brushProp(user, "user",		"user", custom).
brushProp(user, "user",		"user",	custom).
brushProp(user, "user",		"user", custom).
brushProp(user, "user",		"user",	custom).
brushProp(user, "user",		"user", custom).
brushProp(user, "user",		"user",	custom).
brushProp(user, "user",		"user", custom).
brushProp(user, "user",		"user",	custom).
brushProp(user, "user",		"user", custom).
brushProp(user, "user",		"user",	custom).
brushProp(user, "user",		"user", custom).
brushProp(user, "user",		"user",	custom).
brushProp(user, "user",		"user", custom).
brushProp(user, "user",		"user",	custom).

propGet(Key, Val) :-
	recorded(Key, Val).
propSet(Key, Val) :-
	recorded(Key, _, Ref),
	erase(Ref),
	recorda(Key, Val).

viewMode(default, "default view").
viewMode(select, "select view").
viewMode(game, "game view").
viewMode(matrial, "matrial view").
viewMode(density, "density view").

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
	def:shader(blend, SHADER_BLEND, _),
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
	(def:brushType(static, TypeCode, _);
	def:brushType(dynamic, TypeCode, _),
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
	def:brushType(static, TypeCode, _),
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
	def:brushType(Type, TypeCode, _),
	(   Type == static
	->  edi:toolBrushGetMaterial(MatCode),
	    def:material(Mat, MatCode, _, _),
	    format(string(Bar1), 'brush~s  ~s  ~a', [IDt, Draw, Mat])
	;   (   (Dr == img; Dr == imgmat)
	    ->	Vis = visible
	    ;	Vis = hidden),
	    edi:toolBrushGetClass(ClassCode),
	    def:class(_, ClassCode, Class),
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

formatID(0, "").
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













