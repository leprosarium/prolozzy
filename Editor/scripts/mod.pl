:-module(mod, [view/1,
	       setView/1,
	       viewMode/2,
	       roomInfo/1,
	       setRoomInfo/1,
	       brushToolDraw/1,
	       brushNew/1,
	       brushProp/4,
	       brushDraw/1,
	       brushDraw/2]).

brushProp(id, id,		'object id', atom).
brushProp(layer, layer,	layer, common).
brushProp(x, 'x*',		'x readonly', common).
brushProp(y, 'y*',		'y readonly', common).
brushProp(w, 'w*',		'w readonly', common).
brushProp(h, 'h*',		'h readonly', common).
brushProp(tile, 'tile',	'tile id', common).
brushProp(frame, 'frame',	'tile frame', common).
brushProp(x1, 'map x1',	'map left', common).
brushProp(y1, 'map y1',	'map top', common).
brushProp(x2, 'map x2',	'map right', common).
brushProp(y2, 'map y2',	'map bottom', common).
brushProp(flip,  'flip',	'flip', select(Flips)):- findall(Id-Name, def:flip(_, Id, Name), Flips).
brushProp(color, 'color',	'color', color).
brushProp(shader, shader,	shader, select(ShaderNames)) :- findall(Id-Name, def:shader(Name, Id), ShaderNames).
brushProp(scale, 'scale',	'scale', common).
%brushProp(select, 'select*',	'select', common).

		% 16
%brushProp(type, 16, type,		'brush type', select(Types)):- findall(Id-Name, def:brushType(Name, Id), Types).
brushProp(material, material,	'material', select(MatNames)):- findall(Id-Name, def:material(Name, Id, _, _), MatNames).
brushProp(draw, draw,		'draw mode', select(DrawNames)):- findall(Id-Name, def:drawMode(_, Id, Name), DrawNames).
brushProp(disable, disable,	'no update no draw', select([0-no,1-yes])).
brushProp(delay, delay,	'frame delay', common).
brushProp(anim, anim,		'animation mode', select([0-stop,1-play,2-loop])).
brushProp(collider, collider,	'collider mode', select([0-none,1-'call handler',1-'hard collision'])).
%brushProp(class, 24, class,	'generic class', select(ClassNames)) :-
%				findall(Id-Name, def:class(Name, Id),
%				ClassNames).

%brushProp(status, 25, 'status',	'generic status', common).
%brushProp(target, 26, 'target',	'target id', common).
%brushProp(death, 27, 'death',	'death cause', common).
%brushProp(reserved, 28, 'reserved',	'reserved', common).
%brushProp(reserved, 29, 'reserved',	'reserved', common).
%brushProp(reserved, 30, 'reserved',	'reserved', common).
%brushProp(reserved, 31, 'reserved',	'reserved', common).% collision

		% 32
%brushProp(user, 32, 'user',		'user', custom).
%brushProp(user, 33, 'user',		'user',	custom).
%brushProp(user, 34, 'user',		'user', custom).
%brushProp(user, 35, 'user',		'user',	custom).
%brushProp(user, 36, 'user',		'user', custom).
%brushProp(user, 37, 'user',		'user',	custom).
%brushProp(user, 38, 'user',		'user', custom).
%brushProp(user, 39, 'user',		'user',	custom).
%brushProp(user, 40, 'user',		'user', custom).
%brushProp(user, 41, 'user',		'user',	custom).
%brushProp(user, 42, 'user',		'user', custom).
%brushProp(user, 43, 'user',		'user',	custom).
%brushProp(user, 44, 'user',		'user', custom).
%brushProp(user, 45, 'user',		'user',	custom).
%brushProp(user, 46, 'user',		'user', custom).
%brushProp(user, 47, 'user',		'user',	custom).

propGet(Key, Val) :-
	recorded(Key, Val).
propSet(Key, Val) :-
	recorded(Key, _, Ref),
	erase(Ref),
	recorda(Key, Val).

viewMode(default, 'default view').
viewMode(select, 'select view').
viewMode(game, 'game view').
viewMode(material, 'material view').
viewMode(density, 'density view').

property(roomInfo, name).
property(view, Def) :- viewMode(Def, _), !.


:- forall(property(Prop, Def), recorda(Prop, Def)).


view(V) :- propGet(view, V).
setView(V) :- viewMode(V, _), propSet(view, V).
roomInfo(I):- propGet(roomInfo, I).
setRoomInfo(I) :- propSet(roomInfo, I).

brushToolDraw(B) :-
	(   brush:getAnim(B, 0);
	brush:getDelay(B, Delay),
	core:tickCount(Time),
	Fr  is Time // 25,
	(   Delay > 0
	->  Frame is Fr // Delay
	;   Frame = Fr),
	brush:setFrame(B, Frame)).



% Initialize props of the current tool brush, specific to the current type
% Called from Brush Type button in the menu bar

brushNew(Type) :-
	def:shader(blend, SHADER_BLEND),
	(   Type = static
	->  def:drawMode(imgmat, Draw, _)   % draw in img+mat
	;   def:drawMode(img, Draw, _)), % visible

	edi:toolBrush(B),
	brush:setProps(B, [shader=SHADER_BLEND, scale=100, id='', material=0, disable=0, delay=3, anim=2, collider=0, draw=Draw]),
	brush:delete(B).


brushDraw(Br) :-
	view(V),
	brushDraw(V, Br).

brushDraw(default, _).
brushDraw(select, B) :-
	brush:getSelect(B, Select),
	Select \= 0.
brushDraw(game, B) :-
	brush:getDraw(B, DrawCode),
	(def:drawMode(img, DrawCode, _); def:drawMode(imgmat, DrawCode, _)),
	(   brush:getID(B, _)
	->  brush:getDisable(B, 0)
	;   true).


brushDraw(material, B) :-
	brushDrawStaticAlpha(MatCode, B),
	def:material(_, MatCode, _, Color),
	brush:setColor(B, Color).

brushDraw(density, B) :-
	brushDrawStaticAlpha(MatCode, B),
	def:material(_, MatCode, Density, _),
	def:density(Density, Color),
	brush:setColor(B, Color).

brushDrawStaticAlpha(MatCode, B) :-
	\+ brush:getID(B, _),
	brush:getDraw(B, DrawCode),
	(def:drawMode(mat, DrawCode, _); def:drawMode(imgmat, DrawCode, _)),
	brush:getMaterial(B, MatCode),
	def:internalShader(alpharep, Shader),
	brush:setShader(B, Shader).

userUpdate :-
	view(V),
	viewMode(V, View),
	format(string(Bar2), '~s  ', [View]),
	dlgStatusBar:set(2, Bar2).

userUpdatePaint(B) :-
	brush:getDraw(B, DrawCode),
	def:drawMode(Dr, DrawCode, Draw),
	(   brush:getID(B, ID)
	->  (   (Dr == img; Dr == imgmat)
	    ->	Vis = visible
	    ;	Vis = hidden),
%	    brush:getClass(B, ClassCode),
%	    def:class(Class, ClassCode),
%	    format(string(Bar1), 'object ~a  ~a  ~s', [ID, Vis, Class])
	    format(string(Bar1), 'object ~a  ~a', [ID, Vis])
	;   brush:getMaterial(B, MatCode),
	    def:material(Mat, MatCode, _, _),
	    format(string(Bar1), 'brush  ~a  ~a', [Draw, Mat])
	),
	dlgStatusBar:set(1, Bar1),

	brush:getFlip(B, FlipCode),
	brush:getColor(B, Color),
	def:flip(_, FlipCode, Flip),
	brush:getX(B, X),
	brush:getY(B, Y),
	brush:getW(B, W),
	brush:getH(B, H),
	format(string(Bar3), '~16r  ~s  ~d, ~d  ~dx~d  ', [Color, Flip, X, Y, W, H]),
	dlgStatusBar:set(3, Bar3),
	updateRoomInfo(X, Y).

userUpdateEdit :-
	map:getSelect(Select),
	format(string(Bar1), '  selected ~d', [Select]),
	dlgStatusBar:set(1, Bar1),
	dlgStatusBar:set(3, ''),
	edi:getAxeX(AxeX),
	edi:getAxeY(AxeY),
	updateRoomInfo(AxeX, AxeY).

updateRoomInfo(X, Y) :-
	map:getRoomW(RoomW),
	map:getRoomH(RoomH),
	RX is X // RoomW,
	RY is Y // RoomH,
	roomInfo(RX, RY, Info),
	format(string(Bar4), '"~w" room ~d, ~d', [Info, RX, RY]),
	dlgStatusBar:set(4, Bar4).

roomInfo(RX, RY, Val) :-
	roomInfo(Prop),
	roomNames:get(RX, RY, Prop, Val).
roomInfo(_, _, '').










