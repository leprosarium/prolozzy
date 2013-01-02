:- module(def, [material/4,
		material/2,
		def/2,
		dlg/2,
		key/2,
		color/2,
		cmd/2,
		class/2,
		windPow/1,
		dizHurt/1,
		ptile/2,
		tile/2,
		emotion/2,
		noairEmotion/1,
		maxAirLevel/1,
		flipX/1,flipY/1, flipR/1,maxStunLevel/1,
		dizPow/1,
		dizStep/1,
		jumpFixPow/1, jumpProPow/1,
		gameSrcW/1, gameSrcH/1
	       ]).

material(X, Idx) :- material(X, Idx, _, _).
material(air,	 0, void, 0xFF000000).	% void
material(water,	 1, void, 0xFF0060FF).	% water (void); player can drawn in water
material(hurt,	 2, void, 0xFFFF8000).	% hurt (void); player gets hurt
material(kill,	 3, void, 0xFFD00000).	% kill (void); player gets killed
material(cloud,	 4, soft, 0xFFC0C0C0).	% clouds (medium); player sinks on clouds
material(climb,	 5, soft, 0xFF909090).	% stairs (medium); player stands on
material(wind,	 6, soft, 0xFF707070).	% winds (medium); player is pushed up
material(block,	 7, hard, 0xFF006000).	% ground, walls (hard); blocks the player
material(jumpFix, 8, jump, 0xFF008000).	% jumper fix (hard)
material(jumpPro, 9, jump, 0xFF00B000).	% jumper progressive (hard)

% objects classes
class(none,	0).			% default
class(action,	1).			% those objects can be used with action
class(hurt,	2).			% used for objects that should hurt you (no automation implemented)
class(kill,	3).			% used for objects that should kill you (no automation implemented)
class(item,	4).			% those objects can be picked up in the inventory
class(coin,	5).			% those objects can be collected as coins
class(food,	6).			% those objects can eaten to grow the life
class(life,	7).			% those objects gives a credit when picked up.
class(waypoint,	8).			% used for dummy waypoints (no automation implemented)

emotion(ok, 0).
emotion(noair1, 6).			% player's emotion offset for no air1
emotion(noair2, 7).			% player's emotion offset for no air2
emotion(noair3, 8).			% player's emotion offset for no air3

noairEmotion(noair1).
noairEmotion(noair2).
noairEmotion(noair3).

gameSrcW(256).	                % screen's width (fixed by engine to Z80 screen size)
gameSrcH(192).			% screen's height (fixed by engine to Z80 screen size)
maxAirLevel(100).		% player's air critical level
maxStunLevel(20).		% player's stun critical level
dizHurt(4).			% default hurt level for "hurt" materials
windPow(4).		        % default wind power
dizPow(7).			% player's jump power (for default movement)
dizStep(4).	                % player's walk step (for default movement)
jumpFixPow(16).	                % fix jumper power
jumpProPow(16).	% pro jumper power limit
flipX(1).	% flip x (bit value)
flipY(2).	% flip y (bit value)
flipR(4).	% flip rotate (bit value)


ptile(idle, 10).			% ptileault player's idle tile id
ptile(walk, 11).			% ptileault player's walk tile id
ptile(up, 12).
ptile(jump, 13).
ptile(stun, 14).
ptile(dead, 15).
ptile(noair1,	16).
ptile(noair2,	17).
ptile(noair3,	18).
ptile(eat, 19).
ptile(swim, 20).
ptile(swimup,	21).
ptile(swimjump,	22).
ptile(drawn,	23).

tile(id, 0).
tile(w, 1).
tile(h, 2).
tile(frames, 3).
tile(name, 5).

def('TILE_IDLE',	10).			% default
def('TILE_WALK',	11).			% default
def('TILE_UP',		12).			% default
def('TILE_JUMP',	13).			% default
def('TILE_STUN',	14).			% default
def('TILE_DEAD',	15).			% default

def('R_MAX', 8).

dlg(style, 0).				% style
dlg(x,	1).				% x in scr pixels
dlg(y,	2).				% y in scr pixels
dlg(w,	3).				% w in scr pixels
dlg(h,	4).				% h in scr pixels
dlg(color,5).				% user color (usualy for border)
dlg(font,	6).				% font tile id
dlg(user,	8).				% user defined
dlg(max,	16).


key(left,	1).
key(right,	2).
key(up,		4).
key(down,	8).
key(jump,	16).
key(action,	32).
key(menu,	64).
key(max,	128).


% colors
color(black,	0xff000000).
color(blue,	0xff0000ff).
color(red,      0xffff0000).
color(magenta,	0xffff00ff).
color(green,	0xff00ff00).
color(cyan,	0xff00ffff).
color(yellow,	0xffffff00).
color(white,	0xffffffff).
color(dialog,	0xff00ff00).	% dialog default border color


cmd(none, 0).	% nothing to do
cmd(start, 1).	% must start game
cmd(exit, 2).	% must exit game
cmd(refresh, 3).% refresh room material map




