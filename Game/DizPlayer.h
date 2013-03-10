//////////////////////////////////////////////////////////////////////////////////////////////////
// DizPlayer.h
//////////////////////////////////////////////////////////////////////////////////////////////////
#ifndef __DIZPLAYER_H__
#define __DIZPLAYER_H__

//#include "E9System.h"
//#include "D9Debug.h"
#include "DizPaint.h"

#include "SWI-cpp-m.h"


//////////////////////////////////////////////////////////////////////////////////////////////////

#define TILE_IDLE			10			// default 
#define TILE_WALK			11			// default 
#define TILE_UP				12			// default 
#define TILE_JUMP			13			// default 
#define TILE_STUN			14			// default 
#define TILE_DEAD			15			// default 

#define DIZ_STEPX			4			// move step x
#define DIZ_STEPY			4			// move step y; used in adjustments
#define DIZ_STEPYMAX		7			// move step y; used in jumps and falls
#define DIZ_BOXW			16			// collision box width
#define DIZ_BOXH			20			// collision box height

//////////////////////////////////////////////////////////////////////////////////////////////////

#define COLLIDER_HANDLE					(1<<0)
#define COLLIDER_HARD					(1<<1)

//////////////////////////////////////////////////////////////////////////////////////////////////
// cDizPlayer
//////////////////////////////////////////////////////////////////////////////////////////////////
class cDizPlayer
{
public:
					cDizPlayer			();
					~cDizPlayer			();

		// init
		void		Reset				();											// reset when start game
virtual	void		Update				();
virtual	void		Draw				();

		// enter states
		void		EnterIdle			();											// enter idle status
		void		EnterWalk			( int dir );								// enter walk status
		void		EnterJump			( int dir, int pow );						// enter jump status
		void		EnterFall			();											// enter fall status
		void		EnterRoll			();											// enter a roll (keep fall) or idle if ready
		void		EnterJumper			( int mat );								// enter on a jumper with mat material
		void		EnterSpin			( int dir );								// enter a forced roll
		void		EnterKeyState		();											// enter state requested by input keys

		// update states
		void		UpdateIdle			();
		void		UpdateWalk			();
		void		UpdateJump			();
		void		UpdateFall			();
		void		UpdateScripted		();
		
		// checks
		bool		CheckFree			( int x1, int y1, int x2, int y2 );			// general hard material in-box test
		bool		CheckWalkX			();											// test horizontal movement space when walking
		bool		CheckJumpX			();											// test horizontal movement space when jumping
		int			CheckJumpY			( int step );								// test for space above the player when jumping for a specified number of lines; return reduced step if collision detected
		bool		CheckFallX			();											// test horizontal movement space when falling
		int			CheckFallY			( int step );								// test material under player for a specified number of lines; return reduced step if collision detected
		void		CheckCollision		();											// test material hard collision and push player up
		int			CheckJumper			();											// test for jumper right under player; return jumper material or -1
		void		CheckColliders		();											// call collision handler for touched colliders that want it
		bool		CheckCollidersSnap	();											// test colliders collision and snap player if necessary

		void		ReadMatInfo			();											// read materials info
		
		// utils
inline	void		MakeBBW				( int &x1, int &y1, int &x2, int &y2 ) const { x1 = _x-_w/2; x2 = _x+_w/2;	y1 = _y-_h/2; y2 = _y+_h/2; }
		void		MakeBB				( int &x1, int &y1, int &x2, int &y2 ) const;		

inline	cTile*		FindTile			()											{ return g_paint.tiles.Get(g_paint.tiles.Find(_tile)); }
		
		// not exported
		bool		m_input;			// user input accepted
		bool		m_debug;			// prevent player's update while debug movement
		
		PlAtom		status() const { return _status; }
		int			x() const { return _x; }
		int			y() const { return _y; }
		int			w() const { return _w; }
		int			h() const { return _h; }
		int			life() const { return _life; }
		int			pow() const { return _pow; }
		int			tile() const { return _tile; }
		int			frame() const { return _frame; }
		int			costume() const { return _costume; }
		int			stunLevel() const { return _stunLevel; }
		int			matInside() const { return _matInside; }
		int			matUnder() const { return _matUnder; }
		int			matCenter() const { return _matCenter; }
		int			delay() const { return _delay; }
		int			layer() const { return _layer; }
		unsigned	color() const { return _color; }
		int			shader() const { return _shader; }
		bool		flipX() const { return _flipX; }
		bool		flipY() const { return _flipY; }
		int			dir() const { return _dir; }
		int			tileIdle() const { return _tileIdle; }
		int			tileWalk() const { return _tileWalk; }
		int			tileUp() const { return _tileUp; }
		int			tileJump() const { return _tileJump; }
		int			emotion() const { return _emotion; }
		int			anim() const { return _anim; }
		bool		disable() const { return _disable; }
		bool		customMove() const { return _customMove; }

		void		status(const PlAtom &status ) { _status = status; }
		void		x(int x) { _x = x; }
		void 		y(int y) { _y = y; }
		void		life(int life) { _life = life; }
		void		pow(int	pow) { _pow = pow; }
		void		tile(int tile) { _tile = tile; }
		void		frame(int frame) { _frame = frame; }
		void		costume(int costume) { _costume = costume; }
		void		stunLevel(int stunLevel) { _stunLevel = stunLevel; }
		void		delay(int delay) { _delay = delay; }
		void		layer(int layer) { _layer = layer; }
		void		color(unsigned color) { _color = color; }
		void		shader(int shader) { _shader = shader; }
		void		flipX(bool flipX) { _flipX = flipX; }
		void		flipY(bool flipY) { _flipY = flipY; }
		void		dir(int dir) { _dir = dir; }
		void		tileIdle(int tile) { _tileIdle = tile; }
		void		tileWalk(int tile) { _tileWalk = tile; }
		void		tileUp(int tile) { _tileUp = tile; }
		void		tileJump(int tile) { _tileJump = tile; }
		void		emotion(int emotion) { _emotion = emotion; }
		void		anim(int anim) { _anim = anim; }
		void		disable(bool disable) { _disable = disable; }
		void		customMove(bool customMove) { _customMove = customMove; }

private:
		PlAtom		idle, walk, jump, fall, scripted; // Statuses
		PlAtom		_status;			// player status
		int			_x;					// position x in world
		int			_y;					// position y in world
		int			_w;					// width of the object (usually the sprite width); read-only
		int			_h;					// height of the object (usually the sprite height); read-only
		int			_life;				// life energy [0,100]
		int			_pow;				// power jump/fall (positive value used to measure jumps or falls)
		int			_tile;				// the current tile used to draw
		int			_frame;				// the animation frame in the tile (starts with 0)
		int			_costume;			// tile costume (id offset)
		int			_stunLevel;			// normal stun 0 (none); critical STUN_LEVEL=20 (stunned)
		int			_matInside;			// materials inside player's box on bits
		int			_matUnder;			// materials right under player's box on bits
		int			_matCenter;			// material in center of player's box (at mouth point)
		int			_delay;				// update skips 'delay' frames (different from the animation delay that is in tile)
		int			_layer;				// player's layer
		unsigned	_color;				// color used to draw
		int			_shader;			// shader
		bool		_flipX;
		bool		_flipY;				
		int			_dir;				// direction -1,0,1
		int			_tileIdle;			// tile for idle status
		int			_tileWalk;			// tile for walk status
		int			_tileUp;			// tile for jump up status
		int			_tileJump;			// tile for jump side status
		int			_emotion;			// emotion (tile id offset from tileidle); 0=no emmotion, just idle
		int			_anim;				// animation playing mode (0=stop,1=normal,2=loop)
		bool		_disable;			// if disabled no update and no draw
		bool		_customMove;		// custom movement for player

};

extern	cDizPlayer	g_player;

#endif
//////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////
