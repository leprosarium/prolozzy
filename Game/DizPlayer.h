//////////////////////////////////////////////////////////////////////////////////////////////////
// DizPlayer.h
//////////////////////////////////////////////////////////////////////////////////////////////////
#ifndef __DIZPLAYER_H__
#define __DIZPLAYER_H__

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

//////////////////////////////////////////////////////////////////////////////////////////////////

#define COLLIDER_HANDLE					(1<<0)
#define COLLIDER_HARD					(1<<1)

//////////////////////////////////////////////////////////////////////////////////////////////////
// cDizPlayer
//////////////////////////////////////////////////////////////////////////////////////////////////
class cDizPlayer
{
	void ReadMatInfo();
	cTile * FindTile() { return g_paint.tiles.Get(g_paint.tiles.Find(tile)); }

	void EnterIdle();
	void EnterWalk(int dir);
	void EnterJump(int dir, int pow);
	void EnterFall();
	void EnterRoll();
	void EnterJumper(int mat);				// enter on a jumper with mat material
	void EnterSpin(int dir);				// enter a forced roll
	void EnterKeyState();					// enter state requested by input keys

	void UpdateIdle();
	void UpdateWalk();
	void UpdateJump();
	void UpdateFall();
	void UpdateScripted();

	bool CheckFree(const iRect & r) const;				// general hard material in-box test
	bool CheckWalkX() const;							// test horizontal movement space when walking
	bool CheckFallX() const { return CheckWalkX(); }	// test horizontal movement space when falling
	bool CheckJumpX() const { return CheckWalkX(); }	// test horizontal movement space when jumping
	int CheckJumpY(int step) const;						// test for space above the player when jumping for a specified number of lines; return reduced step if collision detected
	int CheckFallY(int step) const;						// test material under player for a specified number of lines; return reduced step if collision detected
	void CheckCollision();								// test material hard collision and push player up
	int CheckJumper();									// test for jumper right under player; return jumper material or -1
	void CheckColliders();								// call collision handler for touched colliders that want it
	bool CheckCollidersSnap();							// test colliders collision and snap player if necessary

	bool key(int k) const;
	bool keyHit(int k) const;

	bool input;			// user input accepted
	int	_matInside;			// materials inside player's box on bits
	int	_matUnder;			// materials right under player's box on bits
	int	_matCenter;			// material in center of player's box (at mouth point)
public:
	bool m_debug;								// prevent player's update while debug movement
	PlAtom idle, walk, jump, fall, scripted;	// Statuses
	PlAtom status;
	iV2 pos;				// position in world
	const iV2 size;
	int life;				// life energy [0,100]
	int	pow;				// power jump/fall (positive value used to measure jumps or falls)
	int	tile;				// the current tile used to draw
	int	tileIdle;			// tile for idle status
	int	tileWalk;			// tile for walk status
	int	tileUp;				// tile for jump up status
	int	tileJump;			// tile for jump side status
	int frame;				// the animation frame in the tile (starts with 0)
	int costume;			// tile costume (id offset)
	int emotion;			// emotion (tile id offset from tileidle); 0=no emmotion, just idle
	int	stunLevel;			// normal stun 0 (none); critical 20 (stunned)
	int	 delay;				// update skips 'delay' frames (different from the animation delay that is in tile)
	int	layer;				//
	dword color;			// color used to draw
	Blend shader;			// shader
	bool flipX;
	bool flipY;	
	int	dir;				// direction -1,0,1
	int	anim;				// animation playing mode (0=stop,1=normal,2=loop)
	bool disable;			// if disabled no update and no draw
	bool customMove;		// custom movement for player

	cDizPlayer();

	void Reset();							// reset when start game
	virtual	void Update();
	virtual	void Draw();

	iRect worldRect() const { return iRect(pos, pos + size).Offset(-size/2); }
	iRect rect() const;

	int			matInside() const { return _matInside; }
	int			matUnder() const { return _matUnder; }
	int			matCenter() const { return _matCenter; }
	dword		flip() const { return (flipX ? 1 : 0 ) | (flipY ? 2 : 0); }
};

extern	cDizPlayer	g_player;

#endif
//////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////
