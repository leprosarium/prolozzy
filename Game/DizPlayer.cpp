//////////////////////////////////////////////////////////////////////////////////////////////////
// DizPlayer.cpp
//////////////////////////////////////////////////////////////////////////////////////////////////
#include "stdafx.h"
#include "DizPlayer.h"
#include "DizGame.h"

cDizPlayer	g_player;

#define DATA(idx)					( m_data[idx] )
//#define MATINSIDE(mat)				( _matInside & (1<<mat) )
#define KEY(key)					( (m_input) ? g_game.Key(key) : false )
#define KEYHIT(key)					( (m_input) ? g_game.KeyHit(key) : false )

void cDizPlayer::MakeBB	( int &x1, int &y1, int &x2, int &y2 ) const
{ 
	iV2 r = g_game.roomPos() * Room::Size;
	MakeBBW(x1,y1,x2,y2);			
	x1 -= r.x;			
	x2 -= r.x;			
	y1 -= r.y;			
	y2 -= r.y;			
}

PREDICATE_M(player, x, 1)
{
	return A1 = g_player.x();
}

PREDICATE_M(player, setX, 1)
{
	int v;
	if(!PL_get_integer(A1, &v))
		return false; 
	g_player.x(v); 
	return true;
}


PREDICATE_M(player, y, 1)
{
	return A1 = g_player.y();
}

PREDICATE_M(player, setY, 1)
{
	int v;
	if(!PL_get_integer(A1, &v))
		return false; 
	g_player.y(v); 
	return true;
}


PREDICATE_M(player, w, 1)
{
	return A1 = g_player.w();
}

PREDICATE_M(player, h, 1)
{
	return A1 = g_player.h();
}

PREDICATE_M(player, status, 1)
{
	return A1 = g_player.status();
}

PREDICATE_M(player, setStatus, 1)
{
	g_player.status(PlAtom(A1));
	return true;
}

PREDICATE_M(player, life, 1)
{
	return A1 = g_player.life();
}

PREDICATE_M(player, setLife, 1)
{
	int v;
	if(!PL_get_integer(A1, &v))
		return false; 
	g_player.life(v); 
	return true;
}

PREDICATE_M(player, tile, 1)
{
	return A1 = g_player.tile();
}

PREDICATE_M(player, setTile, 1)
{
	int v;
	if(!PL_get_integer(A1, &v))
		return false; 
	g_player.tile(v); 
	return true;
}

PREDICATE_M(player, frame, 1)
{
	return A1 = g_player.frame();
}

PREDICATE_M(player, setFrame, 1)
{
	int v;
	if(!PL_get_integer(A1, &v))
		return false; 
	g_player.frame(v); 
	return true;
}


PREDICATE_M(player, costume, 1)
{
	return A1 = g_player.costume();
}

PREDICATE_M(player, setCostume, 1)
{
	int v;
	if(!PL_get_integer(A1, &v))
		return false; 
	g_player.costume(v); 
	return true;
}


PREDICATE_M(player, stunLevel, 1)
{
	return A1 = g_player.stunLevel();
}

PREDICATE_M(player, setStunLevel, 1)
{
	int v;
	if(!PL_get_integer(A1, &v))
		return false; 
	g_player.stunLevel(v); 
	return true;
}

PREDICATE_M(player, matInside, 1)
{
	return A1 = g_player.matInside();
}

PREDICATE_M(player, matUnder, 1)
{
	return A1 = g_player.matUnder();
}

PREDICATE_M(player, matCenter, 1)
{
	return A1 = g_player.matCenter();
}

PREDICATE_M(player, delay, 1)
{
	return A1 = g_player.delay();
}

PREDICATE_M(player, setDelay, 1)
{
	int v;
	if(!PL_get_integer(A1, &v))
		return false; 
	g_player.delay(v); 
	return true;
}

PREDICATE_M(player, layer, 1)
{
	return A1 = g_player.layer();
}

PREDICATE_M(player, setLayer, 1)
{
	int v;
	if(!PL_get_integer(A1, &v))
		return false; 
	g_player.layer(v); 
	return true;
}

PREDICATE_M(player, color, 1)
{
	return A1 = static_cast<int>(g_player.color());
}

PREDICATE_M(player, setColor, 1)
{
	int64 v = A1;
	g_player.color(static_cast<int>(v)); 
	return true;
}

PREDICATE_M(player, shader, 1)
{
	return A1 = static_cast<int>(g_player.shader());
}

PREDICATE_M(player, setShader, 1)
{
	int v = A1;
	if (v <  static_cast<int>(Blend::Min) || 
		v >= static_cast<int>(Blend::Max))
		return false;
	g_player.shader(static_cast<Blend>(v)); 
	return true;
}

PREDICATE_M(player, flipX, 0)
{
	return g_player.flipX();
}

PREDICATE_M(player, flipY, 0)
{
	return g_player.flipY();
}

PREDICATE_M(player, setFlipX, 1)
{
	int v;
	if(!PL_get_bool(A1, &v))
		return false; 
	g_player.flipX(v == TRUE); 
	return true;
}

PREDICATE_M(player, setFlipY, 1)
{
	int v;
	if(!PL_get_bool(A1, &v))
		return false; 
	g_player.flipY(v == TRUE); 
	return true;
}

PREDICATE_M(player, dir, 1)
{
	return A1 = g_player.dir();
}

PREDICATE_M(player, setDir, 1)
{
	int v;
	if(!PL_get_integer(A1, &v))
		return false; 
	g_player.dir(v); 
	return true;
}

PREDICATE_M(player, tileIdle, 1)
{
	return A1 = g_player.tileIdle();
}

PREDICATE_M(player, setTileIdle, 1)
{
	int v;
	if(!PL_get_integer(A1, &v))
		return false; 
	g_player.tileIdle(v); 
	return true;
}

PREDICATE_M(player, tileWalk, 1)
{
	return A1 = g_player.tileWalk();
}

PREDICATE_M(player, setTileWalk, 1)
{
	int v;
	if(!PL_get_integer(A1, &v))
		return false; 
	g_player.tileWalk(v); 
	return true;
}

PREDICATE_M(player, tileUp, 1)
{
	return A1 = g_player.tileUp();
}

PREDICATE_M(player, setTileUp, 1)
{
	int v;
	if(!PL_get_integer(A1, &v))
		return false; 
	g_player.tileUp(v); 
	return true;
}

PREDICATE_M(player, tileJump, 1)
{
	return A1 = g_player.tileJump();
}

PREDICATE_M(player, setTileJump, 1)
{
	int v;
	if(!PL_get_integer(A1, &v))
		return false; 
	g_player.tileJump(v); 
	return true;
}

PREDICATE_M(player, emotion, 1)
{
	return A1 = g_player.emotion();
}

PREDICATE_M(player, setEmotion, 1)
{
	int v;
	if(!PL_get_integer(A1, &v))
		return false; 
	g_player.emotion(v); 
	return true;
}

PREDICATE_M(player, pow, 1)
{
	return A1 = g_player.pow();
}

PREDICATE_M(player, setPow, 1)
{
	int v;
	if(!PL_get_integer(A1, &v))
		return false; 
	g_player.pow(v); 
	return true;
}

PREDICATE_M(player, anim, 1)
{
	return A1 = g_player.anim();
}

PREDICATE_M(player, setAnim, 1)
{
	int v;
	if(!PL_get_integer(A1, &v))
		return false; 
	g_player.anim(v); 
	return true;
}


PREDICATE_M(player, disable, 0)
{
	return g_player.disable();
}

PREDICATE_M(player, setDisable, 1)
{
	int v;
	if(!PL_get_bool(A1, &v))
		return false; 
	g_player.disable(v == TRUE); 
	return true;
}

PREDICATE_M(player, customMove, 0)
{
	return g_player.customMove();
}

PREDICATE_M(player, setCustomMove, 1)
{
	int v;
	if(!PL_get_bool(A1, &v))
		return false; 
	g_player.customMove(v == TRUE); 
	return true;
}

//////////////////////////////////////////////////////////////////////////////////////////////////
// PLAYER
//////////////////////////////////////////////////////////////////////////////////////////////////

PREDICATE_M(player, makeBB, 4)
{
	int x1, x2, y1, y2;
	g_player.MakeBB(x1, y1, x2, y2);
	A1 = x1;
	A2 = y1;
	A3 = x2;
	A4 = y2;
	return true;
}

PREDICATE_M(player, makeBBW, 4)
{
	int x1,y1,x2,y2;
	g_player.MakeBBW(x1,y1,x2,y2);
	A1 = x1;
	A2 = y1;
	A3 = x2;
	A4 = y2;
	return true;
}

//////////////////////////////////////////////////////////////////////////////////////////////////
// INIT
//////////////////////////////////////////////////////////////////////////////////////////////////

cDizPlayer::cDizPlayer() : idle("idle"), walk("walk"), jump("jump"), fall("fall"), scripted("scripted"), _status(idle)
{
	Reset();
}

cDizPlayer::~cDizPlayer()
{
}

void cDizPlayer::Reset()
{

	shader(Blend::Alpha);
	status(idle);
	delay(3);
	layer(7);
	_w					= DIZ_BOXW;
	_h					= DIZ_BOXH;

	tile(TILE_IDLE);
	anim(2);
	color(0xffffffffu);

	tileIdle(TILE_IDLE);
	tileWalk(TILE_WALK);
	tileUp(TILE_UP);
	tileJump(TILE_JUMP);

	_life				= 100;

	m_input	= true;
	m_debug	= false;

}


//////////////////////////////////////////////////////////////////////////////////////////////////
// UPDATE
//////////////////////////////////////////////////////////////////////////////////////////////////

void cDizPlayer::Update()
{

	//Sleep(200); // @T

	if( disable() || m_debug ) return; // disabled

	// delay
	if( !g_game.IsUpdate(delay()) ) return;

	// read mat info
	ReadMatInfo(); // read material info

	// objects collison
	CheckColliders();

	// script
	g_script.playerUpdate();

	if(customMove())	return; // player was custom moved on the player update handler

	// input status
	if( _status == idle || _status == walk )
	{
		EnterKeyState();
	}

	// states update
	if( _status == idle ) 
		UpdateIdle();
	else
	if( _status == walk )
		UpdateWalk();
	else
	if( _status == jump )
		UpdateJump();
	else
	if( _status == fall )
		UpdateFall();
	else
	if( _status == scripted )
		UpdateScripted();

	if( _status != scripted )
	{
		bool snap = CheckCollidersSnap();

		// stand check only if not already snapped to collider
		if( !snap && (_status == idle || _status == walk) )
		{
			int h = CheckFallY(1); // see if it can fall 
			if(h>0) // if any space below then enter in fall
			{
				EnterFall();
				_y++; 
				_pow++; // force one step down (DIZZYMATCH)
			}
		}
		
		// fix collision by rising dizzy
		CheckCollision();
	}

}




//////////////////////////////////////////////////////////////////////////////////////////////////
// ENTER STATES
// don't seem to need to change tile when enter states
//////////////////////////////////////////////////////////////////////////////////////////////////

void cDizPlayer::EnterIdle()
{
	_status			= idle;
	dir(0);
	pow(0);
	flipX(false);
	int idleTile = _costume+tileIdle()+emotion();
	if( _tile != idleTile )
	{
		_frame = 0;
		_tile = idleTile;
	}

}

void cDizPlayer::EnterWalk( int dr )
{
	_status			= walk; 
	dir(dr);
	pow(0);
	flipX(dir()==-1);
	int walkTile = _costume+tileWalk();
	if( _tile!= walkTile)
	{
		_frame = 0;
		_tile = walkTile;
	}

}

void cDizPlayer::EnterJump( int dr, int pw )
{
	_status			= jump; 
	dir(dr);
	pow(pw);
	flipX(dir()==-1);
	if( dir() )
	{
		int jumpTile = _costume+tileJump();
		if(_tile != jumpTile)
		{
			_frame = 0;
			_tile = jumpTile;
		}
	}
	else
	{ 
		int upTile = _costume+tileUp();
		if(_tile != upTile )
		{	
			_frame = 0;
			_tile = upTile;
		}
	}
	
}

void cDizPlayer::EnterFall()
{
	_status				= fall; 
	pow(1);
	// don't change tile and frame
}

void cDizPlayer::EnterRoll()
{
	
	pow(1); // cut fall power to roll on ground
	
	if( _tile==_costume+tileUp() || _tile==_costume+tileJump() ) // only when jumping
		if(cTile* tile = FindTile())
			if(tile->ComputeFrame(frame(), anim()) != 0) return; // don't enter idle unless last frame reached; untill then stay in roll

	EnterKeyState(); // be sure to stop the fall, just in case the fall handler doesn't

	g_script.fall();
	
	_stunLevel = 0; // clear stun level

}

void cDizPlayer::EnterJumper( int mat )
{
	if(_life == 0) { EnterIdle(); return; } // no more jumps for mr. dead guy

	int dir = 0;

	// direction
	if(KEY(KEY_RIGHT))	dir++;
	if(KEY(KEY_LEFT))	dir--;

	// call jump handler to determine the power of the jump
	int pow = g_script.jump(mat, 0); // send the material, clean return for safety

	if(pow>0)
		EnterJump(dir,pow); // still jumpy
	else
		EnterRoll(); // roll

}

void cDizPlayer::EnterSpin( int dr )
{
	EnterFall();
	dir(dr);
	_tile	= _costume+tileJump();
	flipX(dir()==-1);
	_frame	= 1;
}

void cDizPlayer::EnterKeyState()
{
	if( _life <= 0 ) {	EnterIdle(); return; } // prepare to die

	int dir = 0;
	if( KEY(KEY_RIGHT) )	dir++;
	if( KEY(KEY_LEFT) )		dir--;
	if( KEY(KEY_JUMP) )		
	{
		// call jump handler to determine the power of the jump
		int pow = g_script.jump(-1, 0); // send no material, clean return for safety
		if(pow>0) EnterJump(dir,pow); // 7 would be the default jump power
	}
	else
	if(dir!=0)
	{
		EnterWalk(dir);
	}
	else 
	{
		EnterIdle();
	}
}




//////////////////////////////////////////////////////////////////////////////////////////////////
// UPDATE STATES
//////////////////////////////////////////////////////////////////////////////////////////////////

void cDizPlayer::UpdateIdle()
{

	_frame++;

}

void cDizPlayer::UpdateWalk()
{

	if( CheckWalkX() )
		_x += dir()*DIZ_STEPX;

	_frame++;
	
}

void cDizPlayer::UpdateJump()
{
	
	if( CheckJumpX() )
		_x += dir()*DIZ_STEPX;
	int step = std::min(pow(), DIZ_STEPYMAX);
	step = CheckJumpY(step);
	_y -= step;
	_pow--;

	_frame++;

	if( pow()<0 ) // done jumping - see where to go idle or fall
	{
		int under = CheckFallY(1);
		if(under==0)
			EnterKeyState();
		else
			EnterFall();
	}

}

void cDizPlayer::UpdateFall()
{

	if( CheckFallX() )
		_x += dir()*DIZ_STEPX;

	int step = std::min(pow(), DIZ_STEPYMAX);
	int step2 = CheckFallY(step);
	_y += step2;

	_frame++; // keep last tile
	_stunLevel++;

	// stopping fall if fall step was reduced
	if(step2<step)
	{
		// check for jumpers
		int mat = CheckJumper();
		if( mat>=0 ) // it's a jumper!
			EnterJumper( mat );
		else
			EnterRoll();
	}
	else
		_pow++;

}

void cDizPlayer::UpdateScripted()
{
	
	if(anim()!=0) _frame++;

}


//////////////////////////////////////////////////////////////////////////////////////////////////
// CHECKS
//////////////////////////////////////////////////////////////////////////////////////////////////

// check if rectangle have no hard materials in side
bool cDizPlayer::CheckFree( int x1, int y1, int x2, int y2 )
{
	for(iV2 i(x1, y1);i.y<y2;i.y++)
		for(i.x=x1;i.x<x2;i.x++)
			if( g_game.DensMap(i) == g_game.hard ) 
				return false;
	return true;
}

// check side, only above 8 bottom pixels. if bottom is blocked it will step-up on it
bool cDizPlayer::CheckWalkX()
{
	int x1,y1,x2,y2;
	MakeBB(x1,y1,x2,y2);
	if(dir()==1)
		return CheckFree( x2,y1,x2+DIZ_STEPX,y2-8 );
	else
	if(dir()==-1)
		return CheckFree( x1-DIZ_STEPX,y1,x1,y2-8 );
	else
		return false;
}

bool cDizPlayer::CheckJumpX()
{
	int x1,y1,x2,y2;
	MakeBB(x1,y1,x2,y2);
	if(dir()==1)
		return CheckFree( x2,y1,x2+DIZ_STEPX,y2-8 );
	else
	if(dir()==-1)	
		return CheckFree( x1-DIZ_STEPX,y1,x1,y2-8 );
	else
		return false;
}

// check material above the box and see how far can it go
int cDizPlayer::CheckJumpY( int step )
{
	int x1,y1,x2,y2;
	MakeBB(x1,y1,x2,y2);
	for(iV2 i(x1, y1-1);i.y>y1-1-step;i.y--)
		for(i.x=x1;i.x<x2;i.x++)
			if( g_game.DensMap(i) == g_game.hard ) 
				return ((y1-1)-i.y);
	return step;
}

bool cDizPlayer::CheckFallX()
{
	int x1,y1,x2,y2;
	MakeBB(x1,y1,x2,y2);
	if(dir()==1)	
		return CheckFree( x2,y1,x2+DIZ_STEPX,y2-8 );
	else
	if(dir()==-1)	
		return CheckFree( x1-DIZ_STEPX,y1,x1,y2-8 );
	else
		return false;
}

// check material under the box and see how far can it go
int cDizPlayer::CheckFallY( int step )
{
	int x1,y1,x2,y2;
	MakeBB(x1,y1,x2,y2);
	for( iV2 i(x1, y2); i.y<y2+step; i.y++ )
		for(i.x=x1;i.x<x2;i.x++)
			if( g_game.DensMap(i) != g_game._void ) 
				return (i.y-y2); // return minimized step if block found
	return step;
}

// collision inside box bottom will rise dizzy up with maximum DIZ_STEPY 
void cDizPlayer::CheckCollision()
{
	int x1,y1,x2,y2;
	MakeBB(x1,y1,x2,y2);
	
	// going from up to bottom until the first hard line found
	for( iV2 i(x1, y2-DIZ_STEPY); i.y<y2; i.y++ )
	{
		bool hard = false;
		for(i.x=x1;i.x<x2;i.x++)
		{
			if( g_game.DensMap(i) == g_game.hard ) 
			{
				_y -= y2-i.y;
				return;
			}
		}
	}
	
}

int cDizPlayer::CheckJumper()
{
	int x1,y1,x2,y2;
	MakeBB(x1,y1,x2,y2);
	for(iV2 i(x1, y2); i.x<x2;i.x++)
	{
		int mat = g_game.MatMap(i);
		if( g_game.materials[mat].density == g_game.jump ) return mat;
	}

	return -1;
}


void cDizPlayer::CheckColliders()
{
	iRect pr = bbw();
	for(int idx: g_game.m_collider)
	{
		tBrush & obj = g_map.objects.get(idx);
		if(obj.disable) continue; // only enabled objects
		if(!(obj.collider & COLLIDER_HANDLE)) continue; // just those that request it
		bool collision = pr.Intersects(obj.rect());
		if(!collision && !obj.collision) continue; // no collision event

		// call

		enum collMode { exit = 0, enter = 1, keep = 2};
		g_script.collision(obj.id, !collision ? exit : obj.collision ? keep : enter);   // entering collision

		obj.collision = collision;
	}

}

bool cDizPlayer::CheckCollidersSnap()
{
	int x1,y1,x2,y2;
	MakeBBW(x1,y1,x2,y2);

	int stepu=0;			// move player up (deepest entering collider)
	int stepd=DIZ_STEPY+1;	// move player down (space below to the closest collider)

	bool snap=false;

	for(int idx: g_game.m_collider)
	{
		tBrush & obj = g_map.objects.get(idx);
		if(obj.disable) continue; // only enabled objects
		if(!(obj.collider & COLLIDER_HARD)) continue; // only those that need it
		iRect c = obj.rect();
		if( x2<=c.p1.x || x1>=c.p2.x ) continue;
		
		if(y2<=c.p1.y && c.p1.y < y2 + DIZ_STEPY + 1)	// collider's top is inside player's box
			stepd = std::min(stepd, c.p1.y - y2);

		if(y1<=c.p1.y && c.p1.y<y2)				// collider's top is not too far under the player's bottom ( a smaller interval would be (y2-DIZ_STEPY<=cy1 && cy1<y2) )
			stepu = std::max(stepu, y2 - c.p1.y);
	}	

	if(stepu>0)
	{
		snap = true;
		_y -= std::min(stepu, DIZ_STEPY);
	}
	else
	if(stepd<=DIZ_STEPY)
	{
		int stepd2 = CheckFallY(stepd);
		if(stepd2==stepd)
		{
			snap = true;
			_y += stepd;
		}
	}

	if(snap && _status == fall) // stop falls
	{
		EnterRoll();
	}

	return snap;
}

//////////////////////////////////////////////////////////////////////////////////////////////////
// MATERIALS INFO
//////////////////////////////////////////////////////////////////////////////////////////////////
void cDizPlayer::ReadMatInfo()
{
	iV2 p1, p2;
	MakeBB(p1.x,p1.y,p2.x,p2.y);
	
	_matInside = 0;
	for(int y=p1.y;y<p2.y;y++)
		_matInside |= g_game.MatMap(p1.x, p2.x, y);
	_matUnder = g_game.MatMap(p1.x, p2.x, p2.y);
	_matCenter = g_game.MatMap((p1 + p2) / 2);
}


//////////////////////////////////////////////////////////////////////////////////////////////////
// DRAW
//////////////////////////////////////////////////////////////////////////////////////////////////
void cDizPlayer::Draw()
{
	if( disable()) return; // disabled
	if(cTile* tile = FindTile())
	{
		iV2 sz = tile->GetSize();
		R9_SetBlend(shader());
		iV2 p = pos() - Room::Pos2Room(pos()) * Room::Size + iV2(-sz.x/2, h()/2 - sz.y) + g_game.viewShift;
		R9_DrawSprite(g_paint.scrPos(p), tile->FrameRect(tile->ComputeFrame(frame(), anim())), tile->tex, color(), flip(), static_cast<float>(g_paint.scale()));
	}
}

//////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////
