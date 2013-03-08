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
	int rx = g_game.roomX() * Room::Width;	
	int ry = g_game.roomY() * Room::Height;	
	MakeBBW(x1,y1,x2,y2);			
	x1 -= rx;			
	x2 -= rx;			
	y1 -= ry;			
	y2 -= ry;			
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
	int64 v;
	if(!PL_get_int64(A1, &v))
		return false; 
	g_player.color(v); 
	return true;
}

PREDICATE_M(player, shader, 1)
{
	return A1 = g_player.shader();
}

PREDICATE_M(player, setShader, 1)
{
	int v;
	if(!PL_get_integer(A1, &v))
		return false; 
	g_player.shader(v); 
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
	int rx = g_game.roomX();
	int ry = g_game.roomY();
	int x1, x2, y1, y2;
	g_player.MakeBBW(x1, y1, x2, y2);
	A1 = x1 - rx * Room::Width;
	A2 = y1 - ry * Room::Height;
	A3 = x2 - rx * Room::Width;
	A4 = y2 - ry * Room::Height;
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

	shader(SHADER_BLEND);
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
	{
		cTile* tile = FindTile();
		if(tile)
		{
			int frame = ComputeFrame( _frame, tile->m_frames, anim() );
			//if( frame < tile->m_frames-1 ) return; // don't enter idle unless last frame reached; untill then stay in roll
			if( frame != 0 ) return; // don't enter idle unless last frame reached; untill then stay in roll
		}
	}

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
	int step = MIN(pow(),DIZ_STEPYMAX);
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

	int step = MIN(pow(),DIZ_STEPYMAX);
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
	for(int iy=y1;iy<y2;iy++)
		for(int ix=x1;ix<x2;ix++)
			if( g_game.DensMap(ix,iy) == g_game.hard ) 
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
	for(int iy=y1-1;iy>y1-1-step;iy--)
	{
		for(int ix=x1;ix<x2;ix++)
			if( g_game.DensMap(ix,iy) == g_game.hard ) 
				return ((y1-1)-iy);
	}
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
	for( int iy=y2; iy<y2+step; iy++ )
		for(int ix=x1;ix<x2;ix++)
			if( g_game.DensMap(ix,iy) != g_game._void ) 
				return (iy-y2); // return minimized step if block found
	return step;
}

// collision inside box bottom will rise dizzy up with maximum DIZ_STEPY 
void cDizPlayer::CheckCollision()
{
	int x1,y1,x2,y2;
	MakeBB(x1,y1,x2,y2);
	
	// going from up to bottom until the first hard line found
	for( int iy=y2-DIZ_STEPY; iy<y2; iy++ )
	{
		bool hard = false;
		for(int ix=x1;ix<x2;ix++)
		{
			if( g_game.DensMap(ix,iy) == g_game.hard ) 
			{
				_y -= y2-iy;
				return;
			}
		}
	}
	
}

int cDizPlayer::CheckJumper()
{
	int x1,y1,x2,y2;
	MakeBB(x1,y1,x2,y2);

	for(int ix=x1;ix<x2;ix++)
	{
		int mat = g_game.MatMap(ix,y2);
		if( g_game.materials[mat].density == g_game.jump ) return mat;
	}

	return -1;
}


void cDizPlayer::CheckColliders()
{
	int x1,y1,x2,y2;
	int cx1,cy1,cx2,cy2;
	MakeBBW(x1,y1,x2,y2);
	for(std::vector<int>::const_iterator i = g_game.m_collider.begin(), e = g_game.m_collider.end(); i != e; ++i)
	{
		tBrush & obj = g_map.ObjGet(*i);
		if( obj.Get(BRUSH_DISABLE)!=0 ) continue; // only enabled objects
		if(!(obj.Get(BRUSH_COLLIDER) & COLLIDER_HANDLE)) continue; // just those that request it
		cx1 = obj.Get(BRUSH_X);
		cy1 = obj.Get(BRUSH_Y);
		cx2 = cx1+obj.Get(BRUSH_W);
		cy2 = cy1+obj.Get(BRUSH_H);

		bool collision = !( x2<=cx1 || x1>=cx2 || y2<=cy1 || y1>=cy2 );
		if(!collision && !obj.Get(BRUSH_COLLISION)) continue; // no collision event

		// call

		enum collMode { exit = 0, enter = 1, keep = 2};
		g_script.collision(*i, !collision ? exit : obj.Get(BRUSH_COLLISION) ? keep : enter);   // entering collision

		obj.Set(BRUSH_COLLISION,collision);
	}

}

bool cDizPlayer::CheckCollidersSnap()
{
	int x1,y1,x2,y2;
	int cx1,cy1,cx2,cy2;
	MakeBBW(x1,y1,x2,y2);

	int stepu=0;			// move player up (deepest entering collider)
	int stepd=DIZ_STEPY+1;	// move player down (space below to the closest collider)

	bool snap=false;

	for(std::vector<int>::const_iterator i = g_game.m_collider.begin(), e = g_game.m_collider.end(); i != e; ++i)
	{
		tBrush & obj = g_map.ObjGet(*i);
		if( obj.Get(BRUSH_DISABLE)!=0 ) continue; // only enabled objects
		if(!(obj.Get(BRUSH_COLLIDER) & COLLIDER_HARD)) continue; // only those that need it
		obj.MakeBBW(cx1,cy1,cx2,cy2);
		if( x2<=cx1 || x1>=cx2 ) continue;
		
		if(y2<=cy1 && cy1<y2+DIZ_STEPY+1)	// collider's top is inside player's box
			stepd = MIN(stepd,cy1-y2);

		if(y1<=cy1 && cy1<y2)				// collider's top is not too far under the player's bottom ( a smaller interval would be (y2-DIZ_STEPY<=cy1 && cy1<y2) )
			stepu = MAX(stepu,y2-cy1);
	}	

	if(stepu>0)
	{
		snap = true;
		_y -= MIN(stepu,DIZ_STEPY);
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
	int x1,y1,x2,y2;
	MakeBB(x1,y1,x2,y2);
	
	// inside
	_matInside = 0;
	for(int y=y1;y<y2;y++)
	{
		for(int x=x1;x<x2;x++)
		{
			_matInside |= (1<<g_game.MatMap(x,y));
		}
	}	

	// under
	_matUnder = 0;
	for(int x=x1;x<x2;x++)
	{
		_matUnder |= (1<<g_game.MatMap(x,y2));
	}

	// center
	_matCenter = g_game.MatMap((x1+x2)/2,(y1+y2)/2);

}


//////////////////////////////////////////////////////////////////////////////////////////////////
// DRAW
//////////////////////////////////////////////////////////////////////////////////////////////////
void cDizPlayer::Draw()
{
	
	if( disable()) return; // disabled

	int rx = Room::PosX2Room(_x);
	int ry = Room::PosY2Room(_y);
	
	// tile	
	cTile* tile = FindTile();
	if(tile)
	{
		int frame = ComputeFrame(_frame,tile->m_frames,anim());
		int blend = shader();
		int w = tile->GetWidth();
		int h = tile->GetHeight();
		int x = _x-rx*Room::Width - w/2; // @TODO need -1 to the MatchX offset because of the 25 vs 24 width bla bla
		int y = _y-ry*Room::Height + _h/2 - h;
		x += g_game.m_viewx;
		y += g_game.m_viewy;
		fRect src;
		int fx = tile->GetFx(frame);
		int fy = tile->GetFy(frame);
		src.x1 = (float)(fx*w);
		src.x2 = (float)((fx+1)*w);
		src.y1 = float(fy * h);
		src.y2 = float((fy + 1) * h);
		fV2 pos(SCALEX(x), SCALEY(y));
		R9_SetState(R9_STATE_BLEND,blend);
		dword flip = ((flipX() ? 1 : 0 ) | (flipY() ? 2 : 0));
		R9_DrawSprite( pos, src, tile->m_tex, color(), flip, (float)SCALE );
	}

}



//////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////
