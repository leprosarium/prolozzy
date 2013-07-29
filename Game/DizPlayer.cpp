//////////////////////////////////////////////////////////////////////////////////////////////////
// DizPlayer.cpp
//////////////////////////////////////////////////////////////////////////////////////////////////
#include "stdafx.h"
#include "DizPlayer.h"
#include "DizGame.h"

cDizPlayer	g_player;

inline bool cDizPlayer::key(int k) const { return input ? g_game.Key(k) : false; }
inline bool cDizPlayer::keyHit(int k) const { return input ? g_game.KeyHit(k) : false; }

iRect cDizPlayer::rect() const
{ 
	return worldRect().Offset(-g_game.roomPos * Room::Size);
}

#define PLAYER_GET(Prop, Pr) PREDICATE_M(player, Prop, 1) { return A1 = g_player.Pr; }
#define PLAYER_SET(Prop, Pr) PREDICATE_M(player, set##Prop, 1) { g_player.Pr = A1; return true; }
#define PLAYER_PROP(PropGet, PropSet, Pr) PLAYER_GET(PropGet, Pr) PLAYER_SET(PropSet, Pr)

PLAYER_PROP(x, X, pos.x)
PLAYER_PROP(y, Y, pos.y)
PLAYER_PROP(life, Life, life)
PLAYER_PROP(tile, Tile, tile)
PLAYER_PROP(frame, Frame, frame)
PLAYER_PROP(costume, Costume, costume)
PLAYER_PROP(emotion, Emotion, emotion)
PLAYER_PROP(stunLevel, StunLevel, stunLevel)
PLAYER_PROP(delay, Delay, delay)
PLAYER_PROP(layer, Layer, layer)
PLAYER_PROP(dir, Dir, dir)
PLAYER_PROP(tileIdle, TileIdle, tileIdle)
PLAYER_PROP(tileWalk, TileWalk, tileWalk)
PLAYER_PROP(tileUp, TileUp, tileUp)
PLAYER_PROP(tileJump, TileJump, tileJump)
PLAYER_PROP(pow, Pow, pow)
PLAYER_PROP(anim, Anim, anim)

PLAYER_GET(w, size.x)
PLAYER_GET(h, size.y)

PLAYER_GET(status, status)

PREDICATE_M(player, setStatus, 1)
{
	g_player.status = PlAtom(A1);
	return true;
}

PLAYER_GET(matInside, matInside())
PLAYER_GET(matUnder, matUnder())
PLAYER_GET(matCenter, matCenter())

PREDICATE_M(player, color, 1)
{
	return A1 = static_cast<int>(g_player.color);
}

PREDICATE_M(player, setColor, 1)
{
	int64 v = A1;
	g_player.color = static_cast<dword>(v); 
	return true;
}

PREDICATE_M(player, shader, 1)
{
	return A1 = static_cast<int>(g_player.shader);
}

PREDICATE_M(player, setShader, 1)
{
	int v = A1;
	if (v <  static_cast<int>(Blend::Min) || 
		v >= static_cast<int>(Blend::Max))
		return false;
	g_player.shader = static_cast<Blend>(v); 
	return true;
}

#define PLAYER_B_GET(Prop) PREDICATE_M(player, Prop, 0) { return g_player.Prop; }
#define PLAYER_B_SET(Prop, Pr) PREDICATE_M(player, set##Prop, 1) { int v; if(!PL_get_bool(A1, &v)) return false; g_player.Pr = v == TRUE; return true; }
#define PLAYER_B_PROP(Prop, PropSet) PLAYER_B_GET(Prop) PLAYER_B_SET(PropSet, Prop)

PLAYER_B_PROP(flipX, FlipX)
PLAYER_B_PROP(flipY, FlipY)
PLAYER_B_PROP(disable, Disable)
PLAYER_B_PROP(customMove, CustomMove)

PREDICATE_M(player, makeBB, 4)
{
	iRect r = g_player.rect();
	A1 = r.p1.x;
	A2 = r.p1.y;
	A3 = r.p2.x;
	A4 = r.p2.y;
	return true;
}

PREDICATE_M(player, makeBBW, 4)
{
	iRect r = g_player.worldRect();
	A1 = r.p1.x;
	A2 = r.p1.y;
	A3 = r.p2.x;
	A4 = r.p2.y;
	return true;
}

//////////////////////////////////////////////////////////////////////////////////////////////////
// INIT
//////////////////////////////////////////////////////////////////////////////////////////////////

cDizPlayer::cDizPlayer() : idle("idle"), walk("walk"), jump("jump"), fall("fall"), scripted("scripted"), status(idle), size(16, 20)
{
	Reset();
}

void cDizPlayer::Reset()
{
	shader = Blend::Alpha;
	status = idle;
	delay = 3;
	layer = 7;

	tile = TILE_IDLE;
	anim = 2;
	color = 0xffffffffu;

	tileIdle = TILE_IDLE;
	tileWalk = TILE_WALK;
	tileUp = TILE_UP;
	tileJump = TILE_JUMP;

	life = 100;

	input = true;
	m_debug	= false;
}


//////////////////////////////////////////////////////////////////////////////////////////////////
// UPDATE
//////////////////////////////////////////////////////////////////////////////////////////////////

void cDizPlayer::Update()
{
	if(disable || m_debug) return;

	// delay
	if( !g_game.IsUpdate(delay) ) return;

	ReadMatInfo();
	CheckColliders();
	g_script.playerUpdate();
	if(customMove)	return; // player was custom moved on the player update handler

	// input status
	if(status == idle || status == walk)
		EnterKeyState();

	if(status == idle) UpdateIdle();
	else if(status == walk) UpdateWalk();
	else if(status == jump) UpdateJump();
	else if(status == fall) UpdateFall();
	else if(status == scripted) UpdateScripted();

	if(status != scripted)
	{
		// stand check only if not already snapped to collider and if any space below then enter in fall
		if( !CheckCollidersSnap() && (status == idle || status == walk) && CheckFallY(1)>0)
		{
			EnterFall();
			pos.y++; 
			pow++; // force one step down
		}
		CheckCollision();// fix collision by rising dizzy
	}
}

//////////////////////////////////////////////////////////////////////////////////////////////////
// ENTER STATES
// don't seem to need to change tile when enter states
//////////////////////////////////////////////////////////////////////////////////////////////////

void cDizPlayer::EnterIdle()
{
	status = idle;
	setDir(0);
	pow = 0;
	int idleTile = costume + tileIdle + emotion;
	if(tile != idleTile)
	{
		frame = 0;
		tile = idleTile;
	}

}

void cDizPlayer::EnterWalk( int dr )
{
	status = walk; 
	setDir(dr);
	pow = 0;
	int walkTile = costume + tileWalk;
	if(tile!= walkTile)
	{
		frame = 0;
		tile = walkTile;
	}

}

void cDizPlayer::EnterJump( int dr, int pw )
{
	status = jump; 
	setDir(dr);
	pow = pw;
	if(dir)
	{
		int jumpTile = costume + tileJump;
		if(tile != jumpTile)
		{
			frame = 0;
			tile = jumpTile;
		}
	}
	else
	{ 
		int upTile = costume + tileUp;
		if(tile != upTile)
		{	
			frame = 0;
			tile = upTile;
		}
	}
	
}

void cDizPlayer::EnterFall()
{
	status = fall; 
	pow = 1;
	// don't change tile and frame
}

void cDizPlayer::EnterRoll()
{
	pow = 1; // cut fall power to roll on ground
	
	if(tile == costume + tileUp || tile == costume + tileJump ) // only when jumping
		if(cTile* tile = FindTile())
			if(tile->ComputeFrame(frame, anim) != 0) return; // don't enter idle unless last frame reached; untill then stay in roll

	EnterKeyState(); // be sure to stop the fall, just in case the fall handler doesn't

	g_script.fall();
	
	stunLevel = 0; // clear stun level

}

void cDizPlayer::EnterJumper( int mat )
{
	if(!life) { EnterIdle(); return; } // no more jumps for mr. dead guy

	int dir = 0;

	// direction
	if(key(cDizCfg::key::right))	dir++;
	if(key(cDizCfg::key::left))	dir--;

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
	setDir(dr);
	tile = costume + tileJump;
	frame = 1;
}

void cDizPlayer::EnterKeyState()
{
	if( life <= 0 ) {	EnterIdle(); return; } // prepare to die

	int dir = 0;
	if(key(cDizCfg::key::right))	dir++;
	if(key(cDizCfg::key::left))	dir--;
	if(key(cDizCfg::key::jump))		
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
	frame++;
}

void cDizPlayer::UpdateWalk()
{
	if( CheckWalkX() )
		pos.x += dir * DIZ_STEPX;
	frame++;	
}

void cDizPlayer::UpdateJump()
{
	if( CheckJumpX() )
		pos.x += dir * DIZ_STEPX;
	int step = std::min(pow, DIZ_STEPYMAX);
	step = CheckJumpY(step);
	pos.y -= step;
	pow--;

	frame++;

	if(pow < 0) // done jumping - see where to go idle or fall
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
		pos.x += dir * DIZ_STEPX;

	int step = std::min(pow, DIZ_STEPYMAX);
	int step2 = CheckFallY(step);
	pos.y += step2;

	frame++; // keep last tile
	stunLevel++;

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
		pow++;

}

void cDizPlayer::UpdateScripted()
{
	if(anim) frame++;
}


//////////////////////////////////////////////////////////////////////////////////////////////////
// CHECKS
//////////////////////////////////////////////////////////////////////////////////////////////////

// check if rectangle have no hard materials in side
bool cDizPlayer::CheckFree(const iRect & r) const
{
	for(iV2 i(r.p1); i.y < r.p2.y; i.y++)
		for(i.x = r.p1.x; i.x < r.p2.x; i.x++)
			if( g_game.DensMap(i) == g_game.hard ) 
				return false;
	return true;
}

// check side, only above 8 bottom pixels. if bottom is blocked it will step-up on it
bool cDizPlayer::CheckWalkX() const
{
	iRect r = rect();
	if(dir == 1)
		return CheckFree( iRect(r.RightUp(), r.RightDown() + iV2(DIZ_STEPX, -8)) );
	if(dir == -1)
		return CheckFree( iRect(r.LeftUp() - iV2(DIZ_STEPX, 0), r.LeftDown() - iV2(0, 8)));
	return false;
}

// check material above the box and see how far can it go
int cDizPlayer::CheckJumpY(int step) const
{
	iRect r = rect();
	for(iV2 i(r.p1.x, r.p1.y-1); i.y > r.p1.y - 1 - step; i.y--)
		for(i.x = r.p1.x; i.x < r.p2.x; i.x++)
			if( g_game.DensMap(i) == g_game.hard ) 
				return ((r.p1.y-1)-i.y);
	return step;
}

// check material under the box and see how far can it go
int cDizPlayer::CheckFallY(int step) const
{
	iRect r = rect();
	for( iV2 i(r.LeftDown()); i.y < r.p2.y + step; i.y++ )
		for(i.x = r.p1.x; i.x < r.p2.x; i.x++)
			if( g_game.DensMap(i) != g_game._void ) 
				return (i.y - r.p2.y); // return minimized step if block found
	return step;
}

// collision inside box bottom will rise dizzy up with maximum DIZ_STEPY 
void cDizPlayer::CheckCollision()
{
	iRect r = rect();	
	// going from up to bottom until the first hard line found
	for( iV2 i = r.LeftDown() - iV2(0, DIZ_STEPY); i.y < r.p2.y; i.y++ )
		for(i.x = r.p1.x; i.x < r.p2.x;i.x++)
			if( g_game.DensMap(i) == g_game.hard ) 
			{
				pos.y -= r.p2.y-i.y;
				return;
			}

}

int cDizPlayer::CheckJumper()
{
	iRect r = rect();	
	for(iV2 i(r.LeftDown()); i.x < r.p2.x; i.x++)
	{
		int mat = g_game.MatMap(i);
		if( g_game.materials[mat].density == g_game.jump ) return mat;
	}

	return -1;
}


void cDizPlayer::CheckColliders()
{
	iRect pr = worldRect();
	for(auto obj: g_game.m_collider)
	{
		if(obj->disable) continue; // only enabled objects
		if(!(obj->collider & COLLIDER_HANDLE)) continue; // just those that request it
		bool collision = pr.Intersects(obj->rect());
		if(!collision && !obj->collision) continue; // no collision event
		enum collMode { exit = 0, enter = 1, keep = 2};
		g_script.collision(obj->id, !collision ? exit : obj->collision ? keep : enter);   // entering collision
		obj->collision = collision;
	}
}

bool cDizPlayer::CheckCollidersSnap()
{
	iRect r = worldRect();

	int stepu=0;			// move player up (deepest entering collider)
	int stepd=DIZ_STEPY+1;	// move player down (space below to the closest collider)

	bool snap=false;

	for(auto obj: g_game.m_collider)
	{
		if(obj->disable) continue; // only enabled objects
		if(!(obj->collider & COLLIDER_HARD)) continue; // only those that need it
		iRect c = obj->rect();
		if( r.p2.x <= c.p1.x || r.p1.x >= c.p2.x ) continue;
		
		if(r.p2.y <= c.p1.y && c.p1.y < r.p2.y + DIZ_STEPY + 1)	// collider's top is inside player's box
			stepd = std::min(stepd, c.p1.y - r.p2.y);

		if(r.p1.y <= c.p1.y && c.p1.y < r.p2.y)				// collider's top is not too far under the player's bottom ( a smaller interval would be (y2-DIZ_STEPY<=cy1 && cy1<y2) )
			stepu = std::max(stepu, r.p2.y - c.p1.y);
	}	

	if(stepu>0)
	{
		snap = true;
		pos.y -= std::min(stepu, DIZ_STEPY);
	}
	else
	if(stepd<=DIZ_STEPY)
	{
		int stepd2 = CheckFallY(stepd);
		if(stepd2==stepd)
		{
			snap = true;
			pos.y += stepd;
		}
	}

	if(snap && status == fall) // stop falls
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
	iRect r = rect();
	
	_matInside = 0;
	for(int y=r.p1.y; y < r.p2.y; y++)
		_matInside |= g_game.MatMap(r.p1.x, r.p2.x, y);
	_matUnder = g_game.MatMap(r.p1.x, r.p2.x, r.p2.y);
	_matCenter = g_game.MatMap(r.Center());
}


//////////////////////////////////////////////////////////////////////////////////////////////////
// DRAW
//////////////////////////////////////////////////////////////////////////////////////////////////
void cDizPlayer::Draw()
{
	if(disable) return;
	if(cTile* tile = FindTile())
	{
		iV2 sz = tile->GetSize();
		R9_SetBlend(shader);
		iV2 p = pos - Room::Pos2Room(pos) * Room::Size + iV2(-sz.x/2, size.y/2 - sz.y) + g_game.viewShift;
		R9_DrawSprite(g_paint.scrPos(p), tile->FrameRect(tile->ComputeFrame(frame, anim)), tile->tex, color, flip(), static_cast<float>(g_paint.scale));
	}
}

//////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////
