//////////////////////////////////////////////////////////////////////////////////////////////////
// DizGame.cpp
//////////////////////////////////////////////////////////////////////////////////////////////////
#include "stdafx.h"
#include "DizGame.h"
#include "DizApp.h"	// for error msg
#include "E9App.h"
#include <algorithm>

PREDICATE_M(game, frame, 1)
{
	return A1 = g_game.m_gameframe;
}

PREDICATE_M(game, command, 1)
{
	g_game.command(PlAtom(A1));
	return true;
}

PREDICATE_M(game, setName, 1)
{
	App.Name(A1);
	return true;
}


PREDICATE_M(game, fps, 1)
{
	return A1 = g_game.fps();
}

PREDICATE_M(game, setFps, 1)
{
	int v;
	if(!PL_get_integer(A1, &v))
		return false; 
	g_game.fps(v); 
	return true;
}

PREDICATE_M(game, keys, 1)
{
	return A1 = g_game.keys();
}

PREDICATE_M(game, setKeys, 1)
{
	int v;
	if(!PL_get_integer(A1, &v))
		return false; 
	g_game.keys(v); 
	return true;
}

PREDICATE_M(game, keysHit, 1)
{
	return A1 = g_game.keysHit();
}

PREDICATE_M(game, setKeysHit, 1)
{
	int v;
	if(!PL_get_integer(A1, &v))
		return false; 
	g_game.keysHit(v); 
	return true;
}

PREDICATE_M(game, roomX, 1)
{
	return A1 = g_game.roomX();
}

PREDICATE_M(game, setRoomX, 1)
{
	int v;
	if(!PL_get_integer(A1, &v))
		return false; 
	g_game.roomX(v); 
	return true;
}
PREDICATE_M(game, roomY, 1)
{
	return A1 = g_game.roomY();
}

PREDICATE_M(game, setRoomY, 1)
{
	int v;
	if(!PL_get_integer(A1, &v))
		return false; 
	g_game.roomY(v); 
	return true;
}

PREDICATE_M(game, viewX, 1)
{
	return A1 = g_game.viewX();
}

PREDICATE_M(game, setViewX, 1)
{
	int v;
	if(!PL_get_integer(A1, &v))
		return false; 
	g_game.viewX(v); 
	return true;
}
PREDICATE_M(game, viewY, 1)
{
	return A1 = g_game.viewY();
}

PREDICATE_M(game, setViewY, 1)
{
	int v;
	if(!PL_get_integer(A1, &v))
		return false; 
	g_game.viewY(v); 
	return true;
}
PREDICATE_M(game, shakeX, 1)
{
	return A1 = g_game.shakeX();
}

PREDICATE_M(game, setShakeX, 1)
{
	int v;
	if(!PL_get_integer(A1, &v))
		return false; 
	g_game.shakeX(v); 
	return true;
}
PREDICATE_M(game, shakeY, 1)
{
	return A1 = g_game.shakeY();
}

PREDICATE_M(game, setShakeY, 1)
{
	int v;
	if(!PL_get_integer(A1, &v))
		return false; 
	g_game.shakeY(v); 
	return true;
}

PREDICATE_M(game, paused, 0)
{
	return g_game.pause();
}

PREDICATE_M(game, pause, 0)
{
	g_game.pause(true); 
	return true;
}

PREDICATE_M(game, unpause, 0)
{
	g_game.pause(false); 
	return true;
}


PREDICATE_M(game, mapColor, 1)
{
	return A1 = static_cast<int>(g_game.mapColor());
}

PREDICATE_M(game, setMapColor, 1)
{
	int64 v = A1;
	g_game.mapColor(static_cast<int>(v)); 
	return true;
}

PREDICATE_M(game, borderColor, 1)
{
	return A1 = static_cast<int>(g_game.borderColor());
}

PREDICATE_M(game, setBorderColor, 1)
{
	int64 v = A1;
	g_game.borderColor(static_cast<int>(v)); 
	return true;
}

PREDICATE_M(game, ffMagnitude, 1)
{
	return A1 = g_game.FFmagnitude();
}

PREDICATE_M(game, setFFMagnitude, 1)
{
	int v;
	if(!PL_get_integer(A1, &v))
		return false; 
	g_game.FFmagnitude(v); 
	return true;
}

PREDICATE_M(game, ffPeriod, 1)
{
	return A1 = g_game.FFperiod();
}

PREDICATE_M(game, setFFPeriod, 1)
{
	int v;
	if(!PL_get_integer(A1, &v))
		return false; 
	g_game.FFperiod(v); 
	return true;
}

PREDICATE_M(game, viewportMode, 0)
{
	return g_game.viewportMode();
}

PREDICATE_M(game, setViewportMode, 1)
{
	int v;
	if(!PL_get_bool(A1, &v))
		return false; 
	g_game.viewportMode(v != FALSE); 
	return true;
}
PREDICATE_M(game, viewportX, 1)
{
	return A1 = g_game.viewportX();
}

PREDICATE_M(game, setViewportX, 1)
{
	int v;
	if(!PL_get_integer(A1, &v))
		return false; 
	g_game.viewportX(v); 
	return true;
}
PREDICATE_M(game, viewportY, 1)
{
	return A1 = g_game.viewportY();
}

PREDICATE_M(game, setViewportY, 1)
{
	int v;
	if(!PL_get_integer(A1, &v))
		return false; 
	g_game.viewportY(v); 
	return true;
}

PREDICATE_M(game, viewportFlipX, 0)
{
	return g_game.viewportFlipX();
}

PREDICATE_M(game, setViewportFlipX, 1)
{
	int v;
	if(!PL_get_bool(A1, &v))
		return false; 
	g_game.viewportFlipX(v != FALSE); 
	return true;
}
PREDICATE_M(game, viewportFlipY, 0)
{
	return g_game.viewportFlipY();
}

PREDICATE_M(game, setViewportFlipY, 1)
{
	int v;
	if(!PL_get_bool(A1, &v))
		return false; 
	g_game.viewportFlipY(v != FALSE); 
	return true;
}

PREDICATE_M(game, fullMaterialMap, 0)
{
	return g_game.fullMaterialMap();
}

PREDICATE_M(game, setFullMaterialMap, 1)
{
	int v;
	if(!PL_get_bool(A1, &v))
		return false; 
	g_game.fullMaterialMap(v != FALSE); 
	return true;
}

//////////////////////////////////////////////////////////////////////////////////////////////////
// INIT
//////////////////////////////////////////////////////////////////////////////////////////////////
cDizGame g_game;




cDizGame::cDizGame() : 	_void("void"),	soft("soft"), hard("hard"), jump("jump"), none("none"), start("start"), exit("exit"), refresh("refresh"),
						screenSize(GAME_SCRW, GAME_SCRH),
						screenSizeBorder(GAME_SCRWB, GAME_SCRH),
						_command(none), 
						_pause(),
						_fps(GAME_FPS),
						_keys(),
						_keysHit(),
						_viewX(GAME_VIEWX), _viewY(GAME_VIEWY),
						_shakeX(), _shakeY(),
						_mapColor(),
						_borderColor(),
						_FFmagnitude(),
						_FFperiod(),
						_viewportMode(),
						_viewportX(), _viewportY(),
						_viewportFlipX(), _viewportFlipY(),
						_fullMaterialMap()
{
	m_gameframe		= 0;
	m_drawmode		= DRAWMODE_NORMAL;
				
	m_fffx_magnitude	= 0;
	m_fffx_period		= 50;

	m_visible_brushes	= 0;
}

cDizGame::~cDizGame()
{
}

bool cDizGame::Init()
{

	fps(GAME_FPS);

	// load game resolution settings from inf
	sscanf( g_cfg.GetInfoValue( "game_screen_bw" ), "%i", & screenSizeBorder.x );
	sscanf( g_cfg.GetInfoValue( "game_screen_bh" ), "%i", & screenSizeBorder.y );
	sscanf( g_cfg.GetInfoValue( "game_screen_w" ), "%i", & screenSize.x );
	sscanf( g_cfg.GetInfoValue( "game_screen_h" ), "%i", & screenSize.y );
	g_paint.Layout(); // refresh layout

	return CheckVersion();
}

void cDizGame::Done()
{
	m_obj.clear();
	m_collider.clear();
	//m_dlg.Done();
}

bool cDizGame::Start()
{

	// map reset
	g_map.Reset();

	// game
	m_gameframe = 0;
	m_collider.clear();
//	memset(&m_data[G_CFG_MAX], 0, sizeof(int)*(G_MAX-G_CFG_MAX)); // keep cfg
//	FFperiod(50);
//	Set(G_PAUSE,1); // start paused

	// player
	g_player.Reset();
	g_player.x(0);
	g_player.y(0);
	g_player.disable(true);

	// fffx
	if(I9_IsReady()) FFFXStop();

	// room
	SetRoom( 0, 0 );

	// script
	g_script.Start();

	return true;
}

bool cDizGame::CheckVersion()
{
	// requested version must be in the folowing formats: 2.0, 2.01, 2.01b, etc
	// the match is done on first 2 digits (2.0)
	const char* reqv = g_cfg.GetInfoValue("dizzyage_version");
	char* engv = GAME_VERSION;
	if(strlen(reqv)>=3) // enough digits
	{
		if( reqv[0]==engv[0] && reqv[1]==engv[1] && reqv[2]==engv[2] ) return true; // match
		sys_msgbox( E9_GetHWND(), swprint(L"This game was made with DizzyAGE v%S\nBut, you are running it with DizzyAGE v%S\nIf you experience malfunctions, contact the author.",reqv,engv), L"WARNING",MB_OK );
	}
	else
	{
		sys_msgbox( E9_GetHWND(), swprint(L"This game doesn't specify the version of DizzyAGE it was made for.\nYou are running it with DizzyAGE v%S\nIf you experience malfunctions, contact the author.",engv), L"WARNING",MB_OK );
	}
	return true;
}

//////////////////////////////////////////////////////////////////////////////////////////////////
// UPDATE
//////////////////////////////////////////////////////////////////////////////////////////////////
bool cDizGame::Update()
{

	int i;

	// command process
	if(command()==start) // start game
	{
		if(!Start()) return false;
	}
	else
	if(command()==exit) // exit game
	{
		return false;
	}
	if(command()==refresh) // refresh room
	{
		matMap.Update(roomPos(), fullMaterialMap());
	}
	command(none);

	// game frame
	m_gameframe++;

	// key input
	int ks = 0;
	static int oldkeys = 0;

	if(I9_IsReady())
	{
		for(i=0;i<KEY_MAX;i++)
		{
			if( I9_GetKeyValue(g_cfg.m_key[i][0]) ||
				I9_GetKeyValue(g_cfg.m_key[i][1]) )	
				ks |= (1<<i);
		}
		
		// joystick bogus values are higher on y because up can be used as jump...
		int dzx = g_cfg.m_deadzone[0];
		int dzy = g_cfg.m_deadzone[1];
		int jax = I9_JOY_FIRSTAXE(0)+g_cfg.m_joy[4];
		int jay = I9_JOY_FIRSTAXE(0)+g_cfg.m_joy[5];
		int	jb0 = I9_JOY_FIRSTKEY(0)+g_cfg.m_joy[0];
		int	jb1 = I9_JOY_FIRSTKEY(0)+g_cfg.m_joy[1];
		int	jb2 = I9_JOY_FIRSTKEY(0)+g_cfg.m_joy[2];

		if( I9_GetAxeValue(jax)<-dzx || I9_GetJoystickHAT(0,I9_HUT_LEFT)  )	ks |= (1<<KEY_LEFT);
		if( I9_GetAxeValue(jax)> dzx || I9_GetJoystickHAT(0,I9_HUT_RIGHT) )	ks |= (1<<KEY_RIGHT);
		if( I9_GetAxeValue(jay)<-dzy || I9_GetJoystickHAT(0,I9_HUT_UP)    ) ks |= (1<<KEY_UP);
		if( I9_GetAxeValue(jay)> dzy || I9_GetJoystickHAT(0,I9_HUT_DOWN)  )	ks |= (1<<KEY_DOWN);
		if( I9_GetKeyValue(jb1) )											ks |= (1<<KEY_JUMP);		// xbox360 B
		if( I9_GetKeyValue(jb0) )											ks |= (1<<KEY_ACTION);	// xbox360 A
		if( I9_GetKeyValue(jb2) )											ks |= (1<<KEY_MENU);		// xbox360 X
	}

	// avoid keys during debug
	if(g_player.m_debug) ks=0;

	keys(ks);
	int keyshit = (ks ^ oldkeys) & ks;
	keysHit(keyshit);
	oldkeys = ks;

	// game update
	g_script.gameUpdate();

	// update present objects
	if(!pause())
	{
		for(int idx: m_obj)
		{
			tBrush & obj = g_map.objects.get(idx);
			if(obj.disable) continue; // only enabled objects
			if( obj.anim )
				if(IsUpdate(obj.delay))
					++obj.frame;
		}
	}

	// player update
	if(!pause()) g_player.Update();

	// map update
	if( g_map.size() > 0) // if map size is valid
	{
		// room bound check
		if(!Room::Rect(roomPos()).IsInside(g_player.pos()))	g_script.roomOut(); // users may change player's pos on this handler

		// world bound check
		iV2 r = Room::Pos2Room( g_player.pos() );
		if( r.x<0 )				{ r.x=0; g_player.x(0); }
		iV2 sz = g_map.size();
		if( r.x > sz.x - 1 )	{ r.x = sz.x - 1; g_player.x(sz.x * Room::Size.x - 4); }
		if( r.y<0 )				{ r.y = 0; g_player.y(0); }
		if( r.y > sz.y - 1 )	{ r.y = sz.y - 1; g_player.y(sz.y * Room::Size.y - 1); }

		// room tranzit
		if( r != roomPos())
		{
			g_script.roomClose();
			SetRoom( r.x, r.y );
			g_script.roomOpen();
		}
	}

	// menu
	if( KeyHit(KEY_MENU) && !pause() )
		g_script.menu();

	// action
	if( KeyHit(KEY_ACTION) && !pause() && g_player.life() > 0 && !g_player.disable() )
		g_script.action();

	// fffx rumble
	bool update = false;
	if(m_fffx_period!=FFperiod())
	{
		m_fffx_period = FFperiod();
		update = true;
	}
	if(m_fffx_magnitude!=FFmagnitude())
	{
		m_fffx_magnitude = FFmagnitude();
		update = true;
	}
	if(update) FFFXUpdate();

	// game after update
	g_script.gameAfterUpdate();

	return true;
}


//////////////////////////////////////////////////////////////////////////////////////////////////
//DRAW
//////////////////////////////////////////////////////////////////////////////////////////////////
void cDizGame::Draw()
{
	// visible room area
	iV2 view = viewPos();
	fRect rect( g_paint.scrPos(view), g_paint.scrPos(view + Room::Size));

	// view ofset with shake option and optional viewport for scrolling
	viewShift = view + shake();
	if( viewportMode() )
		viewShift += viewportPos();
	
	// viewport flipping
	dword flip = 0;
	iV2 vv;

	iV2 v = g_paint.scrPos(view);
	if( viewportFlipX())
	{
		flip |= static_cast<dword>(Flip::X);
		vv.x = R9_GetWidth() - v.x - Room::Size.x * g_paint.scale() - v.x + 1; // magic +1
	}
	if( viewportFlipY())
	{
		flip |= static_cast<dword>(Flip::Y);
		vv.y = R9_GetHeight() - v.y - Room::Size.y * g_paint.scale() - v.y + 1; // magic +1
	}

	if( flip )
	{
		R9_SetView( vv, flip );
	}

	m_visible_brushes = 0;



	// for each layer
	for(int layer=0;layer<GAME_LAYERS;layer++)
	{
		// room
		if( viewportMode() )
		{
			// full matmap 3x3 rooms
			for( iV2 r; r.y<3; r.y++ )
			{
				for(r.x=0; r.x<3; r.x++ )
				{
					// clip here to avoid duplicate draw (brushes shared in neighbour rooms)
					// Note: brushes order must also be perserved (so the drawframe trick didn't work)
					R9_SetClipping( rect );
					iV2 p1 = g_paint.scrPos(viewShift + (r - 1) * Room::Size);
					R9_AddClipping( fRect(p1, p1 + Room::Size * g_paint.scale()));
					iV2 rr = r - 1;
					g_map.DrawRoom( roomPos() + rr, layer, m_drawmode, viewShift + rr * Room::Size);
				}
			}
		}
		else
		{
			// classic style
			R9_SetClipping( rect );
			g_map.DrawRoom( roomPos(), layer, m_drawmode, viewShift);
		}

		// objects present
		R9_SetClipping( rect );
		for(int idx: m_obj)
		{
			tBrush & obj = g_map.objects.get(idx);
			if( obj.layer != layer ) continue;
			if(obj.disable) continue;
			if((obj.draw & 1)==0 ) continue;
			ObjDraw(obj);
		}

		// dizzy
		if(g_player.layer()==layer) g_player.Draw();
	}

	if( flip )
		R9_SetView( iV2(), 0 );

	// borders
	R9_DrawBar( fRect( 0.0f, 0.0f, (float)R9_GetWidth(), rect.p1.y ),					borderColor()|0xff000000);
	R9_DrawBar( fRect( 0.0f, rect.p2.y, (float)R9_GetWidth(), (float)R9_GetHeight() ),borderColor()|0xff000000);
	R9_DrawBar( fRect( 0.0f, rect.p1.y, rect.p1.x, rect.p2.y ),							borderColor()|0xff000000);
	R9_DrawBar( fRect( rect.p2.x, rect.p1.y, (float)R9_GetWidth(),rect.p2.y ),			borderColor()|0xff000000);

/*	// clip
	rect.x1 = g_paint.m_scrx;
	rect.x2 = g_paint.m_scrx+g_game.m_screen_w*g_paint.m_scale;
	rect.y1 = g_paint.m_scry;
	rect.y2 = g_paint.m_scry+g_game.m_screen_h*g_paint.m_scale;
	R9_SetClipping(rect);
*/
	// HUD (cover, dialogs, menus, etc)
	R9_ResetClipping();
	g_paint.hud.draw(true);
	g_script.drawHud();
	g_paint.hud.draw(false);
	R9_ResetClipping();

}

void cDizGame::Resize() 
{
	matMap.Resize(Room::Size);
	SetRoom(g_game.roomX(), g_game.roomY()); // updates materialmap and re-gather objects
}


//////////////////////////////////////////////////////////////////////////////////////////////////
// ROOM
//////////////////////////////////////////////////////////////////////////////////////////////////
void cDizGame::SetRoom( int x, int y )
{
	roomX(x);
	roomY(y);
	matMap.Update(roomPos(), fullMaterialMap());
	ObjGather();
}

void MatMap::SetSize(const iV2 & size)
{
	Size = size;
	Rect = iRect(-Size, Size * 2); 
	Size3 = Rect.Size();
	Cap = Size3.x * Size3.y;

}
void MatMap::Resize(const iV2 & size)
{
	SetSize(size);
	delete [] map;
	map = new byte[Cap];
}

void MatMap::Update(const iV2 & room, bool full)
{
	// clear
	memset(map, 0, Cap);
	
	// prepare for software rendering
	g_paint.BeginSoftwareRendering(Size3, Cap, map);

	// draw room
	for(int layer=0; layer<GAME_LAYERS; layer++)
	{
		// room
		if( full )
		{
			// full matmap 3x3 rooms
			for( iV2 r; r.y<3; r.y++ )
			{
				int ryH = r.y * Size.y;
				for(r.x=0; r.x<3; r.x++ )
				{
					int rxW = r.x * Size.x;
					// clip here to avoid duplicate draw (brushes shared in neighbour rooms)
					R9_SetClipping(fRect(rxW, ryH, rxW + Size.x, ryH + Size.y));
					iV2 rr = r - 1;
					g_map.DrawRoom(room + rr, layer, DRAWMODE_MATERIAL, iV2(rxW, ryH));
				}
			}
		}
		else
		{
			// classic style
			R9_SetClipping( fRect(Size - Room::Border, Rect.p2 + Room::Border) );
			g_map.DrawRoom( room, layer, DRAWMODE_MATERIAL, Size);
		}
	}
	g_paint.EndSoftwareRendering();

	// @DBG material (dark colors)
	//R9_ImgSaveFile("matdump.tga",&g_paint.m_imgtarget);

}

int MatMap::Get(int x1, int x2, int y) const
{
	int mat = 0;
	if(y < Rect.p1.y || y >= Rect.p2.y)
		return mat;
	x1 = std::max(x1, Rect.p1.x);
	x2 = std::min(x2, Rect.p2.x);
	if(x1 >= x2)
		return mat;
	for(int v = idx(iV2(x1, y)), ve = v + (x2 - x1); v < ve; ++v)
		mat |= (1<<map[v]);
	return mat;
}

//////////////////////////////////////////////////////////////////////////////////////////////////
// OBJECTS
//////////////////////////////////////////////////////////////////////////////////////////////////
void cDizGame::ObjPresent( int idx )
{
	if(std::find(m_obj.begin(), m_obj.end(), idx) == m_obj.end())
		ObjAdd(idx);
}

void cDizGame::ObjGather()
{
	m_obj.clear();
	m_collider.clear();
	iRect roombb = viewportMode() ? 
		RoomBorderRect(Room::Size) :  // extended room bound to 3x3 rooms
		RoomBorderRect(Room::Border); // room bound with small border
	for(int i=0; i<g_map.objects.size(); i++ )
		if(g_map.objects.get(i).rect().Intersects(roombb)) 
			ObjAdd(i); // object is present in current bordered room
}

void cDizGame::ObjDraw( const tBrush & brush )
{
	// draw current tile frame
	int idx = g_paint.tiles.Find(brush.tile);
	cTile* tile = g_paint.tiles.Get(idx); if(!tile) return;
	int frame = tile->ComputeFrame(brush.frame,brush.anim);
	g_paint.DrawBrush( brush, viewShift + brush.pos - roomPos() * Room::Size, frame );
}

//////////////////////////////////////////////////////////////////////////////////////////////////
// FFFX Rumble
//////////////////////////////////////////////////////////////////////////////////////////////////
void cDizGame::FFFXUpdate()
{
	if(!I9_IsReady()) return;
	if(!I9_DeviceIsPresent(I9_DEVICE_JOYSTICK1)) return; // no joystick
	BOOL isrunning = I9_DeviceFFIsPlaying(I9_DEVICE_JOYSTICK1);
	if(g_cfg.m_rumble)
	{
		I9_DeviceFFSet(I9_DEVICE_JOYSTICK1, m_fffx_magnitude*100, m_fffx_period);
		if(m_fffx_magnitude==0 && isrunning) I9_DeviceFFStop(I9_DEVICE_JOYSTICK1);
		else
		if(m_fffx_magnitude!=0 && !isrunning) I9_DeviceFFPlay(I9_DEVICE_JOYSTICK1);
	}
	else
	{
		I9_DeviceFFSet(I9_DEVICE_JOYSTICK1, 0, 0);
		if(isrunning) I9_DeviceFFStop(I9_DEVICE_JOYSTICK1);
	}
}

//////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////
