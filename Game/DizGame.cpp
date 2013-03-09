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
	E9_AppSetStr(E9_APP_NAME, A1);
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

PREDICATE_M(game, roomW, 1)
{
	return A1 = g_game.roomW();
}
PREDICATE_M(game, roomH, 1)
{
	return A1 = g_game.roomH();
}
PREDICATE_M(game, mapW, 1)
{
	return A1 = g_game.mapW();
}

PREDICATE_M(game, mapH, 1)
{
	return A1 = g_game.mapH();
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
	g_game.mapColor(v); 
	return true;
}

PREDICATE_M(game, borderColor, 1)
{
	return A1 = static_cast<int>(g_game.borderColor());
}

PREDICATE_M(game, setBorderColor, 1)
{
	int64 v = A1;
	g_game.borderColor(v); 
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




cDizGame::cDizGame() : 	_void("void"),	soft("soft"), hard("hard"), jump("jump"), none("none"), start("start"), exit("exit"), refresh("refresh"), _command(none), 
						_pause(),
						_fps(GAME_FPS),
						_keys(),
						_keysHit(),
						_roomX(), _roomY(),
						_roomW(), _roomH(),
						_mapW(), _mapH(),
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


	m_screen_bw		= GAME_SCRWB;
	m_screen_bh		= GAME_SCRHB;
	m_screen_w		= GAME_SCRW;
	m_screen_h		= GAME_SCRH;

	m_gameframe		= 0;
	m_viewx			= 0;
	m_viewy			= 0;
	m_drawmode		= DRAWMODE_NORMAL;
				
//	memset(m_matdensity,0,sizeof(m_matdensity));
//	memset(m_matcolor,0,sizeof(m_matcolor));

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
	sscanf( g_cfg.GetInfoValue( "game_screen_bw" ), "%i", &m_screen_bw );
	sscanf( g_cfg.GetInfoValue( "game_screen_bh" ), "%i", &m_screen_bh );
	sscanf( g_cfg.GetInfoValue( "game_screen_w" ), "%i", &m_screen_w );
	sscanf( g_cfg.GetInfoValue( "game_screen_h" ), "%i", &m_screen_h );
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
	int rx, ry;

	// command process
	if(command()==start) // start game
	{
		bool ok = Start();
		if(!ok) return false;
	}
	else
	if(command()==exit) // exit game
	{
		return false;
	}
	if(command()==refresh) // refresh room
	{
		matMap.Update(roomX(), roomY(), fullMaterialMap());
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

	// script update
	g_script.Update();

	// update present objects
	if(!pause())
	{
		for(std::vector<int>::const_iterator i = m_obj.begin(), e = m_obj.end(); i != e; ++i)
		{
			tBrush & obj = g_map.ObjGet(*i);
			if( obj.Get(BRUSH_DISABLE)!=0 ) continue; // only enabled objects
			if( obj.Get(BRUSH_ANIM)!=0 )
			{
				if(IsUpdate(obj.Get(BRUSH_DELAY)))
				{
					int frame=obj.Get(BRUSH_FRAME);
					obj.Set(BRUSH_FRAME,frame+1);
				}
			}
		}
	}

	// player update
	if(!pause()) g_player.Update();

	// map update
	if( g_map.Width()>0 && g_map.Height()>0 ) // if map size is valid
	{
		// room bound check
		rx = roomX();
		ry = roomY();
		bool outroom = false;
		outroom |= ( g_player.x() < rx*Room::Width );
		outroom |= ( g_player.x() >= (rx+1)*Room::Width );
		outroom |= ( g_player.y() < ry*Room::Height );
		outroom |= ( g_player.y() >= (ry+1)*Room::Height );
		if(outroom)	g_script.roomOut(); // users may change player's pos on this handler

		// world bound check
		rx = Room::PosX2Room( g_player.x() );
		ry = Room::PosY2Room( g_player.y() );
		if( rx<0 )				{ rx=0; g_player.x(0); }
		if( rx>g_map.Width()-1 )	{ rx=g_map.Width()-1; g_player.x(g_map.Width()*Room::Width-4); }
		if( ry<0 )				{ ry=0; g_player.y(0); }
		if( ry>g_map.Height()-1 )	{ ry=g_map.Height()-1; g_player.y(g_map.Height()*Room::Height-1); }

		// room tranzit
		if( rx!=roomX() ||ry!=roomY())
		{
			g_script.roomClose();
			SetRoom( rx, ry );
			g_script.roomOpen();
		}
	}

	// menu
	if( KeyHit(KEY_MENU) && !pause() )
	{
		g_script.menu();
	}

	// action
	if( KeyHit(KEY_ACTION) && !pause() )
	{
		if(	g_player.life() > 0 && 
			!g_player.disable() )
		{
			g_script.action();
		}
	}

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
	//int i;

	// clipping
	fRect rect; // visible room area
	fRect clip; // draw room area (for viewportmode=1)
	rect.x1 = (float)(g_paint.m_scrx+viewX()*g_paint.m_scale);
	rect.x2 = (float)(g_paint.m_scrx+(viewX()+Room::Width)*g_paint.m_scale);
	rect.y1 = (float)(g_paint.m_scry+viewY()*g_paint.m_scale);
	rect.y2 = (float)(g_paint.m_scry+(viewY()+Room::Height)*g_paint.m_scale);

	// view ofset with shake option and optional viewport for scrolling
	m_viewx = viewX() + shakeX();
	m_viewy = viewY() + shakeY();
	if( viewportMode() )
	{
		m_viewx += viewportX();
		m_viewy += viewportY();
	}
	
	// viewport flipping
	dword flip = 0;
	int viewx = 0;
	int viewy = 0;

	if( viewportFlipX())
	{
		flip |= R9_FLIPX;
		int vx = g_paint.m_scrx + viewX()*g_paint.m_scale;
		viewx = R9_GetWidth() - vx - roomW()*g_paint.m_scale - vx + 1; // magic +1
	}
	if( viewportFlipY())
	{
		flip |= R9_FLIPY;
		int vy = g_paint.m_scry + viewY()*g_paint.m_scale;
		viewy = R9_GetHeight() - vy - roomH()*g_paint.m_scale - vy + 1; // magic +1
	}

	if( flip )
	{
		R9_SetView( viewx, viewy, flip );
	}

	m_visible_brushes = 0;

	// for each layer
	for(int layer=0;layer<GAME_LAYERS;layer++)
	{
		// room
		if( viewportMode() )
		{
			// full matmap 3x3 rooms
			for( int ry=0; ry<3; ry++ )
			{
				for( int rx=0; rx<3; rx++ )
				{
					// clip here to avoid duplicate draw (brushes shared in neighbour rooms)
					// Note: brushes order must also be perserved (so the drawframe trick didn't work)
					R9_SetClipping( rect );
					clip.x1 = (float)g_paint.m_scrx + (m_viewx+(rx-1)*Room::Width)*g_paint.m_scale,
					clip.y1 = (float)g_paint.m_scry + (m_viewy+(ry-1)*Room::Height)*g_paint.m_scale,
					clip.x2 = clip.x1 + Room::Width*g_paint.m_scale;
					clip.y2 = clip.y1 + Room::Height*g_paint.m_scale;
					R9_AddClipping( clip );
					g_map.DrawRoom( roomX()+rx-1, roomY()+ry-1, layer, m_drawmode, m_viewx+(rx-1)*Room::Width, m_viewy+(ry-1)*Room::Height );
				}
			}
		}
		else
		{
			// classic style
			R9_SetClipping( rect );
			g_map.DrawRoom( roomX(), roomY(), layer, m_drawmode, m_viewx, m_viewy );
		}

		// objects present
		R9_SetClipping( rect );
		for(std::vector<int>::const_iterator i = m_obj.begin(), e = m_obj.end(); i != e; ++i)
		{
			tBrush & obj = g_map.ObjGet( *i );
			if( obj.Get(BRUSH_LAYER)!=layer ) continue;
			if( obj.Get(BRUSH_DISABLE)!=0 ) continue;
			if((obj.Get(BRUSH_DRAW) & 1)==0 ) continue;
			ObjDraw(obj);
		}

		// dizzy
		if(g_player.layer()==layer) g_player.Draw();
	}

	if( flip )
		R9_SetView( 0, 0, 0 );

	// borders
	R9_DrawBar( fRect( 0.0f, 0.0f, (float)R9_GetWidth(), rect.y1 ),					borderColor()|0xff000000);
	R9_DrawBar( fRect( 0.0f, rect.y2, (float)R9_GetWidth(), (float)R9_GetHeight() ),borderColor()|0xff000000);
	R9_DrawBar( fRect( 0.0f, rect.y1, rect.x1, rect.y2 ),							borderColor()|0xff000000);
	R9_DrawBar( fRect( rect.x2, rect.y1, (float)R9_GetWidth(),rect.y2 ),			borderColor()|0xff000000);

/*	// clip
	rect.x1 = g_paint.m_scrx;
	rect.x2 = g_paint.m_scrx+g_game.m_screen_w*g_paint.m_scale;
	rect.y1 = g_paint.m_scry;
	rect.y2 = g_paint.m_scry+g_game.m_screen_h*g_paint.m_scale;
	R9_SetClipping(rect);
*/
	// HUD (cover, dialogs, menus, etc)
	R9_ResetClipping();
	g_paint.m_huddraw = true;
	g_script.drawHud();
	g_paint.m_huddraw = false;
	R9_ResetClipping();

}

void cDizGame::Resize(int w, int h) 
{
	mapW(w);
	mapH(h);
	roomW(Room::Width);
	roomH(Room::Height);
	matMap.Alloc(Room::Width, Room::Height);
	SetRoom(g_game.roomX(), g_game.roomY()); // updates materialmap and re-gather objects
}


//////////////////////////////////////////////////////////////////////////////////////////////////
// ROOM
//////////////////////////////////////////////////////////////////////////////////////////////////
void cDizGame::SetRoom( int x, int y )
{
	roomX(x);
	roomY(y);
	matMap.Update(x, y, fullMaterialMap());
	ObjGather();
}

void MatMap::SetSize(int w, int h)
{
	W = w;
	H = h;
	X1 = -W;
	X2 = 2 * W;
	Y1 = -H;
	Y2 = 2 * H;
	W3 = W * 3;
	H3 = H * 3;
	Size = W3 * H3;

}
void MatMap::Alloc(int w, int h)
{
	SetSize(w, h);
	delete [] map;
	map = new byte[Size];
}

void MatMap::Update(int roomX, int roomY, bool full)
{
	// clear
	memset(map, 0, Size);
	
	// prepare coordinates
	int scrx = g_paint.m_scrx;
	int scry = g_paint.m_scry;
	int scale = g_paint.m_scale;
	g_paint.m_scrx = 0;
	g_paint.m_scry = 0;
	g_paint.m_scale = 1;

	// clipping
	fRect oldclip = R9_GetClipping();
	
	// prepare for software rendering
	g_paint.m_drawtilesoft = true;
	g_paint.m_imgtarget.m_pf = R9_PF_A;
	g_paint.m_imgtarget.m_width = W3;
	g_paint.m_imgtarget.m_height = H3;
	g_paint.m_imgtarget.m_size = Size;
	g_paint.m_imgtarget.m_data = map;

	// draw room
	for(int layer=0; layer<GAME_LAYERS; layer++)
	{
		// room
		if( full )
		{
			// full matmap 3x3 rooms
			for( int ry=0; ry<3; ry++ )
			{
				int ryH = ry * H;
				for( int rx=0; rx<3; rx++ )
				{
					int rxW = rx * W;
					// clip here to avoid duplicate draw (brushes shared in neighbour rooms)
					R9_SetClipping(fRect(rxW, ryH, rxW + W, ryH + H));
					g_map.DrawRoom(roomX + rx - 1, roomY + ry - 1, layer, DRAWMODE_MATERIAL, rxW, ryH);
				}
			}
		}
		else
		{
			// classic style
			R9_SetClipping( fRect(W - Room::Border, H - Room::Border, X2 + Room::Border, Y2 + Room::Border) );
			g_map.DrawRoom( roomX, roomY, layer, DRAWMODE_MATERIAL, W, H);
		}
	}

	// rollback
	g_paint.m_drawtilesoft = false;
	g_paint.m_scrx = scrx;
	g_paint.m_scry = scry;
	g_paint.m_scale = scale;
	R9_SetClipping(oldclip);

	// @DBG material (dark colors)
	//R9_ImgSaveFile("matdump.tga",&g_paint.m_imgtarget);

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

	int i;
	m_obj.clear();
	m_collider.clear();

	iRect objbb;
	iRect roombb;
	if( viewportMode() )
	{
		// extended room bound to 3x3 rooms
		MakeRoomBBW(roombb.x1,roombb.y1,roombb.x2,roombb.y2,0);
		roombb.x1 -= Room::Width;
		roombb.x2 += Room::Width;
		roombb.y1 -= Room::Height;
		roombb.y2 += Room::Height;
	}
	else
	{
		// room bound with small border
		MakeRoomBBW(roombb.x1,roombb.y1,roombb.x2,roombb.y2,Room::Border);
	}

	for( i=0; i<g_map.ObjCount(); i++ )
	{
		tBrush & obj = g_map.ObjGet(i);
		obj.MakeBBW(objbb.x1,objbb.y1,objbb.x2,objbb.y2);
		if(RECT2RECT(objbb,roombb)) ObjAdd(i); // object is present in current bordered room
	}

}

void cDizGame::ObjDraw( const tBrush & brush )
{
	
	// MAKEBBR
	int rx = roomX();
	int ry = roomY();
	int x1,y1,x2,y2;
	brush.MakeBBW(x1,y1,x2,y2);
	x1 = x1 - rx*Room::Width;
	x2 = x2 - rx*Room::Width;
	y1 = y1 - ry*Room::Height;
	y2 = y2 - ry*Room::Height;

	// draw current tile frame
	int idx = g_paint.tiles.Find(brush.Get(BRUSH_TILE));
	cTile* tile = g_paint.tiles.Get(idx); if(!tile) return;
	int frame = ComputeFrame(brush.Get(BRUSH_FRAME),tile->m_frames,brush.Get(BRUSH_ANIM));
	g_paint.DrawBrush( brush, m_viewx+x1, m_viewy+y1, frame );

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
