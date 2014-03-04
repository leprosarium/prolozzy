//////////////////////////////////////////////////////////////////////////////////////////////////
// DizGame.cpp
//////////////////////////////////////////////////////////////////////////////////////////////////
#include "stdafx.h"
#include "DizGame.h"
#include "DizApp.h"
#include "eInput.h"


#define GAME_PROP(Prop, Get, Set) \
PREDICATE_M(game, Get, 1) { return A1 = g_game.Prop; } \
PREDICATE_M(game, Set, 1) { g_game.Prop = A1; return true; }

#define GAME_BOOL_PROP(Prop, Check, Set) \
PREDICATE_M(game, Check, 0) { return g_game.Prop; } \
PREDICATE_M(game, Set, 0) {	g_game.Prop = true; return true; } \
PREDICATE_M(game, un##Set, 0) { g_game.Prop = false; return true; }

#define GAME_COLOR_PROP(Prop, Get, Set) \
PREDICATE_M(game, Get, 1) { return A1 = static_cast<int64>(g_game.Prop); } \
PREDICATE_M(game, Set, 1) { g_game.Prop = static_cast<dword>(static_cast<int64>(A1)); return true; }

PREDICATE_M(game, frame, 1)
{
	return A1 = g_game.frame;
}

PREDICATE_M(game, command, 1)
{
	g_game.command = PlAtom(A1);
	return true;
}

PREDICATE_M(game, setName, 1)
{
	DizApp::app->Name(A1);
	return true;
}

GAME_PROP(fps, fps, setFps)
GAME_PROP(keys, keys, setKeys)
GAME_PROP(keysHit, keysHit, setKeysHit)
GAME_PROP(roomPos.x, roomX, setRoomX)
GAME_PROP(roomPos.y, roomY, setRoomY)
GAME_PROP(viewPos.x, viewX, setViewX)
GAME_PROP(viewPos.y, viewY, setViewY)
GAME_PROP(shake.x, shakeX, setShakeX)
GAME_PROP(shake.y, shakeY, setShakeY)
GAME_BOOL_PROP(pause, paused, pause)
GAME_COLOR_PROP(mapColor, mapColor, setMapColor)
GAME_COLOR_PROP(borderColor, borderColor, setBorderColor)
GAME_BOOL_PROP(viewportMode, viewportMode, setViewportMode)
GAME_PROP(viewport.x, viewportX, setViewportX)
GAME_PROP(viewport.y, viewportY, setViewportY)
GAME_BOOL_PROP(viewportFlipX, viewportFlipX, setViewportFlipX)
GAME_BOOL_PROP(viewportFlipY, viewportFlipY, setViewportFlipY)
GAME_BOOL_PROP(fullMaterialMap, fullMaterialMap, setFullMaterialMap)


PREDICATE_M(game, clearVibrate, 0)
{
	einput->vibra.Clear();
	return true;
}
PREDICATE_M(game, vibrate, 3)
{
	einput->vibra.Vibrate(static_cast<int>(A1) * 655, static_cast<int>(A2) * 655, A3);
	return true;
}

cDizGame g_game;


cDizGame::cDizGame() : 	drawmode(DrawMode::Normal),
						_void("void"),	soft("soft"), hard("hard"), jump("jump"), none("none"), start("start"), exit("exit"), refresh("refresh"),
						screenSize(GAME_SCRW, GAME_SCRH),
						screenSizeBorder(GAME_SCRWB, GAME_SCRH),
						command(none), 
						pause(),
						fps(GAME_FPS),
						keys(),
						keysHit(),
						viewPos(GAME_VIEWX, GAME_VIEWY),
						mapColor(),
						borderColor(),
						viewportMode(),
						viewportFlipX(), viewportFlipY(),
						fullMaterialMap(),
						frame(),
						visible_brushes(0)
{
}


bool cDizGame::Init()
{
	fps = GAME_FPS;

	// load game resolution settings from inf
	std::istringstream(g_cfg.Info("game_screen_bw")) >> screenSizeBorder.x;
	std::istringstream(g_cfg.Info("game_screen_bh")) >> screenSizeBorder.y;
	std::istringstream(g_cfg.Info("game_screen_w")) >> screenSize.x;
	std::istringstream(g_cfg.Info("game_screen_h")) >> screenSize.y;
	g_paint.Layout(); // refresh layout

	return CheckVersion();
}

bool cDizGame::Start()
{
	g_map.Reset();

	frame = 0;
	m_collider.clear();

	g_player.Reset();
	g_player.pos = 0;
	g_player.disable = true;

	einput->vibra.Stop();
	SetRoom(0);

	g_script.Start();

	return true;
}

bool cDizGame::CheckVersion()
{
	// requested version must be in the folowing formats: 2.0, 2.01, 2.01b, etc
	// the match is done on first 2 digits (2.0)
	std::string reqv = g_cfg.Info("dizzyage_version");
	std::string engv = GAME_VERSION;
	std::stringstream msg;
	if(reqv.size() >= 3) // enough digits
	{
		if(reqv[0]==engv[0] && reqv[1]==engv[1] && reqv[2]==engv[2]) return true; // match
		msg << "This game was made with DizzyAGE v" << reqv; 
	}
	else
		msg << "This game doesn't specify the version of DizzyAGE it was made for.";
	msg  << "\nYou are running it with DizzyAGE v" << engv;
	App::ErrorMessage(msg.str().c_str());
	return false;
}

//////////////////////////////////////////////////////////////////////////////////////////////////
// UPDATE
//////////////////////////////////////////////////////////////////////////////////////////////////
bool cDizGame::Update()
{
	// command process
	if(command == start) // start game
	{
		if(!Start()) return false;
	}
	else if(command == exit) // exit game
	{
		return false;
	}
	else if(command == refresh) // refresh room
	{
		matMap.Update(roomPos, fullMaterialMap);
	}
	command = none;

	// game frame
	frame++;

	// key input
	int ks = 0;
	static int oldkeys = 0;

	for(int i=0; i < cDizCfg::key::max; i++)
		if( einput->keyValue(g_cfg.m_key[i][0]) ||
			einput->keyValue(g_cfg.m_key[i][1]) )	
			ks |= (1<<i);
		
	int dzx = g_cfg.m_deadzone[0];
	int dzy = g_cfg.m_deadzone[1];
	int jax = g_cfg.m_joy[4];
	int jay = g_cfg.m_joy[5];
	int	jb0 = g_cfg.m_joy[0];
	int	jb1 = g_cfg.m_joy[1];
	int	jb2 = +g_cfg.m_joy[2];

	if(einput->joystickAxeValue(jax) < -dzx || einput->joystick.left.value) ks |= (1<<cDizCfg::key::left);
	if(einput->joystickAxeValue(jax) >  dzx || einput->joystick.right.value) ks |= (1<<cDizCfg::key::right);
	if(einput->joystickAxeValue(jay) < -dzy || einput->joystick.up.value) ks |= (1<<cDizCfg::key::up);
	if(einput->joystickAxeValue(jay) >  dzy || einput->joystick.down.value) ks |= (1<<cDizCfg::key::down);
	if(einput->joystickButtonValue(jb1)) ks |= (1<<cDizCfg::key::jump);		// xbox360 B
	if(einput->joystickButtonValue(jb0)) ks |= (1<<cDizCfg::key::action);	// xbox360 A
	if(einput->joystickButtonValue(jb2)) ks |= (1<<cDizCfg::key::menu);		// xbox360 X


	// avoid keys during debug
	if(g_player.m_debug) ks=0;

	keys = ks;
	keysHit = ks & ~oldkeys;
	oldkeys = ks;

	einput->vibra.Update();

	// game update
	g_script.gameUpdate();

	// update present objects
	if(!pause)
		for(auto obj: m_obj)
			if(!obj->disable && obj->anim && IsUpdate(obj->delay))
				++obj->frame;

	// player update
	if(!pause) g_player.Update();

	// map update
	if( g_map.size() > 0) // if map size is valid
	{
		// room bound check
		if(!Room::Rect(roomPos).IsInside(g_player.pos))	g_script.roomOut(); // users may change player's pos on this handler

		// world bound check
		iV2 r = Room::Pos2Room(g_player.pos);
		if(r.x < 0) r.x = g_player.pos.x = 0;
		if(r.y < 0)	r.y = g_player.pos.y = 0;
		iV2 sz = g_map.size();
		if( r.x > sz.x - 1 )	{ r.x = sz.x - 1; g_player.pos.x = sz.x * Room::Size.x - 4; }
		if( r.y > sz.y - 1 )	{ r.y = sz.y - 1; g_player.pos.y = sz.y * Room::Size.y - 1; }

		// room tranzit
		if(r != roomPos)
		{
			g_script.roomClose();
			SetRoom(r);
			g_script.roomOpen();
		}
	}

	if(KeyHit(cDizCfg::key::menu) && !pause)
		g_script.menu();
	if(KeyHit(cDizCfg::key::action) && !pause && g_player.life > 0 && !g_player.disable)
		g_script.action();

	g_script.gameAfterUpdate();

	return true;
}


//////////////////////////////////////////////////////////////////////////////////////////////////
//DRAW
//////////////////////////////////////////////////////////////////////////////////////////////////
void cDizGame::Draw()
{
	if(drawmode == DrawMode::None) return;
	// visible room area
	fRect rect( g_paint.scrPos(viewPos), g_paint.scrPos(viewPos + Room::Size));

	// view ofset with shake option and optional viewport for scrolling
	viewShift = viewPos + shake;
	if(viewportMode)
		viewShift += viewport;
	
	// viewport flipping
	dword flip = 0;
	iV2 vv;

	iV2 v = g_paint.scrPos(viewPos);
	if(viewportFlipX)
	{
		flip |= static_cast<dword>(Flip::X);
		vv.x = R9_GetWidth() - v.x - Room::Size.x * g_paint.scale - v.x + 1; // magic +1
	}
	if(viewportFlipY)
	{
		flip |= static_cast<dword>(Flip::Y);
		vv.y = R9_GetHeight() - v.y - Room::Size.y * g_paint.scale - v.y + 1; // magic +1
	}

	if( flip )
		R9_SetView( vv, flip );

	visible_brushes = 0;
	
	// for each layer
	for(int layer=0;layer<GAME_LAYERS;layer++)
	{
		// room
		if(viewportMode)
			// full matmap 3x3 rooms
			for( iV2 r; r.y<3; r.y++ )
				for(r.x=0; r.x<3; r.x++ )
				{
					// clip here to avoid duplicate draw (brushes shared in neighbour rooms)
					// Note: brushes order must also be perserved (so the drawframe trick didn't work)
					R9_SetClipping(rect);
					iV2 p1 = g_paint.scrPos(viewShift + (r - 1) * Room::Size);
					R9_AddClipping(fRect(p1, p1 + Room::Size * g_paint.scale));
					iV2 rr = r - 1;
					g_map.DrawRoom(roomPos + rr, layer, drawmode, viewShift + rr * Room::Size);
				}
		else
		{
			// classic style
			R9_SetClipping(rect);
			g_map.DrawRoom(roomPos, layer, drawmode, viewShift);
		}

		// objects present
		R9_SetClipping(rect);
		for(auto obj: m_obj)
			if( !obj->disable && obj->layer == layer && obj->draw & 1) 
				ObjDraw(*obj);

		// dizzy
		if(g_player.layer == layer) g_player.Draw();
	}

	if( flip )
		R9_SetView( iV2(), 0 );

	// borders
	R9_DrawBar( fRect( 0.0f, 0.0f, (float)R9_GetWidth(), rect.p1.y ),				borderColor|0xff000000);
	R9_DrawBar( fRect( 0.0f, rect.p2.y, (float)R9_GetWidth(), (float)R9_GetHeight() ),borderColor|0xff000000);
	R9_DrawBar( fRect( 0.0f, rect.p1.y, rect.p1.x, rect.p2.y ),						borderColor|0xff000000);
	R9_DrawBar( fRect( rect.p2.x, rect.p1.y, (float)R9_GetWidth(),rect.p2.y ),		borderColor|0xff000000);

	// HUD
	R9_ResetClipping();
	g_paint.hud.visible = true;
	g_script.drawHud();
	g_paint.hud.visible = false;
	R9_ResetClipping();

}

void cDizGame::Resize() 
{
	matMap.Resize(Room::Size);
	SetRoom(g_game.roomPos); // updates materialmap and re-gather objects
}

void cDizGame::NextDrawMode()
{
	if (drawmode == DrawMode::None)
		drawmode = DrawMode::Normal;
	else if (drawmode == DrawMode::Normal)
		drawmode ==DrawMode::Material;
	else if (drawmode == DrawMode::Material)
		drawmode = DrawMode::Density;
	else
		drawmode = DrawMode::None;
	elog::app() << "draw mode " << static_cast<int>(drawmode) << std::endl;
}


//////////////////////////////////////////////////////////////////////////////////////////////////
// ROOM
//////////////////////////////////////////////////////////////////////////////////////////////////
void cDizGame::SetRoom(const iV2 & p)
{
	roomPos = p;
	matMap.Update(roomPos, fullMaterialMap);
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
	memset(map, 0, Cap);
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
					g_map.DrawRoom(room + rr, layer, DrawMode::Material, iV2(rxW, ryH));
				}
			}
		}
		else
		{
			// classic style
			R9_SetClipping( fRect(Size - Room::Border, Rect.p2 + Room::Border) );
			g_map.DrawRoom( room, layer, DrawMode::Material, Size);
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

void cDizGame::ObjPresent(tBrush * b)
{
	if(std::find(m_obj.begin(), m_obj.end(), b) == m_obj.end())
		ObjAdd(b);
}

void cDizGame::ObjGather()
{
	m_obj.clear();
	m_collider.clear();
	iRect roombb = viewportMode ? 
		RoomBorderRect(Room::Size) :  // extended room bound to 3x3 rooms
		RoomBorderRect(Room::Border); // room bound with small border
	for(auto b: g_map.objects)
		if(b->rect().Intersects(roombb)) 
			ObjAdd(b); // object is present in current bordered room
}

void cDizGame::ObjDraw(const tBrush & brush)
{
	if(cTile * tile = g_paint.tiles.Get(g_paint.tiles.Find(brush.tile)))
		g_paint.DrawBrush( brush, viewShift + brush.pos - roomPos * Room::Size, tile->ComputeFrame(brush.frame, brush.anim));
}
