//////////////////////////////////////////////////////////////////////////////////////////////////
// DizGame.h
//////////////////////////////////////////////////////////////////////////////////////////////////
#ifndef __DIZGAME_H__
#define __DIZGAME_H__

#include "E9System.h"
#include "DizCfg.h"
#include "DizPaint.h"
#include "DizMap.h"
#include "DizPlayer.h"
#include "DizScript.h"
#include <vector>

//////////////////////////////////////////////////////////////////////////////////////////////////
// GLOBALS
//////////////////////////////////////////////////////////////////////////////////////////////////

#define DRAWMODE_NORMAL					0			// normal
#define DRAWMODE_MATERIAL				1			// show materials colors
#define DRAWMODE_DENSITY				2			// show materials densities
#define DRAWMODE_NONE					3			// don't draw
#define DRAWMODE_MAX					4			// dummy

//////////////////////////////////////////////////////////////////////////////////////////////////
// MATERIALS
//////////////////////////////////////////////////////////////////////////////////////////////////
#define	MAT_MAX				32		// dummy

class Material
{
public:
	PlAtom	density;
	int		color;
	Material(PlAtom density = PlAtom("void"), int color = 0) : density(density), color(color) {}
};

class MatMap
{
	byte * map; // material map (3x3 rooms, with current room in the middle)
	iV2 Size, Size3;
	int Cap;
	iRect Rect;
	void SetSize(const iV2 & size);
	bool in(const iV2 & p) const { return Rect.IsInside(p); }
	int idx(const iV2 & p)  const { return (p.x + Size.x) + (p.y + Size.y) * Size3.x; }
public:
	MatMap() : map()  { Resize(Room::Size); }
	~MatMap() { delete [] map; }
	void Resize(const iV2 & size);
	void Update(const iV2 & room, bool full);
	byte Get(const iV2 & p) const { return in(p) ? map[idx(p)] : 0; }
	int Get(int x1, int x2, int y) const;
};

//////////////////////////////////////////////////////////////////////////////////////////////////
// cDizGame
//////////////////////////////////////////////////////////////////////////////////////////////////

class cDizGame
{
	MatMap matMap;
public:
	PlAtom _void;		// fall through
	PlAtom	soft;		// stop fall
	PlAtom	hard;		// stop fall, collide
	PlAtom	jump;		// jumper


						cDizGame			();
						~cDizGame			();

		// init
		bool			Init				();					// init
		void			Done				();					// done
		bool			Start				();					// call this to start a new game
		bool			Update				();					// update game (called periodical to match the game fps) return false to exit game
		void			Draw				();					// draw game
		bool			CheckVersion		();					// check game version (first 2 digits)
		
		// settings
		iV2				screenSize;								// game resolution (256x192 - Z80 res)
		iV2				screenSizeBorder;						// game resolution border (320x200)
		// game

		void Resize();

		int				m_gameframe;							// game frame index

inline	bool			IsUpdate			( int delay )		{ return (delay==0 || (m_gameframe%delay==0)); }
inline	bool			Key( int key )							{ return (keys() & (1<<key)) ? 1 : 0; }			// test a key
inline	bool			KeyHit( int key )						{ return (keysHit() & (1<<key)) ? 1 : 0; }			// test a key hit

		Material		materials[MAT_MAX];
		byte			MatMap ( const iV2 & p) { return matMap.Get(p); }
		int				MatMap (int x1, int x2, int y) { return matMap.Get(x1, x2, y); }
		PlAtom			DensMap	( const iV2 & p) { return materials[MatMap(p)].density; }

		// map room
		void			SetRoom				( int x, int y );	// set current room (load)
		iRect			RoomBorderRect		(const iV2 & border)	{ return g_map.RoomBorderRect(roomPos(), border); }

		iV2				viewShift;								// view position (used in draw, set from G_VIEW, G_SHAKE, and G_VIEWPORT)
		int				m_drawmode;								// 0=imgmap (normal), 1=matmap, 2=densitymap, 3=none
			
		// objects
inline	void			ObjAdd				( int idx );		// add object to present lists (objects and coliders)
		void			ObjGather			();					// gather objects present in current room
		void			ObjPresent			( int idx );		// add object to present list if not in it already
		void			ObjDraw				( const tBrush & brush	);	// draw one object
		std::vector<int> m_obj;									// objects list with objects indexes (present in current room)
		std::vector<int> m_collider;							// colliders list with objects indexes (present in current room)

		// FFFX Rumble
		int				m_fffx_magnitude;						// force magnitude [0,100] (0=stopped)
		int				m_fffx_period;							// force period in miliseconds (50=default)
		void			FFFXUpdate();							// update force feedback
inline	void			FFFXStop()								{ m_fffx_magnitude=0; FFmagnitude(0); FFFXUpdate(); }

		// stats
		int				m_visible_brushes;

		bool			pause() const { return _pause; }
		int				fps() const { return _fps; }
		int				keys() const { return _keys; }
		int				keysHit() const { return _keysHit; }
		iV2				roomPos() const { return _roomPos; }
		int				roomX() const { return _roomPos.x; }
		int				roomY() const { return _roomPos.y; }
		iV2				viewPos() const { return iV2(viewX(), viewY()); }
		int				viewX() const { return _viewX; }
		int				viewY() const { return _viewY; }
		iV2				shake() const { return iV2(shakeX(), shakeY()); }
		int				shakeX() const { return _shakeX; }
		int				shakeY() const { return _shakeY; }
		int				mapColor() const { return _mapColor; }
		int				borderColor() const { return _borderColor; }
		int				FFmagnitude() const { return  _FFmagnitude; }
		int				FFperiod() const { return _FFperiod; }
		bool			viewportMode() const { return _viewportMode; }
		iV2				viewportPos() const { return iV2(viewportX(), viewportY()); }
		int				viewportX() const { return _viewportX; }
		int				viewportY() const { return _viewportY; }
		bool			viewportFlipX() const { return _viewportFlipX; }
		bool			viewportFlipY() const { return _viewportFlipY; }
		bool			fullMaterialMap() const { return _fullMaterialMap; }
		PlAtom			command() const { return _command; }

		void			pause(bool pause) { _pause = pause; }
		void			fps(int fps) { _fps = fps; }
		void			keys(int keys) { _keys = keys; }
		void			keysHit(int keysHit) { _keysHit = keysHit; }
		void			roomPos(const iV2 &p) { _roomPos = p; }
		void			roomX(int v) { _roomPos.x = v; }
		void			roomY(int v) { _roomPos.y = v; }
		void			viewX(int viewX) { _viewX = viewX; }
		void			viewY(int viewY) { _viewY = viewY; }
		void			shakeX(int shakeX) { _shakeX = shakeX; }
		void			shakeY(int shakeY) { _shakeY = shakeY; }
		void			mapColor(int color) { _mapColor = color; }
		void			borderColor(int color) { _borderColor = color; }
		void			FFmagnitude(int magnitude) { _FFmagnitude = magnitude; }
		void			FFperiod(int period) { _FFperiod = period; }
		void			viewportMode(bool viewportMode) { _viewportMode = viewportMode; }
		void			viewportX(int x) { _viewportX = x; }
		void			viewportY(int y) { _viewportY = y; }
		void			viewportFlipX(bool flip) { _viewportFlipX = flip; }
		void			viewportFlipY(bool flip) { _viewportFlipY = flip; }
		void			fullMaterialMap(bool fullMaterialMap) { _fullMaterialMap = fullMaterialMap; }
		void			command(PlAtom cmd) { _command = cmd; }

private:
		PlAtom none;				// nothing to do
		PlAtom start;				// must start game
		PlAtom exit;				// must exit game
		PlAtom refresh;				// refresh room material map

		PlAtom			_command;								// game command request
	
		bool			_pause;									// if game is paused
		int				_fps;
		int				_keys;									// input keys: bit 0=KEY_LEFT, 1=KEY_RIGHT, 2=KEY_UP, 3=KEY_DOWN, 4=KEY_JUMP, 5=KEY_ACTION, 6=KEY_MENU
		int				_keysHit;								// input keys: bit 0=KEY_LEFT, 1=KEY_RIGHT, 2=KEY_UP, 3=KEY_DOWN, 4=KEY_JUMP, 5=KEY_ACTION, 6=KEY_MENU; tells if the key was just hitted down this frame !
		iV2				_roomPos;								// current room pos (x, y)
		int				_viewX;									// position of room's view on the hud
		int				_viewY;									// position of room's view on the hud
		int				_shakeX;								// view x offset used for shaking
		int				_shakeY;								// view y offset used for shaking
		int				_mapColor;								// map background color
		int				_borderColor;							// window border color
		int				_FFmagnitude;							// force feedback magnitude [0..100]
		int				_FFperiod;								// force feedback period (miliseconds)
		bool			_viewportMode;							// viewport mode: false=normal, true=extended to 3x3 rooms for scrolling
		int				_viewportX;								// viewport x offset
		int				_viewportY;								// viewport y offset
		bool			_viewportFlipX;							// viewport flip X options
		bool			_viewportFlipY;							// viewport flip Y options
		bool			_fullMaterialMap;						// material map size: false=normal (current room with small border), true=extended to 3x3 rooms for scrolling
};

inline void cDizGame::ObjAdd( int idx )
{
 	m_obj.push_back(idx);
	if( g_map.objects.get(idx).collider )
		m_collider.push_back(idx);

//	obj->Set(BRUSH_COLLISION,0); // reset collision
}

extern cDizGame g_game;



#endif
//////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////
