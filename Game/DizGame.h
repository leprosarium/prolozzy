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

// FFFX Rumble
class FFFX
{
	int _magnitude;						// force magnitude [0,100] (0=stopped)
	int	_period;							// force period in miliseconds (50=default)
	bool updated;
public:
	void magnitude(int m) { _magnitude = m; updated = false; }
	void period(int p) { _period = p; updated = false;}

	int magnitude() const { return _magnitude; }
	int period() const { return _period; }
	FFFX() : _magnitude(), _period(50), updated(false) {}
	void Update();						// update force feedback

};


class cDizGame
{
	MatMap matMap;
	int	drawmode;		// 0=imgmap (normal), 1=matmap, 2=densitymap, 3=none

	bool CheckVersion();
public:
	PlAtom _void;		// fall through
	PlAtom soft;		// stop fall
	PlAtom hard;		// stop fall, collide
	PlAtom jump;		// jumper

	PlAtom command;								// game command request
	
	bool pause;									// if game is paused
	int fps;
	int keys;									// input keys: bit 0=KEY_LEFT, 1=KEY_RIGHT, 2=KEY_UP, 3=KEY_DOWN, 4=KEY_JUMP, 5=KEY_ACTION, 6=KEY_MENU
	int keysHit;								// input keys: bit 0=KEY_LEFT, 1=KEY_RIGHT, 2=KEY_UP, 3=KEY_DOWN, 4=KEY_JUMP, 5=KEY_ACTION, 6=KEY_MENU; tells if the key was just hitted down this frame !
	iV2 roomPos;								// current room pos (x, y)
	iV2 viewPos;								// position of room's view on the hud
	iV2 shake;									// view offset used for shaking
	dword mapColor;								// map background color
	dword borderColor;							// window border color
	bool viewportMode;							// viewport mode: false=normal, true=extended to 3x3 rooms for scrolling
	iV2 viewport;								// viewport offset
	bool viewportFlipX;							// viewport flip X options
	bool viewportFlipY;							// viewport flip Y options
	bool fullMaterialMap;						// material map size: false=normal (current room with small border), true=extended to 3x3 rooms for scrolling




	cDizGame();

	bool Init();
	bool Start();					// call this to start a new game
	bool Update();					// update game (called periodical to match the game fps) return false to exit game
	void Draw();

	
	void NextDrawMode();
		// settings
		iV2				screenSize;								// game resolution (256x192 - Z80 res)
		iV2				screenSizeBorder;						// game resolution border (320x200)
		// game

		void Resize();

		int				m_gameframe;							// game frame index

	bool IsUpdate(int delay) { return (delay==0 || (m_gameframe%delay==0)); }
	bool Key(int key) { return (keys & (1<<key)) ? 1 : 0; }				// test a key
	bool KeyHit(int key) { return (keysHit & (1<<key)) ? 1 : 0; }			// test a key hit

		Material		materials[MAT_MAX];
		byte			MatMap ( const iV2 & p) { return matMap.Get(p); }
		int				MatMap (int x1, int x2, int y) { return matMap.Get(x1, x2, y); }
		PlAtom			DensMap	( const iV2 & p) { return materials[MatMap(p)].density; }

		// map room
	void SetRoom(const iV2 & v);	// set current room (load)
	iRect RoomBorderRect(const iV2 & border) { return g_map.RoomBorderRect(roomPos, border); }

		iV2				viewShift;								// view position (used in draw, set from G_VIEW, G_SHAKE, and G_VIEWPORT)

			
		// objects
inline	void			ObjAdd				(tBrush *);		// add object to present lists (objects and coliders)
		void			ObjGather			();					// gather objects present in current room
		void			ObjPresent			(tBrush *);		// add object to present list if not in it already
		void			ObjDraw				( const tBrush & brush	);	// draw one object
		std::vector<tBrush *> m_obj;									// objects list with objects indexes (present in current room)
		std::vector<tBrush *> m_collider;							// colliders list with objects indexes (present in current room)


		FFFX fffx;


		// stats
		int				m_visible_brushes;

		//bool			pause() const { return _pause; }
		//int				fps() const { return _fps; }
		//int				keys() const { return _keys; }
		//int				keysHit() const { return _keysHit; }
		//iV2				roomPos() const { return _roomPos; }
		//int				roomX() const { return _roomPos.x; }
		//int				roomY() const { return _roomPos.y; }
		//iV2				viewPos() const { return _viewPos; }
		//int				viewX() const { return _viewPos.x; }
		//int				viewY() const { return _viewPos.y; }
		//iV2				shake() const { return _shake; }
		//int				shakeX() const { return _shake.x; }
		//int				shakeY() const { return _shake.y; }
		//int				mapColor() const { return _mapColor; }
		//int				borderColor() const { return _borderColor; }
		//bool			viewportMode() const { return _viewportMode; }
		//iV2				viewportPos() const { return _viewport; }
		//int				viewportX() const { return _viewport.x; }
		//int				viewportY() const { return _viewport.y; }
		//bool			viewportFlipX() const { return _viewportFlipX; }
		//bool			viewportFlipY() const { return _viewportFlipY; }
		//bool			fullMaterialMap() const { return _fullMaterialMap; }
		//PlAtom			command() const { return _command; }

		//void			pause(bool pause) { _pause = pause; }
		//void			fps(int fps) { _fps = fps; }
		//void			keys(int keys) { _keys = keys; }
		//void			keysHit(int keysHit) { _keysHit = keysHit; }
		//void			roomPos(const iV2 &p) { _roomPos = p; }
		//void			roomX(int v) { _roomPos.x = v; }
		//void			roomY(int v) { _roomPos.y = v; }
		//void			viewX(int viewX) { _viewPos.x = viewX; }
		//void			viewY(int viewY) { _viewPos.y = viewY; }
		//void			shakeX(int shakeX) { _shake.x = shakeX; }
		//void			shakeY(int shakeY) { _shake.y = shakeY; }
		//void			mapColor(int color) { _mapColor = color; }
		//void			borderColor(int color) { _borderColor = color; }
		//void			viewportMode(bool viewportMode) { _viewportMode = viewportMode; }
		//void			viewportX(int x) { _viewport.x = x; }
		//void			viewportY(int y) { _viewport.y = y; }
		//void			viewportFlipX(bool flip) { _viewportFlipX = flip; }
		//void			viewportFlipY(bool flip) { _viewportFlipY = flip; }
		//void			fullMaterialMap(bool fullMaterialMap) { _fullMaterialMap = fullMaterialMap; }
		//void			command(PlAtom cmd) { _command = cmd; }

private:
		PlAtom none;				// nothing to do
		PlAtom start;				// must start game
		PlAtom exit;				// must exit game
		PlAtom refresh;				// refresh room material map

};

inline void cDizGame::ObjAdd(tBrush *b)
{
 	m_obj.push_back(b);
	if(b->collider)
		m_collider.push_back(b);
//	b->collision = false; //reset collision
}

extern cDizGame g_game;



#endif
//////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////
