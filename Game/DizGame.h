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
	int	_period;						// force period in miliseconds (50=default)
public:
	int magnitude;
	int period;

	FFFX() : _magnitude(), _period(50), magnitude(_magnitude), period(_period) {}
	void Update();
};


class cDizGame
{
	PlAtom none;				// nothing to do
	PlAtom start;				// must start game
	PlAtom exit;				// must exit game
	PlAtom refresh;				// refresh room material map
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
	int keys;									// input keys: bit 0=key::left, 1=key::right, 2=key::up, 3=key::down, 4=key::jump, 5=key::action, 6=key::menu
	int keysHit;								// input keys: bit 0=key::left, 1=key::right, 2=key::up, 3=key::down, 4=key::jump, 5=key::action, 6=key::menu; tells if the key was just hitted down this frame !
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
