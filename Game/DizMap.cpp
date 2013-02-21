//////////////////////////////////////////////////////////////////////////////////////////////////
// DizMap.cpp
//////////////////////////////////////////////////////////////////////////////////////////////////

#include "stdafx.h"


#include "DizMap.h"
#include "DizGame.h"
#include "DizApp.h"

#include <sstream>

cDizMap	g_map;

int Room::Width = GAME_ROOMW;
int Room::Height = GAME_ROOMH;

PREDICATE_M(map, brushCount, 1)
{
	return A1 = g_map.BrushCount();
}

PREDICATE_M(map, brushFind, 2)
{
	int idx = g_map.BrushFind(PlAtom(A1).handle);
	if(idx == -1)
		return false;
	return A2 = idx;
}

PREDICATE_M(map, brushVar, 3)
{
	int idx = A1;
	int var = A2;
	if(g_map.InvalidBrushIndex(idx))
		throw PlException("invalid brush index");
	if(tBrush::InvalidProp(var)) 
		throw PlException("invalid brush variable");
	tBrush & brush = g_map.BrushGet(idx);
	PlTerm val = A3;
	if(val.type() == PL_VARIABLE)
		return A3 = brush.Get(var);
	brush.Set(var, val);
	return true;
}

PREDICATE_M(map, brushSet , 3) 
{
	int idx = A1;
	if(g_map.InvalidBrushIndex(idx))
		throw PlException("invalid brush index");
	int var = A2;
	if(tBrush::InvalidProp(var)) 
		throw PlException("invalid brush variable");
	tBrush & brush = g_map.BrushGet(idx);

	if(var == BRUSH_COLOR)
	{
		int64 color = A3;
		brush.Set(var, color);
	} 
	else 
	{
		brush.Set(var, A3);
	}
	return true;
}

PREDICATE_M(map, objSet , 3) 
{
	int idx = A1;
	if(g_map.InvalidObjIndex(idx)) 
		throw PlException("invalid object index");
	int var = A2;
	if(Object::InvalidProp(var)) 
		throw PlException("invalid object variable"); 

	tBrush & obj = g_map.ObjGet(idx);
	if(var == BRUSH_COLOR)
	{
		int64 color = A3;
		obj.Set(var, color);
	} 
	else 
	{
		obj.Set(var, A3);
	}
	return true;
}

PREDICATE_M(map, objCount, 1)
{
	return A1 = g_map.ObjCount(); 
}

PREDICATE_M(map, objFind, 2)
{
	return A2 = g_map.ObjFind(PlAtom(A1).handle);
}

PREDICATE_M(map, objVar, 3)
{
	int idx = A1;
	if(g_map.InvalidObjIndex(idx)) 
		throw PlException("invalid object index");
	int var = A2;
	if(Object::InvalidProp(var)) 
		throw PlException("invalid object variable"); 

	tBrush & obj = g_map.ObjGet(idx);
	PlTerm val = A3;
	if(val.type() == PL_VARIABLE)
		return A3 = obj.Get(var);
	int64 v;
	if(!PL_get_int64(val, &v))
		return false;
	obj.Set(var, v); 
	return true;
}

PREDICATE_M(map, objName, 2)
{
	int idx = A1;
	if(g_map.InvalidObjIndex(idx)) 
		throw PlException("invalid object index");
	PlTerm val = A2;
	if(val.type() == PL_VARIABLE) {
		return val = g_map.ObjGet(idx).Name().c_str();
	}
	g_map.ObjGet(idx).Name(std::string(val)); 
	return true;
}

PREDICATE_M(map, brushNew, 1)
{
	int idx = g_map.BrushNew();
	return A1 = idx;
}

PREDICATE_M(map, objNew, 1)
{
	int idx = g_map.ObjNew();
	return A1 = idx;
}

PREDICATE_M(map, resize, 2)
{
	g_map.Resize(A1, A2); 
	return true; 
}

PREDICATE_M(map, setCamX, 1)
{
	return true;
}

PREDICATE_M(map, setCamY, 1)
{
	return true;
}

PREDICATE_M(map, setRoomW, 1)
{
	Room::Width = A1;
	return true;
}

PREDICATE_M(map, setRoomH, 1)
{
	Room::Height = A1;
	return true;
}

PREDICATE_M(map, reset, 0)
{
	g_map.Reset();
	return true;
}


//////////////////////////////////////////////////////////////////////////////////////////////////
// INIT
//////////////////////////////////////////////////////////////////////////////////////////////////
cDizMap::cDizMap() : m_mapw(), m_maph()
{
	guard(cDizMap::cDizMap)
	unguard()
}


void cDizMap::Reset()
{
	guard(cDizMap::Reset)
	Rooms.clear();
	m_mapw = 0;
	m_maph = 0;
	Brushes.clear();
	BrushIndex.clear();
	Objects.clear();
	ObjIndex.clear();
	unguard()
}


int	cDizMap::BrushNew()
{
	Brushes.push_back(tBrush());
	return Brushes.size() - 1;
}

int	cDizMap::ObjNew()
{
	Objects.push_back(Object());
	return Objects.size() - 1;
}

bool cDizMap::Reload()
{
	guard(cDizMap::Reload)
	Reset();
	g_script.reloadMap();
	return true;
	unguard()
}

//////////////////////////////////////////////////////////////////////////////////////////////////
// DRAW ROOM
//////////////////////////////////////////////////////////////////////////////////////////////////
void cDizMap::DrawRoom( int rx, int ry, int layer, int mode, int ofsx, int ofsy )
{
	guard(cDizMap::DrawRoom)
	int color, shader;
	if(InvalidRoomCoord(rx, ry)) return;

	// viewport clipping test
	if( !g_paint.m_drawtilesoft )
	{
		iRect viewport;
		viewport.x1 = g_game.roomX() * Room::Width - g_game.viewportX();
		viewport.y1 = g_game.roomY() * Room::Height - g_game.viewportY();
		viewport.x2 = viewport.x1 + Room::Width;
		viewport.y2 = viewport.y1 + Room::Height;
		if( rx*Room::Width>=viewport.x2 || ry*Room::Height>=viewport.y2 || (rx+1)*Room::Width<=viewport.x1 || (ry+1)*Room::Height<=viewport.y1 )
			return;
	}
	const std::vector<int> & part = GetRoom(rx, ry).Brushes();
	for(size_t i=0;i<part.size();i++)
	{
		int brushidx = part[i];

		tBrush & brush = BrushGet(brushidx);

		if( mode==DRAWMODE_NORMAL	&& (brush.Get(BRUSH_DRAW)&1)==0 ) continue; // don't draw
		if( mode==DRAWMODE_MATERIAL && (brush.Get(BRUSH_DRAW)&2)==0 ) continue; // don't write material
		if( mode==DRAWMODE_DENSITY  && (brush.Get(BRUSH_DRAW)&2)==0 ) continue; // don't write material
		
		if( brush.Get(BRUSH_LAYER) != layer ) continue; // filter layer

		int x = brush.Get(BRUSH_X) - rx * Room::Width + ofsx;
		int y = brush.Get(BRUSH_Y) - ry * Room::Height + ofsy;
		int frame = brush.Get(BRUSH_FRAME);

		if(mode==DRAWMODE_MATERIAL)
		{
			// use special color and shader
			color	= brush.Get(BRUSH_COLOR);
			shader	= brush.Get(BRUSH_SHADER);
			brush.Set(BRUSH_COLOR, g_game.materials[brush.Get(BRUSH_MATERIAL)].color | 0xff000000);
			brush.Set(BRUSH_SHADER, SHADER_ALPHAREP);
			g_paint.m_drawtilemat = brush.Get(BRUSH_MATERIAL); // software use this
			g_paint.DrawBrush( brush, x, y, frame );
			brush.Set(BRUSH_COLOR, color);
			brush.Set(BRUSH_SHADER, shader);
		}
		else
		if(mode==DRAWMODE_DENSITY)
		{
			// use special color and shader
//			dword matd_color[MATD_MAX] = {0xff000000,0xff606060,0xffa0a0a0,0xffffffff};
			color	= brush.Get(BRUSH_COLOR);
			shader	= brush.Get(BRUSH_SHADER);
			PlAtom dens = g_game.materials[brush.Get(BRUSH_MATERIAL)].density;
			dword matd_color = dens == g_game._void ? 0xff000000 : 
					dens == g_game.soft ? 0xff606060 :
					dens == g_game.hard ? 0xffa0a0a0 :
					0xffffffff;
			brush.Set(BRUSH_COLOR,matd_color);
			brush.Set(BRUSH_SHADER, SHADER_ALPHAREP);
			g_paint.DrawBrush( brush, x, y, frame );
			brush.Set(BRUSH_COLOR, color);
			brush.Set(BRUSH_SHADER, shader);
		}
		else
		{
			if(brush.Get(BRUSH_ANIM)==2) // only if looping
			{
				int gameframe = g_game.m_gameframe;
				if(brush.Get(BRUSH_DELAY)>0) gameframe /= brush.Get(BRUSH_DELAY);
				frame += gameframe;
			}
			g_paint.DrawBrush( brush, x, y, frame );
		}
	}

	R9_Flush(); // be sure!

	unguard()
}

//////////////////////////////////////////////////////////////////////////////////////////////////
// PARTITIONS
//////////////////////////////////////////////////////////////////////////////////////////////////

void cDizMap::PartitionAdd( int brushidx )
{
	guard(cDizMap::PartitionAdd)
	tBrush & brush = BrushGet(brushidx);
	iRect rbrush;
	brush.MakeBBW(rbrush.x1,rbrush.y1,rbrush.x2,rbrush.y2);
	/* @TODO find a way to optimize and get the partitions
	int brx = rbrush.x1 % m_mapw; // roomx for top-left brush corner
	int bry = rbrush.y1 / m_maph; // roomy for top-left brush corner
	int rooms[4][2] = { {brx,bry}, {brx+1,bry}, {brx,bry+1}, br
	*/
	std::vector<Room>::iterator room = Rooms.begin();
	for(int ry = 0; ry < Height(); ++ry)
		for(int rx = 0; rx < Width(); ++rx, ++room)
		{
			iRect rpartition;
			MakeRoomBBW( rx, ry, rpartition.x1, rpartition.y1, rpartition.x2, rpartition.y2, Room::Border );
			if( RECT2RECT(rbrush,rpartition) )
				room->AddBrush(brushidx);
		}
	unguard()
}

void cDizMap::PartitionMake()
{
	guard(cDizMap::PartitionMake)
	for(int i=0, e=BrushCount(); i < e; ++i)
		PartitionAdd(i);
	unguard()
}

void cDizMap::Resize( int width, int height )
{
	if(width<MAP_SIZEMIN)	width = MAP_SIZEMIN;	// too small
	if(height<MAP_SIZEMIN)	height = MAP_SIZEMIN;	// too small
	if(width>MAP_SIZEMAX)	width = MAP_SIZEMAX;	// too big
	if(height>MAP_SIZEMAX)	height = MAP_SIZEMAX;	// too big
	m_mapw = width / Room::Width;
	m_maph = height / Room::Height;

	int count = Width() * Height();
	Rooms.clear();
	Rooms.resize(count);
	PartitionMake();

	ObjIndex.clear();
	BrushIndex.clear();
	for(size_t i = 0, e = Objects.size(); i != e; ++i) {
		Object & o = ObjGet(i);
		if(int id = o.Get(BRUSH_ID))
		{
			std::stringstream sid;
			sid << "id" << id;
			PlAtom aid(sid.str().c_str());
			o.id(aid);
			ObjIndex.insert(IntIndex::value_type(aid, i));
		}
	}
	for(size_t i = 0, e = Brushes.size(); i != e; ++i) {
		tBrush & b = BrushGet(i);
		if(int id = b.Get(BRUSH_ID))
		{
			std::stringstream sid;
			sid << "id" << id;
			PlAtom aid(sid.str().c_str());
			b.id(aid);
			BrushIndex.insert(IntIndex::value_type(aid, i));
		}
	}
	g_game.MatMapAlloc();
	g_game.mapW(Width());
	g_game.mapH(Height());
	g_game.roomW(Room::Width);
	g_game.roomH(Room::Height);
	g_game.SetRoom(g_game.roomX(),g_game.roomY()); // updates materialmap and re-gather objects
}



//////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////
