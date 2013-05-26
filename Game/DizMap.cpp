//////////////////////////////////////////////////////////////////////////////////////////////////
// DizMap.cpp
//////////////////////////////////////////////////////////////////////////////////////////////////

#include "stdafx.h"


#include "DizMap.h"
#include "DizGame.h"
#include "DizApp.h"

#include <sstream>
iV2 Room::Size(GAME_ROOMW, GAME_ROOMH);
cDizMap	g_map;


PREDICATE_M(map, brushCount, 1)
{
	return A1 = static_cast<int>(g_map.brushes.size());
}

PREDICATE_M(map, brushFind, 2)
{
	int idx = g_map.brushes.Find(PlAtom(A1).handle);
	if(idx == -1)
		return false;
	return A2 = idx;
}

PREDICATE_M(map, brushVar, 3)
{
	int idx = A1;
	int var = A2;
	if(g_map.brushes.InvalidIdx(idx))
		throw PlException("invalid brush index");
	if(tBrush::InvalidProp(var)) 
		throw PlException("invalid brush variable");
	if(tBrush * brush = g_map.brushes.Get(idx))
	{
		PlTerm val = A3;
		if(val.type() == PL_VARIABLE)
			return A3 = brush->Get(var);
		brush->Set(var, val);
		return true;
	}
	return false;
}

PREDICATE_M(map, brushSet , 3) 
{
	int idx = A1;
	if(g_map.brushes.InvalidIdx(idx))
		throw PlException("invalid brush index");
	int var = A2;
	if(tBrush::InvalidProp(var)) 
		throw PlException("invalid brush variable");
	if(tBrush * brush = g_map.brushes.Get(idx))
	{

		if(var == BRUSH_COLOR)
		{
			int64 color = A3;
			brush->Set(var, static_cast<int>(color));
		} 
		else 
		{
			brush->Set(var, A3);
		}
		return true;
	}
	return false;
}

PREDICATE_M(map, objSet , 3) 
{
	int idx = A1;
	if(g_map.objects.InvalidIdx(idx)) 
		throw PlException("invalid object index");
	int var = A2;
	if(Object::InvalidProp(var)) 
		throw PlException("invalid object variable"); 

	tBrush & obj = g_map.objects.get(idx);
	if(var == BRUSH_COLOR)
	{
		int64 color = A3;
		obj.Set(var, static_cast<int>(color));
	} 
	else 
	{
		obj.Set(var, A3);
	}
	return true;
}

PREDICATE_M(map, objCount, 1)
{
	return A1 = static_cast<int>(g_map.objects.size()); 
}

PREDICATE_M(map, objFind, 2)
{
	return A2 = g_map.objects.Find(PlAtom(A1).handle);
}

PREDICATE_M(map, objVar, 3)
{
	int idx = A1;
	if(g_map.objects.InvalidIdx(idx)) 
		throw PlException("invalid object index");
	int var = A2;
	if(Object::InvalidProp(var)) 
		throw PlException("invalid object variable"); 

	tBrush & obj = g_map.objects.get(idx);
	PlTerm val = A3;
	if(val.type() == PL_VARIABLE)
		return A3 = obj.Get(var);
	int64 v = val;
	obj.Set(var, static_cast<int>(v)); 
	return true;
}

PREDICATE_M(map, objName, 2)
{
	int idx = A1;
	if(g_map.objects.InvalidIdx(idx)) 
		throw PlException("invalid object index");
	PlTerm val = A2;
	if(val.type() == PL_VARIABLE) {
		return val = g_map.objects.get(idx).Name().c_str();
	}
	g_map.objects.get(idx).Name(val); 
	return true;
}

PREDICATE_M(map, brushNew, 1)
{
	int idx = g_map.brushes.New();
	return A1 = idx;
}

PREDICATE_M(map, objNew, 1)
{
	int idx = g_map.objects.New();
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
	Room::Size.x = A1;
	return true;
}

PREDICATE_M(map, setRoomH, 1)
{
	Room::Size.y = A1;
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
}


void cDizMap::Reset()
{
	Rooms.clear();
	m_mapw = 0;
	m_maph = 0;
	brushes.clear();
	objects.clear();
}

bool cDizMap::Reload()
{
	Reset();
	g_script.reloadMap();
	return true;
}

//////////////////////////////////////////////////////////////////////////////////////////////////
// DRAW ROOM
//////////////////////////////////////////////////////////////////////////////////////////////////
void cDizMap::DrawRoom( const iV2 & rp, int layer, int mode, const iV2 & ofs)
{
	int color, shader;
	if(InvalidRoomCoord(rp.x, rp.y)) return;

	// viewport clipping test
	if( !g_paint.drawtilesoft() )
	{
		iV2 p1 = g_game.roomPos() * Room::Size - g_game.viewportPos();
		iRect viewport(p1, p1 + Room::Size);
		if(!(rp * Room::Size < viewport.p2 && (rp + 1) * Room::Size > viewport.p1))
			return;


	}
	const std::vector<int> & part = GetRoom(rp.x, rp.y).Brushes();
	for(size_t i=0;i<part.size();i++)
	{
		int brushidx = part[i];

		tBrush & brush = * brushes.Get(brushidx);

		if( mode==DRAWMODE_NORMAL	&& (brush.Get(BRUSH_DRAW)&1)==0 ) continue; // don't draw
		if( mode==DRAWMODE_MATERIAL && (brush.Get(BRUSH_DRAW)&2)==0 ) continue; // don't write material
		if( mode==DRAWMODE_DENSITY  && (brush.Get(BRUSH_DRAW)&2)==0 ) continue; // don't write material
		
		if( brush.layer() != layer ) continue; // filter layer

		iV2 p = brush.pos() - rp * Room::Size + ofs;
		int frame = brush.Get(BRUSH_FRAME);

		if(mode==DRAWMODE_MATERIAL)
		{
			// use special color and shader
			color	= brush.Get(BRUSH_COLOR);
			shader	= brush.Get(BRUSH_SHADER);
			brush.Set(BRUSH_COLOR, g_game.materials[brush.Get(BRUSH_MATERIAL)].color | 0xff000000);
			brush.shader(Blend::AlphaRep);
			g_paint.drawtilemat(brush.Get(BRUSH_MATERIAL)); // software use this
			g_paint.DrawBrush( brush, p, frame );
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
			brush.shader(Blend::AlphaRep);
			g_paint.DrawBrush( brush, p, frame );
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
			g_paint.DrawBrush( brush, p, frame );
		}
	}

	R9_Flush(); // be sure!
}

//////////////////////////////////////////////////////////////////////////////////////////////////
// PARTITIONS
//////////////////////////////////////////////////////////////////////////////////////////////////

void cDizMap::PartitionAdd( int brushidx )
{
	const tBrush & brush = brushes.get(brushidx);
	iRect rbrush = brush.rect();
	auto room = Rooms.begin();
	for(iV2 r; r.y < Height(); ++r.y)
		for(r.x = 0; r.x < Width(); ++r.x, ++room)
			if(rbrush.Intersects(RoomBorderRect(r, Room::Border)) )
				room->AddBrush(brushidx);
}

void cDizMap::PartitionMake()
{
	for(int i=0, e=brushes.size(); i < e; ++i)
		PartitionAdd(i);
}

void cDizMap::Resize( int width, int height )
{
	if(width<MAP_SIZEMIN)	width = MAP_SIZEMIN;	// too small
	if(height<MAP_SIZEMIN)	height = MAP_SIZEMIN;	// too small
	if(width>MAP_SIZEMAX)	width = MAP_SIZEMAX;	// too big
	if(height>MAP_SIZEMAX)	height = MAP_SIZEMAX;	// too big
	m_mapw = width / Room::Size.x;
	m_maph = height / Room::Size.y;

	Rooms.clear();
	Rooms.resize(Width() * Height());
	PartitionMake();

	objects.Reindex();
	brushes.Reindex();
	g_game.Resize(Width(), Height());
}

//////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////
