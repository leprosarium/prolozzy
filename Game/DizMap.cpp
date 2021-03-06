#include "stdafx.h"
#include "PlBrush.h"

#include "DizMap.h"
#include "DizGame.h"
#include "DizApp.h"

#include <sstream>
iV2 Room::Size(GAME_ROOMW, GAME_ROOMH);
DizMap	g_map;

DizMap::DizMap()
{
	Reset();
}

PREDICATE_M(brush, create, 1)
{
	return PlBrush(g_map.objects[g_map.objects.New()]) = A1;
}

PREDICATE_M(brush, createStatic, 1)
{
	return PlBrush(g_map.brushes[g_map.brushes.New()]) = A1;
}

PREDICATE_M(brush, find, 2)
{
	int idx = g_map.objects.Find(A1);
	if(idx == -1)
		return false;
	return PlBrush(g_map.objects.Get(idx)) = A2;
}

PREDICATE_BRUSH_CONT(map, brush, g_map.objects)

#define BRUSH_PROP(Prop, PROP)\
GET_BRUSH_PROP(Prop, PROP)\
SET_BRUSH_PROP(Prop, PROP)

#define GET_BRUSH_PROP(Prop, PROP) PREDICATE_M(brush, get##Prop, 2) { return A2 = PlBrush(A1)->PROP; }
#define SET_BRUSH_PROP(Prop, PROP) PREDICATE_M(brush, set##Prop, 2) { PlBrush(A1)->PROP = A2; return true; }

BRUSH_PROP(Layer, layer)
BRUSH_PROP(X, pos.x)
BRUSH_PROP(Y, pos.y)
BRUSH_PROP(W, size.x)
BRUSH_PROP(H, size.y)
BRUSH_PROP(Tile, tile)
BRUSH_PROP(Frame, frame)
BRUSH_PROP(MapX1, map.p1.x)
BRUSH_PROP(MapY1, map.p1.y)
BRUSH_PROP(MapX2, map.p2.x)
BRUSH_PROP(MapY2, map.p2.y)
BRUSH_PROP(Flip, flip)
BRUSH_PROP(Scale, scale)
BRUSH_PROP(Material, material)
BRUSH_PROP(Draw, draw)
BRUSH_PROP(Delay, delay)
BRUSH_PROP(Anim, anim)
BRUSH_PROP(Collider, collider)

PREDICATE_M(brush, getColor, 2) 
{
	int64 color = static_cast<dword>(PlBrush(A1)->color);
	return A2 = color;
}

PREDICATE_M(brush, setColor , 2) 
{
	int64 color = A2;
	PlBrush(A1)->color = static_cast<dword>(color);
	return true;
}

PREDICATE_M(brush, getShader, 2) 
{
	return A2 = static_cast<int>(PlBrush(A1)->shader);
}

PREDICATE_M(brush, setShader, 2) 
{
	PlBrush(A1)->shader =  static_cast<Blend>(static_cast<int>(A2));
	return true;
}

PREDICATE_M(brush, getID, 2) 
{
	const std::wstring & id = PlBrush(A1)->id;
	return id.empty() ? false : (A2 = id);
}

PREDICATE_M(brush, setID , 2) 
{
	PlBrush(A1)->id = static_cast<LPCWSTR>(A2);
	return true;
}

PREDICATE_M(brush, getDisable, 2) 
{
	return A2 = PlBrush(A1)->disable ? 1 : 0;
}

PREDICATE_M(brush, setDisable , 2) 
{
	PlBrush(A1)->disable = static_cast<int>(A2) != 0;
	return true;
}

PREDICATE_M(brush, getCollision, 2) 
{
	return A2 = PlBrush(A1)->collision ? 1 : 0;
}

PREDICATE_M(brush, setCollision, 2) 
{
	PlBrush(A1)->collision = static_cast<int>(A2) != 0;
	return true;
}

PREDICATE_M(map, size, 1)
{
	iV2 sz = g_map.size();
	bool r1 = A1 = sz.x;
	bool r2 = A2 = sz.y;
	return r1 && r2;
}

PREDICATE_M(map, resize, 2)
{
	g_map.Resize(iV2(A1, A2)); 
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

PREDICATE_M(map, roomW, 1)
{
	return A1 = Room::Size.x;
}

PREDICATE_M(map, roomH, 1)
{
	return A1 = Room::Size.y;
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

void DizMap::Reset()
{
	Rooms.clear();
	Rooms.push_back(Room());
	_size = 1;
	brushes.clear();
	objects.clear();
}

bool DizMap::Reload()
{
	Reset();
	g_script.reloadMap();
	return true;
}

void DizMap::DrawRoom( const iV2 & rp, int layer, DrawMode mode, const iV2 & ofs)
{
	Blend shader;
	dword color;
	if(InvalidRoomCoord(rp)) return;

	Room & room = GetRoom(rp);
	// viewport clipping test
	if( !g_paint.drawtilesoft() && !room.rect().Intersects(iRect(GetRoom(g_game.roomPos).rect()).Offset(-g_game.viewport)))
		return;

	for(Brush *b: room.Brushes())
	{
		Brush & brush = * b;

		if( mode == DrawMode::Normal   && (brush.draw&1)==0 ) continue; // don't draw
		if( mode == DrawMode::Material && (brush.draw&2)==0 ) continue; // don't write material
		if( mode == DrawMode::Density  && (brush.draw&2)==0 ) continue; // don't write material
		
		if( brush.layer != layer ) continue; // filter layer

		iV2 p = brush.pos - room.rect().p1 + ofs;
		int frame = brush.frame;

		if(mode == DrawMode::Material)
		{
			color	= brush.color;
			shader	= brush.shader;
			brush.color = g_game.materials[brush.material].color | 0xff000000;
			brush.shader= Blend::AlphaRep;
			g_paint.drawtilemat = brush.material; // software use this
			g_paint.DrawBrush( brush, p, frame );
			brush.color = color;
			brush.shader = shader;
		}
		else
		if(mode == DrawMode::Density)
		{
			color	= brush.color;
			shader	= brush.shader;
			PlAtom dens = g_game.materials[brush.material].density;
			brush.color = dens == g_game._void ? 0xff000000 :
					dens == g_game.soft ? 0xff606060 :
					dens == g_game.hard ? 0xffa0a0a0 :
					0xffffffff;
			brush.shader= Blend::AlphaRep;
			g_paint.DrawBrush( brush, p, frame );
			brush.color = color;
			brush.shader = shader;
		}
		else
		{
			if(brush.anim==2) // only if looping
				frame += brush.delay > 0 ? g_game.frame / brush.delay : g_game.frame;
			g_paint.DrawBrush( brush, p, frame );
		}
	}

	R9_Flush(); // be sure!
}


void DizMap::Resize( const iV2 & sz )
{
	_size = iV2(std::min(std::max(sz.x, SizeMin), SizeMax),
				std::min(std::max(sz.y, SizeMin), SizeMax)) / Room::Size;

	Rooms.clear();
	for (iV2 r; r.y < _size.y; ++r.y)
		for (r.x = 0; r.x < _size.x; ++r.x)
			Rooms.push_back(Room(r));

	for (Brush * brush : brushes)
		for (Room & room : Rooms)
			if (brush->rect().Intersects(room.borderRect(Room::Border)))
				room.AddBrush(brush);

	objects.Reindex();
	brushes.Reindex();
	g_game.Resize();
}

