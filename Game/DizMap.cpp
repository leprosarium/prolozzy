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

PREDICATE_M(map, size, 1)
{
	iV2 sz = g_map.size();
	bool r1 = A1 = sz.x;
	bool r2 = A2 = sz.y;
	return r1 && r2;
}

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

#define DEFINE_GET_BRUSH_PROP(pred, member) \
PREDICATE_M(brush, pred, 2) \
{ \
	if(tBrush * brush = g_map.brushes.Get(A1)) \
		return A2 = brush->member; \
	throw PlException("invalid brush index"); \
}

#define DEFINE_SET_BRUSH_PROP(pred, member) \
PREDICATE_M(brush, pred, 2) \
{ \
	if(tBrush * brush = g_map.brushes.Get(A1)) \
	{ \
		brush->member = A2; \
		return true; \
	} \
	throw PlException("invalid brush index"); \
}

#define DEFINE_BRUSH_PROP(getPred, setPred, member) \
DEFINE_GET_BRUSH_PROP(getPred, member) \
DEFINE_GET_BRUSH_PROP(setPred, member)

DEFINE_BRUSH_PROP(layer, setLayer, layer)
DEFINE_BRUSH_PROP(x, setX, pos.x)
DEFINE_BRUSH_PROP(y, setY, pos.y)
DEFINE_BRUSH_PROP(w, setW, size.x)
DEFINE_BRUSH_PROP(h, setH, size.y)
DEFINE_BRUSH_PROP(tile, setTile, tile)
DEFINE_BRUSH_PROP(frame, setFrame, frame)
DEFINE_BRUSH_PROP(x1, setX1, map.p1.x)
DEFINE_BRUSH_PROP(y1, setY1, map.p1.y)
DEFINE_BRUSH_PROP(x2, setX2, map.p2.x)
DEFINE_BRUSH_PROP(y2, setY2, map.p2.y)
DEFINE_BRUSH_PROP(flip, setFlip, flip)
DEFINE_BRUSH_PROP(scale, setScale, scale)
DEFINE_BRUSH_PROP(id, setId, id)
DEFINE_BRUSH_PROP(material, setMaterial, material)
DEFINE_BRUSH_PROP(delay, setDelay, delay)
DEFINE_BRUSH_PROP(collision, setCollision, collision)

PREDICATE_M(brush, color, 2)
{
	if(tBrush * brush = g_map.brushes.Get(A1))
		return A2 = static_cast<int>(brush->color);
	throw PlException("invalid brush index");
}
PREDICATE_M(brush, shader, 2)
{
	if(tBrush * brush = g_map.brushes.Get(A1))
		return A2 = static_cast<int>(brush->shader);
	throw PlException("invalid brush index");
}

PREDICATE_M(brush, draw, 2)
{

	if(tBrush * brush = g_map.brushes.Get(A1))
	{
		int dr = 0; 
		if(brush->drawImg) dr = 1;
		if(brush->drawMat) dr &= 2;
		return A2 = dr;
	}
	throw PlException("invalid brush index");
}
PREDICATE_M(brush, disabled, 1)
{
	if(tBrush * brush = g_map.brushes.Get(A1))
		return A2 = brush->disabled;
	throw PlException("invalid brush index");
}

PREDICATE_M(brush, anim, 2)
{
	if(tBrush * brush = g_map.brushes.Get(A1))
		return A2 = static_cast<int>(brush->anim);
	throw PlException("invalid brush index");
}

PREDICATE_M(brush, collider, 2)
{
	if(tBrush * brush = g_map.brushes.Get(A1))
	{
		int cl = 0; 
		if(brush->collideHandler) cl = 1;
		if(brush->collideHard) cl &= 2;
		return A2 = cl;
	}
	throw PlException("invalid brush index");
}


PREDICATE_M(brush, setColor, 2)
{
	if(tBrush * brush = g_map.brushes.Get(A1))
	{
		int64 c = A2;
		brush->color = static_cast<dword>(c);
		return true;
	}
	throw PlException("invalid brush index");
}

PREDICATE_M(brush, setShader, 2)
{
	if(tBrush * brush = g_map.brushes.Get(A1))
	{
		int v = A2;
		if (v <  static_cast<int>(Blend::Min) || v >= static_cast<int>(Blend::Max))
			return false;
		brush->shader = static_cast<Blend>(v);
	}
	throw PlException("invalid brush index");
}

PREDICATE_M(brush, setDraw, 2)
{

	if(tBrush * brush = g_map.brushes.Get(A1))
	{
		int dr = A2; 
		brush->drawImg = (dr & 1) != 0;
		brush->drawMat = (dr & 2) != 0;
		return true;
	}
	throw PlException("invalid brush index");
}
PREDICATE_M(brush, setDisabled, 1)
{
	if(tBrush * brush = g_map.brushes.Get(A1))
	{
		brush->disabled = static_cast<int>(A2) != 0;
		return true;
	}
	throw PlException("invalid brush index");
}

PREDICATE_M(brush, setAnim, 2)
{
	if(tBrush * brush = g_map.brushes.Get(A1))
	{
		int a = A2;
		if(a == static_cast<int>(Anim::None)) brush->anim = Anim::None;
		else if (a == static_cast<int>(Anim::Once)) brush->anim = Anim::Once;
		else if (a == static_cast<int>(Anim::Loop)) brush->anim = Anim::Loop;
		else return false;
		return true;
	}
	throw PlException("invalid brush index");
}

PREDICATE_M(brush, setCollider, 2)
{
	if(tBrush * brush = g_map.brushes.Get(A1))
	{
		int cl = A2; 
		brush->collideHandler = (cl & 1) != 0;
		brush->collideHard = (cl & 2) != 0;
		return true;
	}
	throw PlException("invalid brush index");
}




PREDICATE_M(map, brushVar, 3)
{
	assert(1);
	return false;
	//int idx = A1;
	//int var = A2;
	//if(g_map.brushes.InvalidIdx(idx))
	//	throw PlException("invalid brush index");
	//if(tBrush::InvalidProp(var)) 
	//	throw PlException("invalid brush variable");
	//if(tBrush * brush = g_map.brushes.Get(idx))
	//{
	//	PlTerm val = A3;
	//	if(val.type() == PL_VARIABLE)
	//		return A3 = brush->Get(var);
	//	brush->Set(var, val);
	//	return true;
	//}
	//return false;
}

PREDICATE_M(map, brushSet , 3) 
{
	assert(1);
	return false;
	//int idx = A1;
	//if(g_map.brushes.InvalidIdx(idx))
	//	throw PlException("invalid brush index");
	//int var = A2;
	//if(tBrush::InvalidProp(var)) 
	//	throw PlException("invalid brush variable");
	//if(tBrush * brush = g_map.brushes.Get(idx))
	//{

	//	if(var == BRUSH_COLOR)
	//	{
	//		int64 color = A3;
	//		brush->Set(var, static_cast<int>(color));
	//	} 
	//	else 
	//	{
	//		brush->Set(var, A3);
	//	}
	//	return true;
	//}
	//return false;
}

PREDICATE_M(map, objSet , 3) 
{
	assert(1);
	return false;

	//int idx = A1;
	//if(g_map.objects.InvalidIdx(idx)) 
	//	throw PlException("invalid object index");
	//int var = A2;
	//if(Object::InvalidProp(var)) 
	//	throw PlException("invalid object variable"); 

	//tBrush & obj = g_map.objects.get(idx);
	//if(var == BRUSH_COLOR)
	//{
	//	int64 color = A3;
	//	obj.Set(var, static_cast<int>(color));
	//} 
	//else 
	//{
	//	obj.Set(var, A3);
	//}
	//return true;
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
	assert(1);
	return false;
	//int idx = A1;
	//if(g_map.objects.InvalidIdx(idx)) 
	//	throw PlException("invalid object index");
	//int var = A2;
	//if(Object::InvalidProp(var)) 
	//	throw PlException("invalid object variable"); 

	//tBrush & obj = g_map.objects.get(idx);
	//PlTerm val = A3;
	//if(val.type() == PL_VARIABLE)
	//	return A3 = obj.Get(var);
	//int64 v = val;
	//obj.Set(var, static_cast<int>(v)); 
	//return true;
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

//////////////////////////////////////////////////////////////////////////////////////////////////
// INIT
//////////////////////////////////////////////////////////////////////////////////////////////////
cDizMap::cDizMap()
{
}


void cDizMap::Reset()
{
	Rooms.clear();
	_size = 0;
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
	if(InvalidRoomCoord(rp.x, rp.y)) return;

	// viewport clipping test
	if( !g_paint.drawtilesoft() )
	{
		iV2 p1 = g_game.roomPos() * Room::Size - g_game.viewportPos();
		iRect viewport(p1, p1 + Room::Size);
		if(!(rp * Room::Size < viewport.p2 && (rp + 1) * Room::Size > viewport.p1))
			return;


	}
	dword color;
	Blend shader;
	const std::vector<int> & part = GetRoom(rp).Brushes();
	for(size_t i=0;i<part.size();i++)
	{
		int brushidx = part[i];

		tBrush & brush = * brushes.Get(brushidx);

		if( mode==DRAWMODE_NORMAL	&& !brush.drawImg) continue; // don't draw
		if( mode==DRAWMODE_MATERIAL && !brush.drawMat) continue; // don't write material
		if( mode==DRAWMODE_DENSITY  && !brush.drawMat) continue; // don't write material
		
		if( brush.layer != layer ) continue; // filter layer

		iV2 p = brush.pos - rp * Room::Size + ofs;
		int frame = brush.frame;

		if(mode==DRAWMODE_MATERIAL)
		{
			// use special color and shader
			color	= brush.color;
			shader	= brush.shader;
			brush.color = g_game.materials[brush.material].color | 0xff000000;
			brush.shader = Blend::AlphaRep;
			g_paint.drawtilemat(brush.material); // software use this
			g_paint.DrawBrush( brush, p, frame );
			brush.color = color;
			brush.shader = shader;
		}
		else
		if(mode==DRAWMODE_DENSITY)
		{
			// use special color and shader
//			dword matd_color[MATD_MAX] = {0xff000000,0xff606060,0xffa0a0a0,0xffffffff};
			color	= brush.color;
			shader	= brush.shader;
			PlAtom dens = g_game.materials[brush.material].density;
			dword matd_color = dens == g_game._void ? 0xff000000 : 
					dens == g_game.soft ? 0xff606060 :
					dens == g_game.hard ? 0xffa0a0a0 :
					0xffffffff;
			brush.color = matd_color;
			brush.shader = Blend::AlphaRep;
			g_paint.DrawBrush( brush, p, frame );
			brush.color = color;
			brush.shader = shader;
		}
		else
		{
			if(brush.anim == Anim::Loop) // only if looping
			{
				int gameframe = g_game.m_gameframe;
				if(brush.delay > 0) gameframe /= brush.delay;
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
	for(iV2 r; r.y < size().y; ++r.y)
		for(r.x = 0; r.x < size().x; ++r.x, ++room)
			if(rbrush.Intersects(RoomBorderRect(r, Room::Border)) )
				room->AddBrush(brushidx);
}

void cDizMap::PartitionMake()
{
	for(int i=0, e=brushes.size(); i < e; ++i)
		PartitionAdd(i);
}

void cDizMap::Resize( const iV2 & sz )
{
	_size = iV2(std::min(std::max(sz.x, MAP_SIZEMIN), MAP_SIZEMAX),
				std::min(std::max(sz.y, MAP_SIZEMIN), MAP_SIZEMAX)) / Room::Size;

	Rooms.clear();
	Rooms.resize(size().x * size().y);
	PartitionMake();

	objects.Reindex();
	brushes.Reindex();
	g_game.Resize();
}

//////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////
