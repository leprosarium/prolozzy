//////////////////////////////////////////////////////////////////////////////////////////////////
// DizMap.h
//////////////////////////////////////////////////////////////////////////////////////////////////
#ifndef __DIZMAP_H__
#define __DIZMAP_H__

#include "E9System.h"
#include "DizPaint.h"

#include <vector>
#include <algorithm>

class Room
{
public:
	static iV2 Size;
	static const int Border = 16;
private:
	std::vector<int> _Brushes;
public:
	void AddBrush(int idx) { _Brushes.push_back(idx); }
	const std::vector<int> & Brushes() const { return _Brushes; }
	static iV2 Pos2Room(const iV2 & p) { return iV2(PosX2Room(p.x), PosY2Room(p.y)); }
	static int PosX2Room(int x) { return x >= 0  ?  x / Size.x : (x + 1) / Size.x - 1; }
	static int PosY2Room(int y) { return y >= 0  ?  y / Size.y : (y + 1) / Size.y - 1; }
	static iRect Rect(const iV2 & r) { return iRect(r * Size, (r + 1) * Size); } 
};

enum class DrawMode { Normal, Material, Density, None };

typedef Indexed<tBrush *, std::string> Brushes;

//////////////////////////////////////////////////////////////////////////////////////////////////
// cDizMap
//////////////////////////////////////////////////////////////////////////////////////////////////
class cDizMap
{
	iV2 _size; // map size

	std::vector<Room> Rooms;
public:
	static const int SizeMin = 128;
	static const int SizeMax = 100000;

	PlFunctor brush;
	bool UnifyBrush(PlTerm t, tBrush * b);
	tBrush * brushPtrNoEx(PlTerm t) { return reinterpret_cast<tBrush *>(static_cast<void *>(t)); }
	tBrush * brushPtr(PlTerm t) { if(!(t = brush)) throw PlTypeError("brush", t); return brushPtrNoEx(t[1]); }
	cDizMap();

	void Resize(const iV2 & sz);	// resize map;
	void Reset();					// reset when start game; clears map brushes

	bool Reload	();					// reload map for debug purposes

	// draw
	void DrawRoom(const iV2 & rp, int layer, DrawMode mode, const iV2 & ofs);	// layer=0..8; mode: 0=normal, 1=material, 2=density

	Room & GetRoom(int idx) { return Rooms[idx]; }
	Room & GetRoom(int rx, int ry) { return GetRoom(RoomIdx(rx, ry)); }
	int RoomIdx(int rx, int ry) const { return rx + ry * _size.x; }
	bool InvalidRoomCoord(int rx, int ry)	{ return rx < 0 || rx >= _size.x || ry < 0 || ry >= _size.y; }

	iRect RoomBorderRect( const iV2 & room, const iV2 & border)	{ return Room::Rect(room).Deflate(border); }

	iV2 size() const { return _size; }

	Brushes brushes;
	Brushes objects;

private:
	// partition
	void PartitionAdd(int brushidx);	// add a brush to partitioning
	void PartitionMake();				// init and partition brushes
};

extern cDizMap	g_map;

#endif
//////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////
