#pragma once

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
	std::vector<Brush *> _Brushes;
	iV2 _pos;
	iRect _rect;
public:
	Room(const iV2 & pos = iV2()) : _pos(pos), _rect(pos * Size, (pos + 1) * Size) { }
	void AddBrush(Brush * b) { _Brushes.push_back(b); }
	const std::vector<Brush *> & Brushes() const { return _Brushes; }

	const iV2 & pos() const { return _pos; }
	const iRect & rect() const { return _rect; }

	iRect borderRect(const iV2 & border) { return iRect(rect()).Deflate(border); }


	static iV2 Pos2Room(const iV2 & p) { return iV2(PosX2Room(p.x), PosY2Room(p.y)); }
	static int PosX2Room(int x) { return x >= 0  ?  x / Size.x : (x + 1) / Size.x - 1; }
	static int PosY2Room(int y) { return y >= 0  ?  y / Size.y : (y + 1) / Size.y - 1; }
};

enum class DrawMode { Normal, Material, Density, None };

typedef Indexed<Brush *, std::string> Brushes;

class DizMap
{
	iV2 _size; // map size

	std::vector<Room> Rooms;
public:
	static const int SizeMin = 128;
	static const int SizeMax = 100000;

	DizMap();

	void Resize(const iV2 & sz);
	void Reset();					// reset when start game; clears map brushes

	bool Reload	();					// reload map for debug purposes

	void DrawRoom(const iV2 & rp, int layer, DrawMode mode, const iV2 & ofs);	// layer=0..8; mode: 0=normal, 1=material, 2=density

	Room & GetRoom(int idx) { return Rooms[idx]; }
	Room & GetRoom(int rx, int ry) { return GetRoom(RoomIdx(rx, ry)); }
	Room & GetRoom(const iV2 & p) { return GetRoom(p.x, p.y); }
	int RoomIdx(int rx, int ry) const { return rx + ry * _size.x; }
	bool InvalidRoomCoord(int rx, int ry)	{ return rx < 0 || rx >= _size.x || ry < 0 || ry >= _size.y; }

	iV2 size() const { return _size; }

	Brushes brushes;
	Brushes objects;

private:
	void PartitionAdd(Brush * brush);
	void PartitionMake();
};

extern DizMap	g_map;
