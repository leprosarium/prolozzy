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
	typedef std::vector<Brush *> BrushSet;
private:
	BrushSet _Brushes;
	iV2 _pos;
	iRect _rect;
public:
	Room(const iV2 & pos = iV2()) : _pos(pos), _rect(pos * Size, iRect::SZ(Size)) { }
	void AddBrush(Brush * b) { _Brushes.push_back(b); }
	const BrushSet & Brushes() const { return _Brushes; }

	const iV2 & pos() const { return _pos; }
	const iRect & rect() const { return _rect; }

	iRect borderRect(const iV2 & border) { return iRect(rect()).Deflate(border); }
	static iV2 Pos2Room(const iV2 & p) { return p / Size; }
};

enum class DrawMode { Normal, Material, Density, None };

typedef Indexed<Brush *, std::wstring> Brushes;

class DizMap
{
	iV2 _size;

	std::vector<Room> Rooms;
public:
	static const int SizeMin = 128;
	static const int SizeMax = 100000;

	DizMap();

	void Resize(const iV2 & sz);
	void Reset();
	bool Reload();
	void DrawRoom(const iV2 & r, int layer, DrawMode mode, const iV2 & ofs);

	Room & GetRoom(const iV2 & r) { return Rooms[RoomIdx(r)]; }
	int RoomIdx(const iV2 & r) const { return r.x + r.y * _size.x; }
	bool InvalidRoomCoord(const iV2 & r) const { return r.x < 0 || r.x >= _size.x || r.y < 0 || r.y >= _size.y; }

	iV2 size() const { return _size; }

	Brushes brushes;
	Brushes objects;
};

extern DizMap	g_map;
