//////////////////////////////////////////////////////////////////////////////////////////////////
// DizMap.h
//////////////////////////////////////////////////////////////////////////////////////////////////
#ifndef __DIZMAP_H__
#define __DIZMAP_H__

#include "E9System.h"
#include "DizPaint.h"

#include <vector>
#include <algorithm>

#define MAP_SIZEMIN		128
#define MAP_SIZEMAX		100000

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

class Object : public tBrush
{
	std::string _Name;
public:
	Object() : tBrush() {}
	std::string Name() const { return _Name; }
	void Name(const std::string & name) { _Name = name; std::transform(_Name.begin(), _Name.end(), _Name.begin(), ::toupper);  }
};

template<typename T>
class Reindexed : public Indexed<T>
{
public:
	void Reindex();
};

template<typename T>
void Reindexed<T>::Reindex()
{
	Index.clear();
	for(size_t i = 0, e = size(); i != e; ++i) {
		T & b = get(i);
		if(int id = b.Get(BRUSH_ID))
		{
			std::stringstream sid;
			sid << "id" << id;
			PlAtom aid(sid.str().c_str());
			b.id(aid);
			Index.insert(IntIndex::value_type(aid, i));
		}
	}
}

typedef Reindexed<tBrush> Brushes;
typedef Reindexed<Object> Objects;

//////////////////////////////////////////////////////////////////////////////////////////////////
// cDizMap
//////////////////////////////////////////////////////////////////////////////////////////////////
class cDizMap
{
		int				m_mapw;				// map width in rooms
		int				m_maph;				// map height in rooms

		std::vector<Room> Rooms;

public:
						cDizMap				();
		void			Resize				( int width, int height );	// resize map; return true if no crop occured
		void			Reset				();								// reset when start game; clears map brushes


		bool			Reload				();								// reload map for debug purposes

		// draw
		void			DrawRoom			( const iV2 & rp, int layer, int mode, const iV2 & ofs);	// layer=0..8; mode: 0=normal, 1=material, 2=density

		Room &			GetRoom(int idx) { return Rooms[idx]; }
		Room &			GetRoom(int rx, int ry) { return GetRoom(RoomIdx(rx, ry)); }
		int			    RoomIdx(int rx, int ry) const { return rx + ry * Width(); }
		bool			InvalidRoomCoord(int rx, int ry)	{ return rx < 0 || rx >= Width() || ry < 0 || ry >= Height(); }

		iRect			RoomBorderRect( const iV2 & room, const iV2 & border)	{ return Room::Rect(room).Deflate(border); }

		int				Width() const { return m_mapw; }
		int				Height() const { return m_maph; }

		Brushes brushes;
		Objects objects;
private:
		// partition
		void				PartitionAdd	( int brushidx );	// add a brush to partitioning
		void				PartitionMake	();					// init and partition brushes
};

extern cDizMap	g_map;

#endif
//////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////
