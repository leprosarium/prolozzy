//////////////////////////////////////////////////////////////////////////////////////////////////
// DizMap.h
//////////////////////////////////////////////////////////////////////////////////////////////////
#ifndef __DIZMAP_H__
#define __DIZMAP_H__

#include "E9System.h"
#include "D9Debug.h"
#include "DizPaint.h"

#include <vector>
#include <hash_map> 
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
	static int PosX2Room(int x) { return x >= 0  ?  x / Size.x : (x + 1) / Size.x - 1; }
	static int PosY2Room(int y) { return y >= 0  ?  y / Size.y : (y + 1) / Size.y - 1; }
};

class Object : public tBrush
{
	std::string _Name;
public:
	Object() : tBrush() {}
	Object(int (&data)[BRUSH_MAX], const PlAtom & id) : tBrush(data, id) {}
	std::string Name() const { return _Name; }
	void Name(const std::string & name) { _Name = name; std::transform(_Name.begin(), _Name.end(), _Name.begin(), ::toupper);  }
};

//////////////////////////////////////////////////////////////////////////////////////////////////
// cDizMap
//////////////////////////////////////////////////////////////////////////////////////////////////
class cDizMap
{
		int				m_mapw;				// map width in rooms
		int				m_maph;				// map height in rooms

		std::vector<tBrush> Brushes;							// brushes list (static brushes)
		IntIndex BrushIndex;

		std::vector<Object>	Objects;								// objects list (dynamic brushes)
		IntIndex			ObjIndex;							// objects hash
		std::vector<Room> Rooms;

public:
						cDizMap				();
		void			Resize				( int width, int height );	// resize map; return true if no crop occured
		void			Reset				();								// reset when start game; clears map brushes


		bool			Reload				();								// reload map for debug purposes

		// draw
		void			DrawRoom			( int rx, int ry, int layer, int mode, int ofsx=0, int ofsy=0 );	// rx=roomx, ry=roomy layer=0..8; mode: 0=normal, 1=material, 2=density; ofsx=ofsetx, ofs=ofsety


		Room &			GetRoom(int idx) { return Rooms[idx]; }
		Room &			GetRoom(int rx, int ry) { return GetRoom(RoomIdx(rx, ry)); }
		int			    RoomIdx(int rx, int ry) const { return rx + ry * Width(); }
		bool			InvalidRoomCoord(int rx, int ry)	{ return rx < 0 || rx >= Width() || ry < 0 || ry >= Height(); }

inline	void			MakeRoomBBW			( int rx, int ry, int &x1, int &y1, int &x2, int &y2, int border=0 )	{ x1=rx*Room::Size.x-border; y1=ry*Room::Size.y-border; x2=(rx+1)*Room::Size.x+border; y2=(ry+1)*Room::Size.y+border; }

		int				Width() const { return m_mapw; }
		int				Height() const { return m_maph; }

		// brushes
		bool			InvalidBrushIndex(int idx) { return idx < 0 || idx >= BrushCount(); }
		int			    BrushNew		();
inline	int				BrushCount		()					{ return Brushes.size(); }
inline	tBrush &		BrushGet		( int idx )			{ return Brushes[idx]; }
inline	int				BrushFind		( atom_t id )		{ IntIndex::iterator i = BrushIndex.find(id); if(i != BrushIndex.end()) return i->second; return -1; }


		// objects
		bool			InvalidObjIndex(int idx) { return idx < 0 || idx >= ObjCount(); }
		int			    ObjNew		();
inline	int					ObjCount		()					{ return Objects.size(); }
inline	Object &			ObjGet			( int idx )			{ return Objects[idx]; }
inline	int					ObjFind			( int id )			{ IntIndex::iterator i = ObjIndex.find(id); if(i != ObjIndex.end()) return i->second; return -1; }

private:
		// partition
		void				PartitionAdd	( int brushidx );	// add a brush to partitioning
		void				PartitionMake	();					// init and partition brushes
};

extern cDizMap	g_map;

#endif
//////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////
