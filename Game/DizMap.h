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

#define MAP_ID							"dizzymap"
#define MAP_CHUNKID						0x11111111
#define MAP_CHUNKINFO					0x22222222	// obsolete
#define MAP_CHUNKINFO2					0x22222223
#define MAP_CHUNKMARKERS				0x33333333	// obsolete
#define MAP_CHUNKMARKERS2				0x33333334
#define MAP_CHUNKBRUSHES				0x88888888	// obsolete
#define MAP_CHUNKBRUSHES2				0x88888889


class Room
{
public:
	static const int PropsNum = 8;		// number of room properties
	static int Width;
	static int Height;
	static const int Border = 16;
private:
	std::string _Name;
	int _Props[PropsNum];
	std::vector<int> _Brushes;
public:
	std::string Name() const { return _Name; }
	void Name(const std::string & name) { _Name = name; std::transform(_Name.begin(), _Name.end(), _Name.begin(), ::toupper); }

	int Prop(size_t i) const { return _Props[i]; }
	void Prop(size_t i, int v) { _Props[i] = v; }
	void AddBrush(int idx) { _Brushes.push_back(idx); }
	const std::vector<int> & Brushes() const { return _Brushes; }
	static bool InvalidProp(int idx) { return idx < 0 || idx >= PropsNum; }
	static int PosX2Room(int x) { return x >= 0  ?  x / Room::Width : (x + 1) / Room::Width - 1; }
	static int PosY2Room(int y) { return y >= 0  ?  y / Room::Height : (y + 1) / Room::Height - 1; }
};

class Object : public tBrush
{
	std::string _Name;
public:
//	Object() {}
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
		char			m_filename[128];	// map file name (kept for reloads)

		bool			LoadMap				();								// load brushes and stuff

public:
						cDizMap				();

		void			Reset				();								// reset when start game; clears map brushes

		// load
		bool			Load				( const char* filename );				// load map data; map and objects should be empty before load map

		bool			Reload				();								// reload map for debug purposes

		// draw
		void			DrawRoom			( int rx, int ry, int layer, int mode, int ofsx=0, int ofsy=0 );	// rx=roomx, ry=roomy layer=0..8; mode: 0=normal, 1=material, 2=density; ofsx=ofsetx, ofs=ofsety


		Room &			GetRoom(int idx) { return Rooms[idx]; }
		Room &			GetRoom(int rx, int ry) { return GetRoom(RoomIdx(rx, ry)); }
		int			    RoomIdx(int rx, int ry) const { return rx + ry * Width(); }
		bool			InvalidRoomCoord(int rx, int ry)	{ return rx < 0 || rx >= Width() || ry < 0 || ry >= Height(); }
		int				GetRoomProp			( int rx, int ry, int idx );
		void			SetRoomProp			( int rx, int ry, int idx, int value );
		std::string		GetRoomName			( int rx, int ry );
		void			SetRoomName			( int rx, int ry, const std::string & name );

inline	void			MakeRoomBBW			( int rx, int ry, int &x1, int &y1, int &x2, int &y2, int border=0 )	{ x1=rx*Room::Width-border; y1=ry*Room::Height-border; x2=(rx+1)*Room::Width+border; y2=(ry+1)*Room::Height+border; }

		int				Width() const { return m_mapw; }
		int				Height() const { return m_maph; }

		// brushes
		bool			InvalidBrushIndex(int idx) { return idx < 0 || idx >= BrushCount(); }
inline	int				BrushCount		()					{ return Brushes.size(); }
inline	tBrush &		BrushGet		( int idx )			{ return Brushes[idx]; }
inline	int				BrushFind		( atom_t id )		{ IntIndex::iterator i = BrushIndex.find(id); if(i != BrushIndex.end()) return i->second; return -1; }


		// objects
		bool			InvalidObjIndex(int idx) { return idx < 0 || idx >= ObjCount(); }
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
