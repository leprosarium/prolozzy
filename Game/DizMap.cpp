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


PREDICATE_M(map, load, 1)
{
	g_map.Reset();
	return g_map.Load(A1);
}

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

PREDICATE_M(map, roomProp, 4)
{
	int rx = A1;
	int ry = A2;
	int idx = A3;
	if(g_map.InvalidRoomCoord(rx, ry))
		throw PlException("invalid room coordinates");
	if(idx < 0 || idx >= Room::PropsNum)
		throw PlException("invalid room property");
	PlTerm val = A4;
	if(val.type() == PL_VARIABLE)
		return A4 = g_map.GetRoomProp( rx, ry, idx );
	int64 v;
	if(!PL_get_int64(val, &v))
		return false;
	g_map.SetRoomProp( rx, ry, idx, v);
	return true;
}

PREDICATE_M(map, roomName, 3)
{
	int rx = A1;
	int ry = A2;
	if(g_map.InvalidRoomCoord(rx, ry)) 
		throw PlException("invalid room coordinates");
	PlTerm val = A3;
	if(val.type() == PL_VARIABLE)
		return val = g_map.GetRoomName( rx, ry ).c_str();
	g_map.SetRoomName( rx, ry, std::string(val) );
	return true;

}
//////////////////////////////////////////////////////////////////////////////////////////////////
// INIT
//////////////////////////////////////////////////////////////////////////////////////////////////
cDizMap::cDizMap() : m_mapw(), m_maph()
{
	guard(cDizMap::cDizMap)
	m_filename[0]=0;

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


//////////////////////////////////////////////////////////////////////////////////////////////////
// LOAD
//////////////////////////////////////////////////////////////////////////////////////////////////
bool cDizMap::Load( const char* filename )
{
	guard(cDizMap::Load)
	if(!filename || !filename[0]) return false; // invalid file
	strcpy(m_filename,filename);
	_strlwr(m_filename);
	dlog(LOGAPP,"Loading map \"%s\"\n",m_filename);

	if(!LoadMap()) { dlog(LOGERR,"Loading map FAILED!\n\n"); return false; }
	dlog(LOGAPP,"Loading map SUCCESSFUL!\n\n");

	PartitionMake();

	g_game.MatMapAlloc();
	g_game.mapW(Width());
	g_game.mapH(Height());
	g_game.roomW(Room::Width);
	g_game.roomH(Room::Height);
	g_game.SetRoom(g_game.roomX(),g_game.roomY()); // updates materialmap and re-gather objects

	return true;
	unguard()
}

#define ERROR_CHUNK( info )	{ F9_FileClose(file); dlog(LOGAPP,"brocken chunk (%s)\n", info); return false; }
bool cDizMap::LoadMap()
{
	guard(cDizMap::LoadMap)

	// open file
	F9FILE file = F9_FileOpen(m_filename);
	if(!file) { dlog(LOGAPP, "  map file not found.\n"); return false; }
	F9_FileSeek(file,0,2);
	int filesize = F9_FileTell(file);
	F9_FileSeek(file,0,0);

	// read chunks
	int size;
	int chunkid=0;
	int chunksize=0;
	int chunkcount=0;
	int count_brush = 0;
	int count_obj = 0;
	int dummy;
	char* buffer;

	while(true)
	{
		if( F9_FileRead(&chunkid,4,file)!=4 )		{ ERROR_CHUNK("header"); }
		if( F9_FileRead(&chunksize,4,file)!=4 )	{ ERROR_CHUNK("header"); }
		
		switch(chunkid)
		{
			case MAP_CHUNKID:
			{
				if( chunksize!=strlen(MAP_ID) )	{ ERROR_CHUNK("size"); }
				buffer = (char*)smalloc(chunksize);
				size = 0;
				size += F9_FileRead(buffer, chunksize, file);
				if(size!=chunksize) { sfree(buffer);  ERROR_CHUNK("size"); }

				if(memcmp(buffer,MAP_ID,chunksize)!=0) { dlog(LOGAPP,"invalid map id: '%s' (current version: '%s')\n", buffer, MAP_ID); sfree(buffer); F9_FileClose(file); return false; }
				sfree(buffer);
				break;
			}

			case MAP_CHUNKINFO2:
			{
				if( chunksize!= 6*4 ) { ERROR_CHUNK("size"); }
				size = 0;
				size += F9_FileRead(&m_mapw, 4, file);
				size += F9_FileRead(&m_maph, 4, file);
				size += F9_FileRead(&Room::Width, 4, file);
				size += F9_FileRead(&Room::Height, 4, file);
				size += F9_FileRead(&dummy, 4, file);
				size += F9_FileRead(&dummy, 4, file);
				if( size!=chunksize ) { ERROR_CHUNK("size"); }
				
				if(Room::Width<32 || Room::Width>1024) { ERROR_CHUNK("bad room width"); }
				if(Room::Height<32 || Room::Height>1024) { ERROR_CHUNK("bad room height"); }
				m_mapw = m_mapw / Room::Width;
				m_maph = m_maph / Room::Height;
				break;
			}

			case MAP_CHUNKMARKERS2:
			{
				F9_FileSeek(file,chunksize,1); // skip
				break;
			}

			case MAP_CHUNKBRUSHES2:
			{
				if(chunksize%(BRUSH_MAX*4)!=0) { ERROR_CHUNK("size"); }
				int data[BRUSH_MAX];
				size = 0;
				while(size<chunksize)
				{
					int ret = F9_FileRead(data, BRUSH_MAX*4, file);
					if(ret!=BRUSH_MAX*4) { ERROR_CHUNK("size"); }
					size += ret;

					int type = data[BRUSH_TYPE];
					std::stringstream id;
					id << "id" << data[BRUSH_ID];
					PlAtom aid(id.str().c_str());
					if(type==0) 
					{
						
						// new brush
						Brushes.push_back(tBrush(data, aid));
						tBrush & brush = Brushes.back();
						count_brush++;

						// add tracker to hash
						if(int id = brush.Get(BRUSH_ID))
							BrushIndex.insert(IntIndex::value_type(aid, Brushes.size() - 1));

					}
					else
					if(type==1) 
					{
						// new object
						Objects.push_back(Object(data, aid));
						Object & obj = Objects.back();
						obj.Set(BRUSH_COLLISION,0);
						count_obj++;

						// add tracker to hash
						if(int id = obj.Get(BRUSH_ID))
							ObjIndex.insert(IntIndex::value_type(aid, Objects.size() - 1));
					}
				}
				break;
			}
			
			default:
			{
				dlog(LOGAPP, "  unknown chunk: id=%x size=%i\n", chunkid, chunksize);
				if(chunksize>0) F9_FileSeek(file,chunksize,1);
			}
		}
		if( F9_FileTell(file)>=filesize) break;

	}

	F9_FileClose(file);
	dlog(LOGAPP,"  map=%ix%i, room=%ix%i, brushes=%i, objects=%i\n", Width(), Height(), Room::Width, Room::Height, count_brush, count_obj );

	int count = Width() * Height();
	if(count<=0) return false; // validate size

	// init roomprops
	Rooms.resize(count);

	return true;
	unguard()
}


bool cDizMap::Reload()
{
	guard(cDizMap::Reload)
	Reset();
	if(!Load(m_filename)) return false;
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
// DATA
//////////////////////////////////////////////////////////////////////////////////////////////////
int cDizMap::GetRoomProp( int rx, int ry, int idx )
{
	guard(cDizMap::GetRoomProp)
	if(Rooms.empty() || InvalidRoomCoord(rx, ry) || Room::InvalidProp(idx)) return 0;
	return GetRoom(rx, ry).Prop(idx);
	unguard()
}
				
void cDizMap::SetRoomProp( int rx, int ry, int idx, int value )	
{
	guard(cDizMap::SetRoomProp)
	if(Rooms.empty() || InvalidRoomCoord(rx, ry) || Room::InvalidProp(idx)) return;
	GetRoom(rx, ry).Prop(idx, value);
	unguard()
}

std::string cDizMap::GetRoomName( int rx, int ry )
{
	guard(cDizMap::GetRoomName)
	if(Rooms.empty() || InvalidRoomCoord(rx ,ry)) return 0;
	return GetRoom(rx, ry).Name();
	unguard()
}
				
void cDizMap::SetRoomName( int rx, int ry, const std::string & name )	
{
	guard(cDizMap::SetRoomName)
	if(Rooms.empty() || InvalidRoomCoord(rx, ry))  return;
	GetRoom(rx, ry).Name(name);
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

//////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////
