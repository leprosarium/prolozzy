//////////////////////////////////////////////////////////////////////////////////////////////////
// DizPaint.h
//////////////////////////////////////////////////////////////////////////////////////////////////
#ifndef __DIZPAINT_H__
#define __DIZPAINT_H__

#include "R9Font.h"
#include "DizDef.h"

#include <hash_map>
#include <deque> 

//////////////////////////////////////////////////////////////////////////////////////////////////
// DEFINES
//////////////////////////////////////////////////////////////////////////////////////////////////

// colors
#define INK_BLACK		0xff000000
#define INK_WHITE		0xffffffff

// shaders
#define SHADER_OPAQUE	0		// BLEND_OPAQUE
#define	SHADER_BLEND	1		// BLEND_ALPHA
#define SHADER_ADD		2		// BLEND_ADD
#define SHADER_MOD		3		// BLEND_MOD
#define SHADER_MOD2		4		// BLEND_MOD2
#define SHADER_ALPHAREP	5		// BLEND_ALPHAREP
#define SHADER_MAX		6

// macros
#define SCALE			( g_paint.m_scale )
#define SCALEX(x)		( g_paint.m_scrx+(x)*g_paint.m_scale )
#define SCALEY(y)		( g_paint.m_scry+(y)*g_paint.m_scale )
#define SCALESIZE(w)	( (w)*g_paint.m_scale )

//////////////////////////////////////////////////////////////////////////////////////////////////
// TILE
//////////////////////////////////////////////////////////////////////////////////////////////////

class cTile
{
	cTile(const cTile &);
public:
		int			m_id;						// unique id >=0
		int			m_group;					// resource group
		int			m_frames;					// number of frames
		int         fx;
		int			fy;
		R9TEXTURE	m_tex;						// texture
		r9Img		m_img;						// img alpha mask
		char*		m_name;						// tile name
						
					cTile(int m_id) : m_id(m_id), m_group(), m_frames(1), fx(1), fy(1), m_tex(NULL), m_name(NULL) { }
					~cTile() { guardfast(~cTile); if(m_tex)  R9_TextureDestroy(m_tex);  R9_ImgDestroy(&m_img); if(m_name)  sfree(m_name); unguardfast(); }
	    int         GetFx(int frame) const		{ return frame % fx; }
		int         GetFy(int frame) const		{ return frame / fx; }
		int			GetWidth()					{ return fx > 0 ? R9_TextureGetWidth(m_tex) / fx : R9_TextureGetWidth(m_tex); }
		int			GetHeight()					{ return fy > 0 ? R9_TextureGetHeight(m_tex) / fy : R9_TextureGetHeight(m_tex); }
};

//////////////////////////////////////////////////////////////////////////////////////////////////
// FONT
//////////////////////////////////////////////////////////////////////////////////////////////////
class cFont
{
public:
		int			m_id;						// unique id >=0
		int			m_group;					// resource group
		r9Font*		m_font;						// font
		
					cFont()						{ m_id=0; m_group=0; m_font=NULL; }
					~cFont()					{ if(m_font) sdelete(m_font); }
inline	int			GetSize()					{ return (int)m_font->GetSize(); }
inline	int			GetCharWidth( char c )		{ return (int)m_font->GetCharWidth(c); }
inline	int			GetTextWidth( const char* text )	{ return (int)m_font->GetTextWidth(text); }
inline	int			GetOfsX()					{ return (int)m_font->GetOfsX(); }
inline	int			GetOfsY()					{ return (int)m_font->GetOfsY(); }
};

//////////////////////////////////////////////////////////////////////////////////////////////////
// BRUSH OBJECTS
//////////////////////////////////////////////////////////////////////////////////////////////////
#define BRUSH_LAYER		0			// layer idx
#define BRUSH_X			1			// position x in world
#define BRUSH_Y			2			// position y in world
#define BRUSH_W			3			// width of the object (usually the sprite width)
#define BRUSH_H			4			// height of the object (usually the sprite height)
#define BRUSH_TILE		5			// tile id
#define BRUSH_FRAME		6			// current tile animation frame (starts with 0)
#define BRUSH_MAP		7			// 4 values: x1,y1,x2,y2
#define BRUSH_FLIP		11			// flip: 0=none, 1=x, 2=y, 3=xy
#define BRUSH_COLOR		12			// color
#define BRUSH_SHADER	13			// shader 0..SHADER_MAX
#define BRUSH_SCALE		14			// scale value (0=1, 1, 2, ...)

#define BRUSH_TYPE		16			// type: 0=static(for map drawing), 1=dynamic(for objects)
#define BRUSH_ID		17			// id used to access from script (brush + obj)
#define BRUSH_MATERIAL	18			// material that brushes will write in material map if draw set correctly (brush only)
#define BRUSH_DRAW		19			// draw mode: 0=don't draw, 1=draw in view, 2=write material in material map (brush only), 3=both (brush only)
#define BRUSH_DISABLE	20			// 0=enable, 1=disabled (don't draw, don't update)      
#define BRUSH_DELAY		21			// frame delay ( should be updated only once in BRUSH_DELAY frames ) (brush + obj)
#define BRUSH_ANIM		22			// animation mode: 0=none, 1=play once, 2=play loop 
#define BRUSH_COLLIDER	23			// colider mode: 0=none, 1=call handler, 2=hard collision, 3=both
#define BRUSH_CLASS		24			// generic object class (pickable, action, hurt and others)
#define BRUSH_STATUS	25			// generic object status (used to select different behaviours)
#define BRUSH_TARGET	26			// generic object target id (used to link objects)
#define BRUSH_DEATH		27			// death cause, set in player P_DEATH for hurt and kill objects
#define BRUSH_COLLISION	31			// if collision with player 1, else 0 (updated per frame)

#define BRUSH_USER		32			// more user defined free values
#define BRUSH_MAX		48			// dummy

struct tBrush
{
	int		m_data[BRUSH_MAX];
	PlAtom _id;
public:
	tBrush() : _id("0") { 	memset(m_data, 0, sizeof(m_data)); Set(BRUSH_TILE, -1); Set(BRUSH_COLOR, 0xffffffff); }
	tBrush(int (&data)[BRUSH_MAX], const PlAtom &_id) : _id(_id) { memcpy(m_data, data, sizeof(m_data)); }
	PlAtom id() const { return _id;}
	void id(const PlAtom &id) { _id = id; }
	int Get( int idx ) const { return m_data[idx]; }
	void Set( int idx, int val ) { m_data[idx] = val; }
	void MakeBBW	( int &x1, int &y1, int &x2, int &y2 ) const{ x1 = m_data[BRUSH_X]; x2 = x1+m_data[BRUSH_W]; y1 = m_data[BRUSH_Y]; y2 = y1+m_data[BRUSH_H]; }
	float mapScale() const { return m_data[BRUSH_SCALE] > 0 ? m_data[BRUSH_SCALE] / 100.0f : 1.0f; }		
	float mapWith() const   { return ( (m_data[BRUSH_FLIP] & R9_FLIPR) ? (m_data[BRUSH_MAP+3] - m_data[BRUSH_MAP+1]) : (m_data[BRUSH_MAP+2] - m_data[BRUSH_MAP+0]) ) * mapScale(); }
	float mapHeight() const { return ( (m_data[BRUSH_FLIP] & R9_FLIPR) ? (m_data[BRUSH_MAP+2] - m_data[BRUSH_MAP+0]) : (m_data[BRUSH_MAP+3] - m_data[BRUSH_MAP+1]) ) * mapScale(); }
	static bool InvalidProp(int idx) { return idx < 0 || idx >= BRUSH_MAX; }	
};

//////////////////////////////////////////////////////////////////////////////////////////////////
// DIZPAINT
//////////////////////////////////////////////////////////////////////////////////////////////////

class Tiles : std::vector<cTile*>
{
	typedef std::vector<cTile*> Cont;
	IntIndex Index;
public:
	using Cont::size;
	using Cont::iterator;
	using Cont::const_iterator;
	using Cont::begin;
	using Cont::end;

	~Tiles() { clear(); }
	void clear() {  for(iterator i = begin(), e = end(); i != e; ++i) delete *i; Index.clear(); Cont::clear(); }
	void erase(iterator i) { value_type tile = *i; Index.erase(tile->m_id); delete tile; Cont::erase(i); }


	void Done();
	bool InvalidIdx(int idx) {return idx < 0 && idx >= size(); }

	cTile * Get(int idx) { if(InvalidIdx(idx)) return 0; return (*this)[idx];}
	int Add(int id);								// add a new empty tile; id must be unique
	void Del(int idx);								// delete tile by index
	int Find(int id) { IntIndex::iterator i = Index.find(id); return i != Index.end() ? i->second : -1; }
	bool Load(char* path, int group = 0);			// load tiles from a path, and set the specified group
	bool LoadFile(const char* filepath, int group = 0);	// load a tile file
	void Unload(int group=0 );						// unload load tiles (destroy) from a specified group
};

class cDizPaint
{
public:
						cDizPaint		();
						~cDizPaint		();
	
		bool			Init			();
		void			Done			();
		bool			Reacquire		();								// called before render reset to reload render resources
		void			Unacquire		();								// called before render reset to free render resources
		void			Layout			();								// compute layout position (scale,scrx,scry)

		// Draw scaled
		void			DrawTile		( int idx, int x, int y, iRect& map, dword color=0xffffffff, int flip=0, int frame=0, int blend=R9_BLEND_ALPHA, float scale=1.0f );	// tile scale (in editor paint it was full scale)
		void			DrawTile		( int idx, int x, int y, dword color=0xffffffff, int flip=0, int frame=0, int blend=R9_BLEND_ALPHA, float scale=1.0f );				// tile scale (in editor paint it was full scale)
		void			DrawChar		( int fontidx, int x, int y, char c, dword color=0xffffffff );

		// Draw brush 
		void			DrawBrush		( const tBrush & brush, int x, int y, int frame=-1 ); // if frame is -1, tile is automatic animated

		// Tile material draw (software)
		void			DrawTileSoft	( int idx, int x, int y, iRect& map, dword color=0xffffffff, int flip=0, int frame=0, int blend=R9_BLEND_ALPHA, float scale=1.0f );	// paints tile in the image target map (faster, no rotation, no scale)
		void			DrawTileSoft2	( int idx, int x, int y, iRect& map, dword color=0xffffffff, int flip=0, int frame=0, int blend=R9_BLEND_ALPHA, float scale=1.0f );	// paints tile in the image target map (accept rotation and scale)
		r9Img			m_imgtarget;	// target image in PF_A8 format (pointing to material map data)
		bool			m_drawtilesoft;	// true for DrawBrush to call DrawTileSoft
		byte			m_drawtilemat;	// material to draw the tile

		// HUD draw functions
		void			HudClipping		( iRect& dst );																	// set a clipping rect
		void			HUDDrawTile		( int tileid, iRect& dst, iRect& src, dword flags, int frame );				// draw tile
		void			HUDDrawText		( int tileid, iRect& dst, char* text, int align );								// draw text with escape commands
		void			HUDGetTextSize	( char* text, int& w, int& h, int&c, int&r );						// in text's width and height in pixels and the number of columns and rows
		int				HUDScanText		( char* text, int start, int& end, int* data );									// helper for hud text; scans for command and return command and data info
		
		int				m_hudfont;		// current font id
		int				m_hudshader;	// current hud shader
		dword			m_hudcolor;		// current hud color
		int				m_huddraw;		// draw allowed

		// screen props	
		int				m_scrx;			// screen offset x
		int				m_scry;			// screen offset y
		int				m_scale;		// scale factor

		// fonts
		cFont*			FontGet			( int idx )						{ if(0<=idx && idx<m_font.size()) return m_font[idx]; else return 0; }
		void			FontDel			( int idx );
		bool			FontLoad		( char* path, int group=0 );	// load fonts from a path and set the specified group
		bool			FontLoadFile	( const char* filepath, int group=0 );// load a font file
		void			FontUnload		( int group=0 );				// unload fonts (destroy) from the specified group
		int				FontFind		( int id )						{ for(int i=0;i<m_font.size();i++) if(m_font[i]->m_id==id) return i; return -1; }

		std::vector<cFont*> m_font;			// fonts list
		Tiles tiles;
};

//////////////////////////////////////////////////////////////////////////////////////////////////
// UTILS
//////////////////////////////////////////////////////////////////////////////////////////////////
extern	cDizPaint	g_paint;

inline int ComputeFrame( int frame, int framecount, int anim )
{
	if( anim==1 )
	{
		if( frame>framecount-1 ) 
			frame=framecount-1;
		else
		if( frame<0 )
			frame=0;
	}
	else
	if( anim==2 )
	{
		if( framecount>0 ) 
			frame = frame % framecount;
		else
			frame = 0; // @TODO work on this negative looping !!!
	}
	
	return frame;
}

#endif
//////////////////////////////////////////////////////////////////////////////////////////////////
