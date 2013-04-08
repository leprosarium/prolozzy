//////////////////////////////////////////////////////////////////////////////////////////////////
// DizPaint.h
//////////////////////////////////////////////////////////////////////////////////////////////////
#ifndef __DIZPAINT_H__
#define __DIZPAINT_H__

#include "R9Font.h"
#include "DizDef.h"

#include <deque> 
#include <algorithm>

//////////////////////////////////////////////////////////////////////////////////////////////////
// TILE
//////////////////////////////////////////////////////////////////////////////////////////////////


class cTile
{
	cTile(const cTile &);
	cTile & operator = (const cTile &);
public:
		int			id;						// unique id >=0
		int			group;					// resource group
		int			frames;					// number of frames
		int         fx;
		int			fy;
		R9TEXTURE	tex;						// texture
		r9Img		img;						// img alpha mask
		std::string	name;						// tile name
		
		cTile(int id) : id(id), group(), frames(1), fx(1), fy(1), tex(nullptr) { }
		cTile(cTile && t) : 
			id(t.id),
			group(t.group),
			frames(t.frames),
			fx(t.fx),
			fy(t.fy),
			tex(t.tex),
			img(std::move(t.img)),
			name(std::move(t.name))
		{
			t.tex = 0; 
		}

		cTile & operator = (cTile && t) 
		{
			id = t.id;
			group = t.group;
			frames = t.frames;
			fx = t.fx;
			fy = t.fy;
			tex = t.tex;
			name = std::move(t.name);
			img = std::move(t.img);
			t.tex = 0;
			return *this;
		}
		~cTile() { Destroy();  R9_ImgDestroy(&img); }
		void Destroy() { if(tex) { R9_TextureDestroy(tex); tex = 0; } }

		int         GetFx(int frame) const		{ return frame % fx; }
		int         GetFy(int frame) const		{ return frame / fx; }
		iV2			GetF(int frame) const		{ return iV2(GetFx(frame), GetFy(frame)); }
		int			GetWidth() const			{ return fx > 0 ? R9_TextureGetWidth(tex) / fx : R9_TextureGetWidth(tex); }
		int			GetHeight()	const			{ return fy > 0 ? R9_TextureGetHeight(tex) / fy : R9_TextureGetHeight(tex); }
		iV2			GetSize() const { return iV2(GetWidth(), GetHeight()); }
		iV2			TexSize() const { return iV2(R9_TextureGetWidth(tex), R9_TextureGetHeight(tex)); }
		fRect		FrameRect(int frame, const iRect & map) const { fRect src = map; return src.Offset(GetF(frame) * GetSize()); }
		fRect		FrameRect(int frame) const { iV2 sz = GetSize(); iV2 p = GetF(frame) * sz; return fRect(p, p + sz); }

		int ComputeFrameOnce(int frame) const { return std::max(std::min(frame, frames - 1), 0); }
		int ComputeFrameLoop(int frame) const { return frames > 0 ? frame % frames : 0; }
		int ComputeFrame( int frame, int anim ) const
		{
			if( anim==1 ) return ComputeFrameOnce(frame);
			if( anim==2 ) return ComputeFrameLoop(frame);
			return frame;
		}


};

//////////////////////////////////////////////////////////////////////////////////////////////////
// FONT
//////////////////////////////////////////////////////////////////////////////////////////////////
class cFont
{
	cFont(const cFont &);
	cFont & operator = (const cFont &);
public:
	int id;						// unique id >=0
	int group;					// resource group
	r9Font * font;						// font
		
	cFont() : id(), group(), font() {}
	cFont(int id, int group, r9Font * font) : id(id), group(group), font(font) {}
	~cFont() { if(font) delete font; }
	cFont(cFont && f) : id(f.id), group(f.group), font(f.font) { f.font = 0; }
	cFont & operator = (cFont && f) { id = f.id; group = f.group; font = f.font; f.font = 0; return *this; }

	int GetSize() const { return (int)font->GetSize(); }
	int GetCharWidth( char c ) const { return (int)font->GetCharWidth(c); }
	int GetTextWidth( const char* text ) const { return (int)font->GetTextWidth(text); }
	int	GetOfsX() const { return (int)font->GetOfsX(); }
	int GetOfsY() const { return (int)font->GetOfsY(); }
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
	Blend _shader;
public:
	tBrush() : _id("0"), _shader(Blend::Opaque) { 	memset(m_data, 0, sizeof(m_data)); Set(BRUSH_TILE, -1); Set(BRUSH_COLOR, 0xffffffff); }
//	tBrush(int (&data)[BRUSH_MAX], const PlAtom &_id) : _id(_id) { memcpy(m_data, data, sizeof(m_data)); }
	PlAtom id() const { return _id;}
	void id(const PlAtom &id) { _id = id; }
	int Get( int idx ) const { if (idx == BRUSH_SHADER) return static_cast<int>(shader()); return m_data[idx]; }
	void Set( int idx, int val ) { m_data[idx] = val; if(idx == BRUSH_SHADER) shader(static_cast<Blend>(val)); }
	void MakeBBW	( int &x1, int &y1, int &x2, int &y2 ) const{ x1 = Get(BRUSH_X); x2 = x1 + Get(BRUSH_W); y1 = Get(BRUSH_Y); y2 = y1 + Get(BRUSH_H); }
	void MakeBBW	( iV2 & p1, iV2 & p2 ) const { p1 = pos(); p2 = p1 + size(); }

	float mapScale() const { return Get(BRUSH_SCALE) > 0 ? Get(BRUSH_SCALE) / 100.0f : 1.0f; }		
	float mapWith() const   { return ( (Get(BRUSH_FLIP) & R9_FLIPR) ? (Get(BRUSH_MAP+3) - Get(BRUSH_MAP+1)) : (Get(BRUSH_MAP+2) - Get(BRUSH_MAP+0)) ) * mapScale(); }
	float mapHeight() const { return ( (Get(BRUSH_FLIP) & R9_FLIPR) ? (Get(BRUSH_MAP+2) - Get(BRUSH_MAP+0)) : (Get(BRUSH_MAP+3) - Get(BRUSH_MAP+1)) ) * mapScale(); }
	static bool InvalidProp(int idx) { return idx < 0 || idx >= BRUSH_MAX; }	

	iV2 pos() const { return iV2(Get(BRUSH_X), Get(BRUSH_Y)); }
	iV2 size() const { return iV2(Get(BRUSH_W), Get(BRUSH_H)); }
	iRect rect() const { iV2 p = pos(); return iRect(p, p + size()); }
	iRect map() const { return iRect(Get(BRUSH_MAP+0), Get(BRUSH_MAP+1), Get(BRUSH_MAP+2), Get(BRUSH_MAP+3)); }
	fV2 mapSize() const { fV2 sz = map().Size(); return ((Get(BRUSH_FLIP) & R9_FLIPR) ? fV2(sz.y, sz.x) : sz) * mapScale(); }

	Blend shader() const { return _shader; }
	void shader(Blend s) { _shader = s; }
};

//////////////////////////////////////////////////////////////////////////////////////////////////
// DIZPAINT
//////////////////////////////////////////////////////////////////////////////////////////////////

class Tiles : std::vector<cTile>
{
	IntIndex Index;
	typedef std::vector<cTile> Cont;
	bool LoadFile(const char* filepath, size_t & total, size_t & fail, size_t & duplicates, int group);	// load a tile file

public:
	using Cont::size;

	void clear() {  
		Index.clear(); 
		Cont::clear(); 
	}
	void erase(iterator i) { 
		Index.erase(i->id);
		Cont::erase(i); 
	}

	bool InvalidIdx(int idx) const {return idx < 0 && static_cast<size_type>(idx) >= size(); }

	cTile * Get(int idx) { return InvalidIdx(idx) ? nullptr : &(*this)[idx]; }
	const cTile * Get(int idx) const { return InvalidIdx(idx) ? nullptr : &(*this)[idx]; }
	int Add(int id);								// add a new empty tile; id must be unique
	void Del(int idx);								// delete tile by index
	int Find(int id) const { auto i = Index.find(id); return i != Index.end() ? i->second : -1; }
	bool Load(char* path, int group = 0);			// load tiles from a path, and set the specified group
	void Unload(int group=0 );						// unload load tiles (destroy) from a specified group

	bool Reacquire(); // called before render reset to reload render resources
	void Unacquire(); // called before render reset to free render resources
};

class Fonts : std::vector<cFont>
{
	typedef std::vector<cFont> Cont;
public:
	using Cont::clear;

	bool InvalidIdx(int idx) const {return idx < 0 && static_cast<size_type>(idx) >= size(); }
	cFont * Get(int idx) { return InvalidIdx(idx)? nullptr : &(*this)[idx]; }
	const cFont * Get(int idx) const { return InvalidIdx(idx)? nullptr : &(*this)[idx]; }
	void Del(int idx) {	if(!InvalidIdx(idx)) erase(begin() + idx); }
	bool LoadFile(const char* filepath, int group = 0);
	int	Find(int id) const { for(size_type i=0;i<size(); i++) if((*this)[i].id==id) return i; return -1; }
	bool Load(char * path, int group = 0);	// load fonts from a path and set the specified group
	void Unload(int group = 0);				// unload fonts (destroy) from the specified group

	void Unacquire() { std::for_each(begin(), end(), [](cFont &f) { if(f.font) f.font->SetTexture(nullptr); }); }

};

class HUD
{
	enum Cmd { None, Align, Color, Focus, Tile };
	int _font;		// current font id
	Blend _shader;	// current hud shader
	dword _color;	// current hud color
	bool _isDrawing;		// draw allowed
	Cmd ScanText(std::string::const_iterator start, std::string::const_iterator end, std::string::const_iterator & res, int* data);								// helper for hud text; scans for command and return command and data info

public:
	HUD() : _font(), _shader(Blend::Alpha), _color(0xffffffff), _isDrawing() {}
	void SetClipping(const iRect & dst);														// set a clipping rect
	void DrawTile(int tileid, const iRect & dst, const iRect & src, dword flags, int frame);	// draw tile
	void DrawText(int tileid, const iRect & dst, const std::string & text, int align);						// draw text with escape commands
	void GetTextSize(const std::string & text, int& w, int& h, int&c, int&r);								// in text's width and height in pixels and the number of columns and rows

	void font(int f) { _font = f; }
	void shader(Blend b) { _shader = b; }
	void color(dword c) { _color = c; }
	void draw(bool d) { _isDrawing = d; }

	int font() const { return _font; }
	Blend shader() const { return _shader; }
	dword color() const { return _color; }
	bool isDrawing() const { return _isDrawing; }
};

class cDizPaint
{
	r9Img _imgtarget;	// target image in PF_A8 format (pointing to material map data)
	bool _drawtilesoft;	// true for DrawBrush to call DrawTileSoft
	byte _drawtilemat;	// material to draw the tile
	int _scale;		// scale factor
	iV2 _scrOffs;			// screen offset

	struct
	{
		int scale;
		iV2 offs;
		fRect clip;
	} rollback;

	void DrawTileSoft(int idx, const iV2 & p, const iRect & map, dword color=0xffffffff, int flip=0, int frame=0, Blend blend = Blend::Alpha, float scale=1.0f ) const;	// paints tile in the image target map (faster, no rotation, no scale)
	void DrawTileSoft2(int idx, const iV2 & p, const iRect & map, dword color=0xffffffff, int flip=0, int frame=0, Blend blend = Blend::Alpha, float scale=1.0f ) const;	// paints tile in the image target map (accept rotation and scale)

	std::function<void(const iV2&)> selectDrawMethod(const tBrush & brush, int idx, int frame) const;

public:
	cDizPaint() : _scale(1), _drawtilesoft(), _drawtilemat() {}
	
	bool Init()	{ Layout(); return true; }
	void Done() { tiles.clear(); fonts.clear(); }
	void Layout();								// compute layout position (scale,scrx,scry)
		
	bool Reacquire() { return tiles.Reacquire(); }	// called before render reset to reload render resources
	void Unacquire();								// called before render reset to free render resources

	// Draw scaled
	void DrawTile(int idx, const iV2 & p, const iRect & map, dword color=0xffffffff, int flip=0, int frame=0, Blend blend = Blend::Alpha, float scale=1.0f ) const;	// tile scale (in editor paint it was full scale)
	void DrawTile(int idx, const iV2 & p, dword color=0xffffffff, int flip=0, int frame=0, Blend blend = Blend::Alpha, float scale=1.0f) const;				// tile scale (in editor paint it was full scale)
	void DrawChar(int fontidx, const iV2 & p, char c, dword color=0xffffffff ) const;

	// Draw brush 
	void DrawBrush( const tBrush & brush, const iV2 & p, int frame=-1 ) const; // if frame is -1, tile is automatic animated

	void BeginSoftwareRendering(const iV2 & size, dword cap, byte * data);
	void EndSoftwareRendering();


	// screen props
	iV2 scrPos(const iV2 & p) const { return scrOffs() + p * scale(); }
	int scale() const { return _scale; }
	void scale(int s) { _scale = s; }

	const iV2 & scrOffs() const { return _scrOffs; }
	void scrOffs(const iV2 & v) { _scrOffs = v; }

	void drawtilemat(int mat) { _drawtilemat = mat; }
	int drawtilemat() const { return _drawtilemat; }

	bool drawtilesoft() const { return _drawtilesoft; }

	Tiles tiles;
	Fonts fonts;
	HUD hud;
};

extern	cDizPaint	g_paint;

#endif
//////////////////////////////////////////////////////////////////////////////////////////////////
