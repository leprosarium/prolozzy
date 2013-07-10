//////////////////////////////////////////////////////////////////////////////////////////////////
// DizPaint.h
//////////////////////////////////////////////////////////////////////////////////////////////////
#ifndef __DIZPAINT_H__
#define __DIZPAINT_H__

#include "R9Font.h"
#include "DizDef.h"
#include "Brush.h"

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
		int			GetWidth() const			{ return fx > 0 ? tex->width / fx : tex->width; }
		int			GetHeight()	const			{ return fy > 0 ? tex->height / fy : tex->height; }
		iV2			GetSize() const { return iV2(GetWidth(), GetHeight()); }
		iV2			TexSize() const { return tex->size(); }
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
	r9Font * font;				// font
		
	cFont() : id(), group(), font() {}
	cFont(int id, int group, r9Font * font) : id(id), group(group), font(font) {}
	~cFont() { delete font; }
	cFont(cFont && f) : id(f.id), group(f.group), font(f.font) { f.font = 0; }
	cFont & operator = (cFont && f) { id = f.id; group = f.group; font = f.font; f.font = 0; return *this; }

	int GetSize() const { return static_cast<int>(font->GetSize()); }
	int GetCharWidth( char c ) const { return static_cast<int>(font->GetCharWidth(c)); }
	int GetTextWidth( const std::string & text ) const { return static_cast<int>(font->GetTextWidth(text)); }
	int	GetOfsX() const { return (int)font->GetOfsX(); }
	int GetOfsY() const { return (int)font->GetOfsY(); }
};

//////////////////////////////////////////////////////////////////////////////////////////////////
// DIZPAINT
//////////////////////////////////////////////////////////////////////////////////////////////////

template<class T, class Key = int>
class Indexed : std::vector<T>
{
public:
	typedef std::vector<T> Cont;
	typedef std::unordered_map<Key, int> IndexType;
	IndexType Index;
	using Cont::size;
	using Cont::begin;
	using Cont::end;
	using Cont::empty;
	using Cont::front;

	Indexed() { reserve(2000); }
	bool InvalidIdx(int idx) const {return idx < 0 && static_cast<size_type>(idx) >= size(); }
	T & get(int idx) { return (*this)[idx]; }
	const T & get(int idx) const{ return (*this)[idx]; }
	T * Get(int idx) { return InvalidIdx(idx) ? nullptr : & get(idx); }
	const T * Get(int idx) const { return InvalidIdx(idx) ? nullptr : & get(idx); }
	int	New() { push_back(T()); return size() - 1; }

	void clear() { Index.clear(); Cont::clear(); }
	void erase(iterator i) { Index.erase(i->id); Cont::erase(i); }
	
	int Find(const Key & id) const { auto i = Index.find(id); return i != Index.end() ? i->second : -1; }
	int Add(const Key & id, T && t)
	{
		if(Find(id)!=-1) return -1; // duplicate id
		push_back(std::move(t));
		int idx = size() - 1;

		Index.insert(IndexType::value_type(id, idx));
		return idx;
	}
	void Erase( int idx )
	{
		if(!InvalidIdx(idx))
			erase(begin() + idx);
	}
	void Reindex()
	{
		Index.clear();
		for(size_t i = 0, e = size(); i != e; ++i) {
			T & b = get(i);
			if(!b.id.empty())
				Index.insert(IndexType::value_type(b.id, i));
		}
	}
};

template<class T, class Key>
class Indexed<T*, Key> : std::vector<T*>
{
public:
	typedef std::vector<T*> Cont;
	typedef std::unordered_map<Key, int> IndexType;
	IndexType Index;
	using Cont::size;
	using Cont::begin;
	using Cont::end;
	using Cont::empty;
	using Cont::front;

	~Indexed() { clear(); }

	bool InvalidIdx(int idx) const {return idx < 0 && static_cast<size_type>(idx) >= size(); }
	T * get(int idx) { return (*this)[idx]; }
	const T * get(int idx) const{ return (*this)[idx]; }
	T * Get(int idx) { return InvalidIdx(idx) ? nullptr : get(idx); }
	const T * Get(int idx) const { return InvalidIdx(idx) ? nullptr : get(idx); }
	int	New() { push_back(new T()); return size() - 1; }

	void clear() { for(auto a: *this) delete a; Index.clear(); Cont::clear(); }
	void erase(iterator i) { Index.erase(i->id); delete *i; Cont::erase(i); }
	
	int Find(const Key & id) const { auto i = Index.find(id); return i != Index.end() ? i->second : -1; }
	int Add(const Key & id, T * t)
	{
		if(Find(id)!=-1) return -1; // duplicate id
		push_back(t);
		int idx = size() - 1;

		Index.insert(IndexType::value_type(id, idx));
		return idx;
	}
	void Erase( int idx )
	{
		if(!InvalidIdx(idx))
		{
			auto it = begin() + idx;
			delete *it;
			erase(it);
		}
	}
	void Reindex()
	{
		Index.clear();
		for(size_t i = 0, e = size(); i != e; ++i) {
			auto id = get(i)->id;
			if(!id.empty())
				Index.insert(IndexType::value_type(id, i));
		}
	}
};



class Tiles : public Indexed<cTile>
{
	bool LoadFile(const std::string & file, size_t & total, size_t & fail, size_t & duplicates, int group);	// load a tile file
public:
	bool Load(const std::string & path, int group = 0); 	// load tiles from a path, and set the specified group
	void Unload(int group=0 );						// unload load tiles (destroy) from a specified group
	bool Reacquire(); // called before render reset to reload render resources
	void Unacquire(); // called before render reset to free render resources
};

class Fonts : public Indexed<cFont>
{
	bool LoadFile(const std::string & filepath, size_t & total, size_t & fail, size_t & duplicates, int group = 0);
public:
	bool Load(const std::string & path, int group = 0);	// load fonts from a path and set the specified group
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
