//////////////////////////////////////////////////////////////////////////////////////////////////
// DizPaint.h
//////////////////////////////////////////////////////////////////////////////////////////////////
#ifndef __DIZPAINT_H__
#define __DIZPAINT_H__

#include "R9Font.h"
#include "DizDef.h"
#include "Brush.h"

#include <deque>
#include <unordered_map>
#include <algorithm>

//////////////////////////////////////////////////////////////////////////////////////////////////
// TILE
//////////////////////////////////////////////////////////////////////////////////////////////////


class Tile
{
	Tile(const Tile &);
	Tile & operator= (const Tile &);
public:
		int			id;						// unique id >=0
		int			frames;					// number of frames
		int         fx;
		int			fy;
		R9TEXTURE	tex;						// texture
		r9Img		img;						// img alpha mask
		std::string	name;						// tile name
		
		Tile(int id) : id(id), frames(1), fx(1), fy(1), tex(nullptr) { }
		Tile(Tile && t) : 
			id(t.id),
			frames(t.frames),
			fx(t.fx),
			fy(t.fy),
			tex(t.tex),
			img(std::move(t.img)),
			name(std::move(t.name))
		{
			t.tex = 0; 
		}

		Tile & operator = (Tile && t) 
		{
			id = t.id;
			frames = t.frames;
			fx = t.fx;
			fy = t.fy;
			tex = t.tex;
			name = std::move(t.name);
			img = std::move(t.img);
			t.tex = 0;
			return *this;
		}
		~Tile() { Destroy();  R9_ImgDestroy(&img); }
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
class Font
{
	Font(const Font &);
	Font & operator = (const Font &);
public:
	int id;						// unique id >=0
	r9Font * font;				// font
		
	Font() : id(), font() {}
	Font(int id, r9Font * font) : id(id), font(font) {}
	~Font() { delete font; }
	Font(Font && f) : id(f.id), font(f.font) { f.font = nullptr; }
	Font & operator = (Font && f) { id = f.id; font = f.font; f.font = nullptr; return *this; }

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



class Tiles : public Indexed<Tile>
{
	bool LoadFile(const std::string & file, size_t & total, size_t & fail, size_t & duplicates);
public:
	bool Load(const std::string & path); 
	bool Reacquire(); // called before render reset to reload render resources
	void Unacquire(); // called before render reset to free render resources
};

class Fonts : public Indexed<Font>
{
	bool LoadFile(const std::string & filepath, size_t & total, size_t & fail, size_t & duplicates);
public:
	bool Load(const std::string & path);
	void Unacquire() { std::for_each(begin(), end(), [](Font &f) { if(f.font) f.font->SetTexture(nullptr); }); }

};

class HUD
{
	enum class Cmd { None, Align, Color, Focus, Tile };
	Cmd ScanText(std::string::const_iterator start, std::string::const_iterator end, std::string::const_iterator & res, int* data);								// helper for hud text; scans for command and return command and data info

public:
	int font;		// current font id
	Blend shader;	// current shader
	dword color;	// current color
	bool visible;	// draw allowed

	HUD() : font(), shader(Blend::Alpha), color(0xffffffff), visible() {}
	void SetClipping(const iRect & dst);														// set a clipping rect
	void DrawTile(int tileid, const iRect & dst, const iRect & src, dword flags, int frame);	// draw tile
	void DrawText(int tileid, const iRect & dst, const std::string & text, int align);						// draw text with escape commands
	void GetTextSize(const std::string & text, int& w, int& h, int&c, int&r);								// in text's width and height in pixels and the number of columns and rows
};

class DizPaint
{
	r9Img _imgtarget;	// target image in PF_A8 format (pointing to material map data)
	bool _drawtilesoft;	// true for DrawBrush to call DrawTileSoft

	struct
	{
		int scale;
		iV2 offs;
		fRect clip;
	} rollback;

	void DrawTileSoft(int idx, const iV2 & p, const iRect & map, dword color=0xffffffff, int flip=0, int frame=0, Blend blend = Blend::Alpha, float scale=1.0f ) const;	// paints tile in the image target map (faster, no rotation, no scale)
	void DrawTileSoft2(int idx, const iV2 & p, const iRect & map, dword color=0xffffffff, int flip=0, int frame=0, Blend blend = Blend::Alpha, float scale=1.0f ) const;	// paints tile in the image target map (accept rotation and scale)

	std::function<void(const iV2&)> selectDrawMethod(const Brush & brush, int idx, int frame) const;

public:
	int scale;			// scale factor
	iV2 scrOffs;		// screen offset
	byte drawtilemat;	// material to draw the tile

	DizPaint() : scale(1), _drawtilesoft(), drawtilemat() {}
	
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
	void DrawBrush( const Brush & brush, const iV2 & p, int frame=-1 ) const; // if frame is -1, tile is automatic animated

	void BeginSoftwareRendering(const iV2 & size, dword cap, byte * data);
	void EndSoftwareRendering();


	// screen props
	iV2 scrPos(const iV2 & p) const { return scrOffs + p * scale; }

	bool drawtilesoft() const { return _drawtilesoft; }

	Tiles tiles;
	Fonts fonts;
	HUD hud;
};

extern	DizPaint	g_paint;

#endif
//////////////////////////////////////////////////////////////////////////////////////////////////
