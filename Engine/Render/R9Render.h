///////////////////////////////////////////////////////////////////////////////////////////////////
// R9Render.h
// Render
///////////////////////////////////////////////////////////////////////////////////////////////////
#ifndef __R9RENDER_H__
#define __R9RENDER_H__

#include <vector>
#include <assert.h>

#include "E9System.h"
#include "E9Engine.h"
#include "R9Img.h"
#include "E9Math.h"
#include "D9Log.h"


///////////////////////////////////////////////////////////////////////////////////////////////////
enum class Api
{
	DirectX,
	OpenGL,
	Default = DirectX

};

#define	R9_STATES				5		// dummy

enum class Primitive
{
	Line,		// line primitive with 2 vertexes
	Triangle	// triangle primitive with 3 vertexes
};

enum class Blend
{
	Min = 0,
	Opaque = Min,	// no alpha
	Alpha,			// alpha blend
	Add,			// additive
	Mod,			// modulative
	Mod2,			// decal
	AlphaRep,		// alpha replicate
	Max
};

enum class TAddress
{
	Wrap,		// repeat texture mapping
	Clamp		// clamp texture mapping
};

enum class Filter
{
	Point,
	Linear
};

const int CfgWidth = 640;		// default width
const int CfgHeight = 480;		// default height
const int CfgBPP = 32;			// default bpp

const int ChrW = 6;		// system font chr width
const int ChrH = 9;		// system font chr height

enum class Flip
{
	X = 1,
	Y = 2,
	R = 4,
	XY = X | Y,
	XR = X | R,
	YR = Y | R,
	XYR = XY | R
};

template<Flip F, class T>
inline bool Is(T v) { return (v & static_cast<T>(F)) != T(); }

///////////////////////////////////////////////////////////////////////////////////////////////////
struct r9Vertex
{
	float x,y;
	float u,v;
	dword color;
};

struct r9Cfg
{
	int windowed;		// windowed
	int bpp;			// bpp 16/32 (ignored in windowed)
	int width;		// resolution width
	int height;		// resolution height
	int refresh;		// refresh rate (0=default);
	int vsync;			// vsync (0=off)
	r9Cfg();				
};

struct r9DisplayMode
{
	int		windowed;		// windowed 1/0 (1 for current display mode)
	int		bpp;			// bpp 16/32
	int		width;		// resolution width
	int		height;		// resolution height
	int		refresh;		// refresh rate (0 for current display mode)
	dword	reserved1;	// reserved for platforms
	bool operator <(const r9DisplayMode & m) const
	{
		if(windowed != m.windowed )	return windowed < m.windowed;
		if(bpp != m.bpp ) return bpp < m.bpp;
		if(width != m.width) return width < m.width;
		if(height != m.height) return height < m.height;
		return refresh < m.refresh;
	}
	void log(int ch) const { dlog(ch, L"   \t%ix%i \t%ibpp \t%iHz \t%S\n", width, height, bpp, refresh, windowed ? "windowed" : ""); }
};

struct r9Texture
{
	int		width;		// requested width
	int		height;		// requested height
	int		realwidth;	// real created width (may be greater than requested, pow2 etc)
	int		realheight;	// real created height (may be greater than requested, pow2 etc)
	void*	handler;	// platform handler
	void*	handlerex;	// platform handler ex

	bool IsValid() const { return handler != nullptr; }
	iV2 size() const { return iV2(width, height); }
	iV2 realSize() const { return iV2(realwidth, realheight); }

};
typedef r9Texture* R9TEXTURE;

typedef void (*r9HandleReset)();	// HandleReset is called after the device was successfully restored, so user can repaint content of render targets (targets and states are managed by render)

class r9Font;

///////////////////////////////////////////////////////////////////////////////////////////////////
class r9Render
{
protected:
	virtual void ApplyTexture() = 0;
	virtual	void ApplyViewport() = 0;
	virtual void ApplyView() = 0;
	virtual void ApplyBlend() = 0;
	virtual void ApplyTAddress() = 0;
	virtual void ApplyFilter() = 0;
	virtual	bool Init() = 0;							
	virtual	void Finish() = 0;							
	virtual	R9TEXTURE TextureCreateImg(r9Img* img) = 0;
	virtual	void ResetDefaultStates() = 0;
	virtual	void DoClear(dword color) = 0;
	virtual	bool DoBeginScene(R9TEXTURE target) = 0;
	virtual	void DoEndScene() = 0;
	virtual void DoPresent() = 0;
	virtual	bool DoTakeScreenShot( r9Img* img, fRect* rect , bool full) = 0;
	virtual bool CopyTargetToImage( R9TEXTURE target, r9Img* img, const iV2 &p, const iV2 & sz) = 0;
public:
	static std::vector<r9DisplayMode> DisplayModes;

	r9Render(Api api);
	virtual ~r9Render() {}

	bool Init(HWND hwnd, r9Cfg * cfg);					// init render; if cfg is NULL, default cfg is used
	void Done();										
	int GetWidth() const { return m_cfg.width; }
	int GetHeight() const { return m_cfg.height; }
	r9Cfg&		GetCfg()										{ return m_cfg; }
	Api			GetApi()										{ return api; }
	void SetDefaultStates();								// set states to default values
	bool NeedFlush() const { return m_needflush; }

	virtual	bool LoadDll() = 0;
	virtual	void UnloadDll() = 0;
	virtual	void GatherDisplayModes() const = 0;			// fill list with valid displaymodes and return count; use NULL just for the count; first entry is the current mode (windowed) if available; @WARNING: only safe to call in windowed mode, at start
	virtual	bool IsReady() = 0;								// if render is ready; avoid using render in window messages, before the device is ready

// batch primitives
	virtual void Push( r9Vertex* vx, int vxs, Primitive primitive ) = 0;// push vertices in the batch buffer
	virtual	void Flush() = 0;										// flush the batch buffer


	// texture
	R9TEXTURE TextureCreate(r9Img* img);					// create texture from image
	R9TEXTURE TextureLoad(const char* filename);			// load a texture from file
	virtual	R9TEXTURE TextureCreateTarget(int width, int height) = 0;	// create a texture with render target support
	virtual	void TextureDestroy(R9TEXTURE tex) = 0;				// destroy texture

// states
	void SetTexture(R9TEXTURE tex);							// set current texture (if different); flushes if needed
	R9TEXTURE GetTexture() const { return m_texture; }

	void SetViewport(const fRect & rect );					// set viewport rect (used as scissor)
	const fRect & GetViewport() const { return m_viewport; }

	void SetView(const iV2 & offs, dword flip);				// set view options

	void SetBlend(Blend b);
	Blend GetBlend() const { return blend; }

	void SetPrimitive(Primitive p);
	Primitive GetPrimitive() const { return primitive; }

	void SetTAddress(TAddress a);
	TAddress GetTAddress() const { return taddress; }

	void SetFilter(Filter f);
	Filter GetFilter() const { return filter; }



// flow
	void Clear(dword color)  { if(m_beginendscene) DoClear(color); }// clear backbuffer
	bool BeginScene(R9TEXTURE target = nullptr);					// begine scene drawing; if target is valid then render in texture target
	void EndScene();												// end scene drawing
	void Present() { DoPresent(); }										// present scene (flip buffers)
	bool IsBeginEndScene() const { return m_beginendscene; }
	virtual	bool CheckDevice() = 0;									// check if device is lost and if so, try to reset it
	virtual	bool ToggleVideoMode() = 0;								// @OBSOLETE toggle between windowed and full screen


// draw functions
	void DrawLine(const fV2 & a, const fV2 & b, dword color = 0xffffffff);
	void DrawTriangle(const fV2 & a, const fV2 & b, const fV2 & c, const fV2 & ta, const fV2 & tb, const fV2 & tc, R9TEXTURE tex, dword color=0xffffffff );
	void DrawBar(const fRect & dst, dword color=0xffffffff ) { DrawQuad(dst,fRect(0.0f,0.0f,1.0f,1.0f), nullptr, color); }
	void DrawQuad( const fRect& dst, const fRect& src, R9TEXTURE tex, dword color=0xffffffff );
	void DrawQuadRot(const fV2 & pos, const fV2 & size, const fV2 & center, float angle, const fRect & src, R9TEXTURE tex, dword color=0xffffffff ); // center is relative to the middle of the rectangle
	void DrawSprite( const fV2 & pos, const fRect & src, R9TEXTURE tex, dword color=0xffffffff, dword flip=0, float scale=1.0f ); // does clipping

// clipping
	bool IsClipping() const { return m_cliprect.Ordered(); }
	void SetClipping(const fRect & rect) { m_cliprect = rect; }
	const fRect & GetClipping() const { return m_cliprect; }
	void ClipBar( fRect& dst );								// clip destination rect (dst must be ordered)
	void ClipQuad(fRect & dst, fRect & src );				// clip destination rect and source mapping rect (dst must be ordered, src coordinates can be flipped)
	void ClipSprite( fRect& dst, fRect& src, int flip=0 );	// clip destination rect and source mapping rect (dst and src must have the same sizes; src coordinates can't be flipped); rotation not supported

// screen shot
	bool SaveScreenShot( fRect* rect=nullptr, bool full = true);					// auto save screenshot
	bool TakeScreenShot( r9Img* img, fRect* rect = nullptr, bool full = true);		// shoots full screen or backbuffer (create img)

	bool CopyTargetToImage( R9TEXTURE target, r9Img* img, fRect* rect );	// copy the content of a texture target from (0,0) into an image at a specified rect

// font
	bool MakeFont();									// creates render debug font, from source resources; call at the end of platform Init, since it requires a texture to be created

// members
		HMODULE			m_dll;				// platform dll
		HWND			m_hwnd;				// associated window
		Api				api;				// render api (fixed per render class)
		r9Cfg			m_cfg;				// render config
		bool			m_beginendscene;	// if inside begin-end scene cycle
		R9TEXTURE		m_texture;			// current texture
		fRect			m_viewport;			// viewport rect
		iV2				viewOffs;			// view offset
		dword			m_viewflip;			// view flip option
		fRect			m_cliprect;			// clipping rect (clipping is performed if rect is valid)
		bool			m_needflush;		// need flush, batch buffer not empty
		int				m_primitivecount;	// rendered primitives from the scene
		r9Font*			m_font;				// render font (created from source resource)
		r9HandleReset	m_handlereset;		// on reset user callback

private:
	Blend blend;
	Primitive primitive;
	TAddress taddress;
	Filter filter;
};


///////////////////////////////////////////////////////////////////////////////////////////////////
// INLINES
///////////////////////////////////////////////////////////////////////////////////////////////////
inline void r9Render::DrawLine(const fV2 & a, const fV2 & b, dword color)
{
	SetTexture(nullptr);
	r9Vertex vx[2] = {{a.x, a.y, 0.0f, 0.0f, color}, {b.x, b.y, 1.0f, 1.0f, color}};
	Push(vx, 2, Primitive::Line);
}

inline void r9Render::DrawTriangle(const fV2 & a, const fV2 & b, const fV2 & c, const fV2 & ta, const fV2 & tb, const fV2 & tc, R9TEXTURE tex, dword color )
{
	SetTexture(tex);
	r9Vertex vx[3] = {{a.x, a.y, ta.x, ta.y, color}, { b.x, b.y, tb.x, tb.y, color}, {c.x, c.y, tc.x, tc.y, color}};
	Push(vx, 3, Primitive::Triangle);
}

inline void r9Render::DrawQuad( const fRect & dst, const fRect & src, R9TEXTURE tex, dword color )
{
	SetTexture(tex);

	r9Vertex vx[6] = {
		{ dst.p1.x, dst.p1.y, src.p1.x, src.p1.y, color },
		{ dst.p2.x, dst.p1.y, src.p2.x, src.p1.y, color },
		{ dst.p1.x, dst.p2.y, src.p1.x, src.p2.y, color },
		{ dst.p2.x, dst.p1.y, src.p2.x, src.p1.y, color },
		{ dst.p2.x, dst.p2.y, src.p2.x, src.p2.y, color },
		{ dst.p1.x, dst.p2.y, src.p1.x, src.p2.y, color}};
	Push(vx, 6, Primitive::Triangle);
}

inline void r9Render::ClipBar( fRect& dst )
{
	if(!IsClipping()) return;
	
	if(dst.p1.x<m_cliprect.p1.x) dst.p1.x=m_cliprect.p1.x;	else
	if(dst.p1.x>m_cliprect.p2.x) dst.p1.x=m_cliprect.p2.x;
	
	if(dst.p1.y<m_cliprect.p1.y) dst.p1.y=m_cliprect.p1.y;	else
	if(dst.p1.y>m_cliprect.p2.y) dst.p1.y=m_cliprect.p2.y;
	
	if(dst.p2.x>m_cliprect.p2.x) dst.p2.x=m_cliprect.p2.x;	else
	if(dst.p2.x<m_cliprect.p1.x) dst.p2.x=m_cliprect.p1.x;
	
	if(dst.p2.y>m_cliprect.p2.y) dst.p2.y=m_cliprect.p2.y;	else
	if(dst.p2.y<m_cliprect.p1.y) dst.p2.y=m_cliprect.p1.y;

}

inline void r9Render::ClipQuad( fRect & dst, fRect & src )
{
	if(!IsClipping()) return;
	
	float f;
	fRect dst0 = dst;
	fV2 ssz = src.Size();
	fV2 dsz = dst.Size();

	dst.Clip(m_cliprect);

	if(dsz.x>0.0f) {
		f = ssz.x / dsz.x;
		src.p1.x += f * (dst.p1.x - dst0.p1.x);
		src.p2.x += f * (dst.p2.x - dst0.p2.x);
	}
	if(dsz.y>0.0f) {
		f = ssz.y / dsz.y;
		src.p1.y += f * (dst.p1.y - dst0.p1.y);
		src.p2.y += f * (dst.p2.y - dst0.p2.y);
	}

}

inline void r9Render::ClipSprite( fRect& dst, fRect& src, int flip )
{
	if(!IsClipping()) return;
	if(flip & 1)
	{
		if(dst.p1.x<m_cliprect.p1.x) { src.p2.x-=(m_cliprect.p1.x-dst.p1.x); dst.p1.x=m_cliprect.p1.x; }
		if(dst.p2.x>m_cliprect.p2.x) { src.p1.x+=(dst.p2.x-m_cliprect.p2.x); dst.p2.x=m_cliprect.p2.x; }
	}
	else
	{
		if(dst.p1.x<m_cliprect.p1.x) { src.p1.x+=(m_cliprect.p1.x-dst.p1.x); dst.p1.x=m_cliprect.p1.x; }
		if(dst.p2.x>m_cliprect.p2.x) { src.p2.x-=(dst.p2.x-m_cliprect.p2.x); dst.p2.x=m_cliprect.p2.x; }
	}
	if(flip & 2)
	{
		if(dst.p1.y<m_cliprect.p1.y) { src.p2.y-=(m_cliprect.p1.y-dst.p1.y); dst.p1.y=m_cliprect.p1.y; }
		if(dst.p2.y>m_cliprect.p2.y) { src.p1.y+=(dst.p2.y-m_cliprect.p2.y); dst.p2.y=m_cliprect.p2.y; }
	}
	else
	{
		if(dst.p1.y<m_cliprect.p1.y) { src.p1.y+=(m_cliprect.p1.y-dst.p1.y); dst.p1.y=m_cliprect.p1.y; }
		if(dst.p2.y>m_cliprect.p2.y) { src.p2.y-=(dst.p2.y-m_cliprect.p2.y); dst.p2.y=m_cliprect.p2.y; }
	}
}

///////////////////////////////////////////////////////////////////////////////////////////////////
// INTERFACE
///////////////////////////////////////////////////////////////////////////////////////////////////
extern r9Render* r9_render;
		
bool R9_InitInterface(Api api = Api::Default);
		void		R9_GetDisplayModes( r9DisplayMode* &displaymode, int& displaymodecount );		// available after init interface
bool R9_FilterCfg(r9Cfg & cfg, Api & api);														// available after init interface
		void		R9_LogCfg( r9Cfg& cfg, Api api );
		void		R9_DoneInterface();

		BOOL		R9_Init( HWND hwnd, r9Cfg* cfg, Api api = Api::Default);
		void		R9_Done();
inline	BOOL		R9_IsReady()											{ return (r9_render && r9_render->IsReady()); }
inline	int			R9_GetWidth()											{ assert(r9_render); return r9_render->GetWidth(); }
inline	int			R9_GetHeight()											{ assert(r9_render); return r9_render->GetHeight(); }
inline	r9Cfg&		R9_GetCfg()												{ assert(r9_render); return r9_render->GetCfg(); }
inline	Api			R9_GetApi()												{ assert(r9_render); return r9_render->GetApi(); }
inline	void		R9_SetHandleReset( r9HandleReset callback )				{ assert(r9_render); r9_render->m_handlereset = callback; }

inline	R9TEXTURE	R9_TextureCreate( r9Img* img )							{ assert(r9_render); return r9_render->TextureCreate(img); }
inline	R9TEXTURE	R9_TextureLoad( const char* filename )					{ assert(r9_render); return r9_render->TextureLoad(filename); }
inline	R9TEXTURE	R9_TextureCreateTarget( int width, int height )			{ assert(r9_render); return r9_render->TextureCreateTarget(width,height); }
inline	void		R9_TextureDestroy( R9TEXTURE tex )						{ assert(r9_render); r9_render->TextureDestroy(tex); }

inline	void		R9_SetTexture( R9TEXTURE tex )							{ assert(r9_render); r9_render->SetTexture(tex); }
inline	R9TEXTURE	R9_GetTexture()											{ assert(r9_render); return r9_render->GetTexture(); }
inline	void		R9_SetViewport( fRect& rect )							{ assert(r9_render); r9_render->SetViewport(rect); }
inline	const fRect& R9_GetViewport()										{ assert(r9_render); return r9_render->GetViewport(); }
inline	void		R9_SetView(const iV2 & offs, dword flip )				{ assert(r9_render); r9_render->SetView(offs,flip); }
inline	void		R9_SetBlend(Blend b)						{ assert(r9_render); r9_render->SetBlend(b); }
inline	Blend		R9_GetBlend()								{ assert(r9_render); return r9_render->GetBlend(); }
inline	void		R9_SetFilter(Filter f)						{ assert(r9_render); r9_render->SetFilter(f); }



inline	void		R9_Clear( dword color )									{ assert(r9_render); r9_render->Clear(color); }
inline	BOOL		R9_BeginScene( R9TEXTURE target=NULL )					{ assert(r9_render); return r9_render->BeginScene( target ); }
inline	void		R9_EndScene()											{ assert(r9_render); r9_render->EndScene(); }
inline	void		R9_Present()											{ assert(r9_render); r9_render->Present(); }
inline	BOOL		R9_IsBeginEndScene()									{ assert(r9_render); return r9_render->IsBeginEndScene(); }
inline	BOOL		R9_CheckDevice()										{ assert(r9_render); return r9_render->CheckDevice(); }
inline	BOOL		R9_ToggleVideoMode()									{ assert(r9_render); return r9_render->ToggleVideoMode(); }

inline	void		R9_Push( r9Vertex* vx, int vxs, Primitive primitive )	{ assert(r9_render); r9_render->Push(vx, vxs, primitive); }
inline	void		R9_Flush()												{ assert(r9_render); r9_render->Flush(); }
inline	BOOL		R9_NeedFlush()											{ assert(r9_render); return r9_render->NeedFlush(); }

inline	void		R9_DrawLine( fV2& a, fV2& b, dword color=0xffffffff )	{ assert(r9_render); r9_render->DrawLine(a,b,color); }
inline	void		R9_DrawTriangle( fV2& a, fV2& b,fV2& c, fV2& ta, fV2& tb, fV2& tc, R9TEXTURE tex, dword color=0xffffffff ) {  assert(r9_render); r9_render->DrawTriangle(a,b,c,ta,tb,tc,tex,color); }
inline	void		R9_DrawBar( fRect& dst, dword color=0xffffffff ) {  assert(r9_render); r9_render->DrawBar(dst,color); }
inline	void		R9_DrawQuad( fRect& dst, fRect& src, R9TEXTURE tex, dword color=0xffffffff ) {  assert(r9_render); r9_render->DrawQuad(dst,src,tex,color); }
inline	void		R9_DrawQuadRot( fV2& pos, fV2& size, fV2& center, float angle, fRect& src, R9TEXTURE tex, dword color=0xffffffff ) {  assert(r9_render); r9_render->DrawQuadRot(pos,size,center,angle,src,tex,color); }
inline	void		R9_DrawSprite( const fV2 & pos, const fRect & src, R9TEXTURE tex, dword color=0xffffffff, dword flip=0, float scale=1.0f ) {  assert(r9_render); r9_render->DrawSprite(pos,src,tex,color,flip,scale); }
		void		R9_DrawText( fV2& pos, const char* text, dword color=0xffffffff, float scale=1.0f );

inline	BOOL		R9_IsClipping()											{ assert(r9_render); return r9_render->IsClipping(); }
inline	void		R9_SetClipping( fRect& rect )							{ assert(r9_render); r9_render->SetClipping(rect); }
inline	const fRect & R9_GetClipping()										{ assert(r9_render); return r9_render->GetClipping(); }
inline	void		R9_ResetClipping()										{ assert(r9_render); r9_render->SetClipping(fRect()); }
inline	void		R9_AddClipping( fRect& rect )							{ assert(r9_render); if(!r9_render->IsClipping()) r9_render->SetClipping(rect); else r9_render->SetClipping(r9_render->GetClipping() * rect); }
inline 	void		R9_ClipBar( fRect& dst )								{ assert(r9_render); r9_render->ClipBar(dst); }
inline 	void		R9_ClipQuad( fRect& dst, fRect& src )					{ assert(r9_render); r9_render->ClipQuad(dst,src); }
inline 	void		R9_ClipSprite( fRect& dst, fRect& src, int flip=0 )		{ assert(r9_render); r9_render->ClipSprite(dst,src,flip); }

inline	bool		R9_SaveScreenShot( fRect* rect = nullptr, bool full = true)				{ assert(r9_render); return r9_render->SaveScreenShot(rect,full); }
inline	bool		R9_TakeScreenShot( r9Img* img, fRect* rect = nullptr, bool full = true)	{ assert(r9_render); return r9_render->TakeScreenShot(img,rect,full); }
inline	bool		R9_CopyTargetToImage( R9TEXTURE target, r9Img* img, fRect* rect )	{ assert(r9_render); return r9_render->CopyTargetToImage(target,img,rect); }

#endif
///////////////////////////////////////////////////////////////////////////////////////////////////
