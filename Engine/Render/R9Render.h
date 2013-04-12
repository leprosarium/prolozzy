///////////////////////////////////////////////////////////////////////////////////////////////////
// R9Render.h
// Render
///////////////////////////////////////////////////////////////////////////////////////////////////
#ifndef __R9RENDER_H__
#define __R9RENDER_H__

#include <vector>


#include "E9System.h"
#include "E9Engine.h"
#include "D9Debug.h"
#include "R9Img.h"
#include "E9Math.h"


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

#define	R9_CFG_WIDTH			640		// default width
#define	R9_CFG_HEIGHT			480		// default height
#define	R9_CFG_BPP				32		// default bpp

#define	R9_CHRW					6		// system font chr width
#define	R9_CHRH					9		// system font chr height

#define	R9_FLIPX				1		// flip horizontal
#define	R9_FLIPY				2		// flip vertical
#define	R9_FLIPR				4		// flip rotation
#define	R9_FLIPXY				(R9_FLIPX|R9_FLIPY)
#define	R9_FLIPXR				(R9_FLIPX|R9_FLIPR)
#define	R9_FLIPYR				(R9_FLIPY|R9_FLIPR)
#define	R9_FLIPXYR				(R9_FLIPXY|R9_FLIPR)

///////////////////////////////////////////////////////////////////////////////////////////////////
struct r9Vertex
{
	float x,y;
	float u,v;
	dword color;
};

struct r9Cfg
{
	int		m_windowed;		// windowed 1/0
	int		m_bpp;			// bpp 16/32 (ignored in windowed)
	int		m_width;		// resolution width
	int		m_height;		// resolution height
	int		m_refresh;		// refresh rate (0=default);
	int		m_vsync;		// vsync (0=off)
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
	int		m_width;		// requested width
	int		m_height;		// requested height
	int		m_realwidth;	// real created width (may be greater than requested, pow2 etc)
	int		m_realheight;	// real created height (may be greater than requested, pow2 etc)
	void*	m_handler;		// platform handler
	void*	m_handlerex;	// platform handler ex
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
	virtual	R9TEXTURE TextureCreateImg(r9Img* img) = 0;		// create texture from image

public:
	static std::vector<r9DisplayMode> DisplayModes;

	r9Render(Api api);
	virtual ~r9Render() {}

	bool Init(HWND hwnd, r9Cfg * cfg);					// init render; if cfg is NULL, default cfg is used
	void Done();										
	int GetWidth() const { return m_cfg.m_width; }
	int GetHeight() const { return m_cfg.m_height; }
	r9Cfg&		GetCfg()										{ return m_cfg; }
	Api			GetApi()										{ return api; }

	virtual	bool LoadDll() = 0;
	virtual	void UnloadDll() = 0;
	virtual	void GatherDisplayModes() const = 0;			// fill list with valid displaymodes and return count; use NULL just for the count; first entry is the current mode (windowed) if available; @WARNING: only safe to call in windowed mode, at start
	virtual	bool IsReady() = 0;								// if render is ready; avoid using render in window messages, before the device is ready

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

	void SetView(int x, int y, dword flip);				// set view options
virtual	void		SetDefaultStates();								// set states to default values

	void SetBlend(Blend b);
	Blend GetBlend() const { return blend; }

	void SetPrimitive(Primitive p);
	Primitive GetPrimitive() const { return primitive; }

	void SetTAddress(TAddress a);
	TAddress GetTAddress() const { return taddress; }

	void SetFilter(Filter f);
	Filter GetFilter() const { return filter; }



// flow
virtual	void		Clear( dword color );							// clear backbuffer
virtual	BOOL		BeginScene( R9TEXTURE target=NULL );			// begine scene drawing; if target is valid then render in texture target
virtual	void		EndScene();										// end scene drawing
virtual	void		Present();										// present scene (flip buffers)
inline	BOOL		IsBeginEndScene()								{ return m_beginendscene; }
virtual	BOOL		CheckDevice();									// check if device is lost and if so, try to reset it
virtual	BOOL		ToggleVideoMode();								// @OBSOLETE toggle between windowed and full screen

// batch primitives
	virtual void Push( r9Vertex* vx, int vxs, Primitive primitive ) = 0;// push vertices in the batch buffer
	virtual	void Flush() = 0;										// flush the batch buffer
inline	BOOL		NeedFlush()										{ return m_needflush; }

// draw functions
inline	void		DrawLine( fV2& a, fV2& b, dword color=0xffffffff );
inline	void		DrawTriangle( fV2& a, fV2& b, fV2& c, fV2& ta, fV2& tb, fV2& tc, R9TEXTURE tex, dword color=0xffffffff );
inline	void		DrawBar( fRect& dst, dword color=0xffffffff );
inline	void		DrawQuad( fRect& dst, fRect& src, R9TEXTURE tex, dword color=0xffffffff );
		void		DrawQuadRot( fV2& pos, fV2& size, fV2& center, float angle, fRect& src, R9TEXTURE tex, dword color=0xffffffff ); // center is relative to the middle of the rectangle
		void		DrawSprite( const fV2 & pos, const fRect & src, R9TEXTURE tex, dword color=0xffffffff, dword flip=0, float scale=1.0f ); // does clipping

// clipping
inline	BOOL		IsClipping()										{ return ( (m_cliprect.x1<m_cliprect.x2) && (m_cliprect.y1<m_cliprect.y2) ); }
inline	void		SetClipping( fRect& rect )							{ m_cliprect = rect; }
inline	fRect&		GetClipping()										{ return m_cliprect; }
inline 	void		ClipBar( fRect& dst );								// clip destination rect (dst must be ordered)
inline 	void		ClipQuad(fRect & dst, fRect & src );				// clip destination rect and source mapping rect (dst must be ordered, src coordinates can be flipped)
inline 	void		ClipSprite( fRect& dst, fRect& src, int flip=0 );	// clip destination rect and source mapping rect (dst and src must have the same sizes; src coordinates can't be flipped); rotation not supported

// screen shot
virtual	BOOL		SaveScreenShot( fRect* rect=NULL, BOOL full=TRUE);					// auto save screenshot
virtual BOOL		TakeScreenShot( r9Img* img, fRect* rect=NULL, BOOL full=TRUE );		// shoots full screen or backbuffer (create img)
virtual BOOL		CopyTargetToImage( R9TEXTURE target, r9Img* img, fRect* rect );		// copy the content of a texture target from (0,0) into an image at a specified rect

// font
		BOOL		CreateFont();									// creates render debug font, from source resources; call at the end of platform Init, since it requires a texture to be created

// members
		HMODULE			m_dll;				// platform dll
		HWND			m_hwnd;				// associated window
		Api				api;				// render api (fixed per render class)
		r9Cfg			m_cfg;				// render config
		BOOL			m_beginendscene;	// if inside begin-end scene cycle
		R9TEXTURE		m_texture;			// current texture
		fRect			m_viewport;			// viewport rect
		int				m_viewx;			// view x offset
		int				m_viewy;			// view y offset
		dword			m_viewflip;			// view flip option
		fRect			m_cliprect;			// clipping rect (clipping is performed if rect is valid)
		BOOL			m_needflush;		// need flush, batch buffer not empty
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
inline void r9Render::DrawLine( fV2& a, fV2& b, dword color )
{
	if(GetTexture()!=NULL) SetTexture(NULL);

	r9Vertex vx[2];

	vx[0].x = a.x;
	vx[0].y = a.y;
	vx[0].u = 0.0f;
	vx[0].v = 0.0f;
	vx[0].color = color;

	vx[1].x = b.x;
	vx[1].y = b.y;
	vx[1].u = 1.0f;
	vx[1].v = 1.0f;
	vx[1].color = color;

	Push(vx, 2, Primitive::Line);
}

inline void r9Render::DrawTriangle( fV2& a, fV2& b, fV2& c, fV2& ta, fV2& tb, fV2& tc, R9TEXTURE tex, dword color )
{
	if(GetTexture()!=tex) SetTexture(tex);

	r9Vertex vx[3];

	vx[0].x = a.x;
	vx[0].y = a.y;
	vx[0].u = ta.x;
	vx[0].v = ta.y;
	vx[0].color = color;

	vx[1].x = b.x;
	vx[1].y = b.y;
	vx[1].u = tb.x;
	vx[1].v = tb.y;
	vx[1].color = color;

	vx[2].x = c.x;
	vx[2].y = c.y;
	vx[2].u = tc.x;
	vx[2].v = tc.y;
	vx[2].color = color;

	Push(vx, 3, Primitive::Triangle);
}

inline void r9Render::DrawQuad( fRect& dst, fRect& src, R9TEXTURE tex, dword color )
{
	if(GetTexture()!=tex) SetTexture(tex);

	r9Vertex vx[6];

	vx[0].x = dst.x1;
	vx[0].y = dst.y1;
	vx[0].u = src.x1;
	vx[0].v = src.y1;
	vx[0].color = color;

	vx[1].x = dst.x2;
	vx[1].y = dst.y1;
	vx[1].u = src.x2;
	vx[1].v = src.y1;
	vx[1].color = color;

	vx[2].x = dst.x1;
	vx[2].y = dst.y2;
	vx[2].u = src.x1;
	vx[2].v = src.y2;
	vx[2].color = color;

	vx[3].x = dst.x2;
	vx[3].y = dst.y1;
	vx[3].u = src.x2;
	vx[3].v = src.y1;
	vx[3].color = color;

	vx[4].x = dst.x2;
	vx[4].y = dst.y2;
	vx[4].u = src.x2;
	vx[4].v = src.y2;
	vx[4].color = color;

	vx[5].x = dst.x1;
	vx[5].y = dst.y2;
	vx[5].u = src.x1;
	vx[5].v = src.y2;
	vx[5].color = color;

	Push(vx, 6, Primitive::Triangle);
}

inline void r9Render::DrawBar( fRect& dst, dword color )
{
	DrawQuad(dst,fRect(0.0f,0.0f,1.0f,1.0f),NULL,color);
}

inline void r9Render::ClipBar( fRect& dst )
{
	if(!IsClipping()) return;
	
	if(dst.x1<m_cliprect.x1) dst.x1=m_cliprect.x1;	else
	if(dst.x1>m_cliprect.x2) dst.x1=m_cliprect.x2;
	
	if(dst.y1<m_cliprect.y1) dst.y1=m_cliprect.y1;	else
	if(dst.y1>m_cliprect.y2) dst.y1=m_cliprect.y2;
	
	if(dst.x2>m_cliprect.x2) dst.x2=m_cliprect.x2;	else
	if(dst.x2<m_cliprect.x1) dst.x2=m_cliprect.x1;
	
	if(dst.y2>m_cliprect.y2) dst.y2=m_cliprect.y2;	else
	if(dst.y2<m_cliprect.y1) dst.y2=m_cliprect.y1;

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
		src.x1 += f * (dst.x1 - dst0.x1);
		src.x2 += f * (dst.x2 - dst0.x2);
	}
	if(dsz.y>0.0f) {
		f = ssz.y / dsz.y;
		src.y1 += f * (dst.y1 - dst0.y1);
		src.y2 += f * (dst.y2 - dst0.y2);
	}

}

inline void r9Render::ClipSprite( fRect& dst, fRect& src, int flip )
{
	if(!IsClipping()) return;
	if(flip & 1)
	{
		if(dst.x1<m_cliprect.x1) { src.x2-=(m_cliprect.x1-dst.x1); dst.x1=m_cliprect.x1; }
		if(dst.x2>m_cliprect.x2) { src.x1+=(dst.x2-m_cliprect.x2); dst.x2=m_cliprect.x2; }
	}
	else
	{
		if(dst.x1<m_cliprect.x1) { src.x1+=(m_cliprect.x1-dst.x1); dst.x1=m_cliprect.x1; }
		if(dst.x2>m_cliprect.x2) { src.x2-=(dst.x2-m_cliprect.x2); dst.x2=m_cliprect.x2; }
	}
	if(flip & 2)
	{
		if(dst.y1<m_cliprect.y1) { src.y2-=(m_cliprect.y1-dst.y1); dst.y1=m_cliprect.y1; }
		if(dst.y2>m_cliprect.y2) { src.y1+=(dst.y2-m_cliprect.y2); dst.y2=m_cliprect.y2; }
	}
	else
	{
		if(dst.y1<m_cliprect.y1) { src.y1+=(m_cliprect.y1-dst.y1); dst.y1=m_cliprect.y1; }
		if(dst.y2>m_cliprect.y2) { src.y2-=(dst.y2-m_cliprect.y2); dst.y2=m_cliprect.y2; }
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
inline	BOOL		R9_TextureIsValid( R9TEXTURE tex )						{ return tex->m_handler!=NULL; }
inline	int			R9_TextureGetWidth( R9TEXTURE tex )						{ return tex->m_width; }
inline	int			R9_TextureGetHeight( R9TEXTURE tex )					{ return tex->m_height; }
inline	int			R9_TextureGetRealWidth( R9TEXTURE tex )					{ return tex->m_realwidth; }
inline	int			R9_TextureGetRealHeight( R9TEXTURE tex )				{ return tex->m_realheight; }

inline	void		R9_SetTexture( R9TEXTURE tex )							{ assert(r9_render); r9_render->SetTexture(tex); }
inline	R9TEXTURE	R9_GetTexture()											{ assert(r9_render); return r9_render->GetTexture(); }
inline	void		R9_SetViewport( fRect& rect )							{ assert(r9_render); r9_render->SetViewport(rect); }
inline	const fRect& R9_GetViewport()										{ assert(r9_render); return r9_render->GetViewport(); }
inline	void		R9_SetView( int x, int y, dword flip )					{ assert(r9_render); r9_render->SetView(x,y,flip); }
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
inline	fRect&		R9_GetClipping()										{ assert(r9_render); return r9_render->GetClipping(); }
inline	void		R9_ResetClipping()										{ assert(r9_render); r9_render->SetClipping(fRect()); }
inline	void		R9_AddClipping( fRect& rect )							{ assert(r9_render); if(!r9_render->IsClipping()) r9_render->SetClipping(rect); else r9_render->SetClipping(r9_render->GetClipping() * rect); }
inline 	void		R9_ClipBar( fRect& dst )								{ assert(r9_render); r9_render->ClipBar(dst); }
inline 	void		R9_ClipQuad( fRect& dst, fRect& src )					{ assert(r9_render); r9_render->ClipQuad(dst,src); }
inline 	void		R9_ClipSprite( fRect& dst, fRect& src, int flip=0 )		{ assert(r9_render); r9_render->ClipSprite(dst,src,flip); }

inline	BOOL		R9_SaveScreenShot( fRect* rect=NULL, BOOL full=TRUE)				{ assert(r9_render); return r9_render->SaveScreenShot(rect,full); }
inline	BOOL		R9_TakeScreenShot( r9Img* img, fRect* rect=NULL, BOOL full=TRUE )	{ assert(r9_render); return r9_render->TakeScreenShot(img,rect,full); }
inline	BOOL		R9_CopyTargetToImage( R9TEXTURE target, r9Img* img, fRect* rect )	{ assert(r9_render); return r9_render->CopyTargetToImage(target,img,rect); }

#endif
///////////////////////////////////////////////////////////////////////////////////////////////////
