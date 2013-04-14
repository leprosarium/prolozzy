///////////////////////////////////////////////////////////////////////////////////////////////////
// R9Render.cpp
///////////////////////////////////////////////////////////////////////////////////////////////////
#include "stdafx.h"
#include "R9Render.h"
#include "R9RenderDX.h"
#include "R9RenderGL.h"
#include "R9ImgLoader.h"
#include "R9Font.h"
#include "R9Resources.h"

r9Cfg::r9Cfg() : 
	m_width(R9_CFG_WIDTH),
	m_height(R9_CFG_HEIGHT),
	m_bpp(R9_CFG_BPP),
	m_windowed(1),
	m_refresh(),
	m_vsync()
{
}

r9Render::r9Render(Api api) : 
	api(api), 
	blend(Blend::Alpha), 
	primitive(Primitive::Triangle), 
	taddress(TAddress::Wrap), 
	filter(Filter::Linear),
	m_dll(),
	m_hwnd(),
	m_beginendscene(),
	m_texture(),
	m_viewx(),
	m_viewy(),
	m_viewflip(),
	m_needflush(),
	m_primitivecount(),
	m_font(),
	m_handlereset()
{
}

bool r9Render::Init(HWND hwnd, r9Cfg * cfg)
{
	m_hwnd = hwnd;
	
	if(cfg) m_cfg = *cfg;
	Api api = r9Render::api;
	R9_FilterCfg(m_cfg, api);

	if(m_cfg.m_bpp!=16 && m_cfg.m_bpp!=32) return false;

	if(!Init()) return false;

	SetDefaultStates();

	// clear doublebuffer
	if(BeginScene()) { Clear(0xff000000); EndScene(); Present(); }
	if(BeginScene()) { Clear(0xff000000); EndScene(); Present(); }

	// font
	CreateFont();
	return true;
}

void r9Render::Done()
{
	if(m_font)
	{
		TextureDestroy(m_font->GetTexture()); 
		m_font->Destroy(); 
		delete m_font; 
	}
	Finish();
}

R9TEXTURE r9Render::TextureCreate(r9Img* img)
{
	if(!img) return nullptr;
	if(!img->isValid()) return nullptr;
	int imgbpp = R9_PFBpp(img->m_pf);
	if(imgbpp!=24 && imgbpp!=32) return nullptr;
	return TextureCreateImg(img);	
}



R9TEXTURE r9Render::TextureLoad( const char* filename )
{
	r9Img img;
	if(!R9_ImgLoadFile(filename, &img)) return nullptr;
	R9TEXTURE tex = TextureCreate(&img);
	R9_ImgDestroy(&img);
	return tex;
}

void r9Render::SetTexture( R9TEXTURE texture ) 
{
	if(m_texture==texture) return;
	if(NeedFlush()) Flush();
	m_texture = texture;
	ApplyTexture();
}

void r9Render::SetViewport( const fRect & rect )
{
	if(m_viewport==rect) return;
	m_viewport = rect;
	ApplyViewport();
}

void r9Render::SetView( int x, int y, dword flip )
{
	m_viewx = x;
	m_viewy = y;
	m_viewflip = flip;
	ApplyView();
}

void r9Render::SetBlend(Blend b)
{
	if(blend == b) return;
	if(NeedFlush()) Flush();
	blend = b;
	ApplyBlend();
}	

void r9Render::SetPrimitive(Primitive p)
{
	if(primitive == p) return;
	if(NeedFlush()) Flush();
	primitive = p;
}	

void r9Render::SetTAddress(TAddress a)
{
	if(taddress == a) return;
	if(NeedFlush()) Flush();
	taddress = a;
	ApplyTAddress();
}	

void r9Render::SetFilter(Filter f)
{
	if(filter == f) return;
	if(NeedFlush()) Flush();
	filter = f;
	ApplyFilter();
}	


void r9Render::SetDefaultStates()
{
	ResetDefaultStates();

	m_texture = NULL;
	if(NeedFlush()) Flush();
	blend = Blend::Alpha;
	primitive = Primitive::Triangle;
	taddress = TAddress::Wrap;
	filter = Filter::Linear;
	ApplyBlend();
	ApplyTAddress();
	ApplyFilter();
	SetViewport(fRect(0,0,GetWidth(),GetHeight()));
	SetView( 0, 0, 0 );
}

bool r9Render::BeginScene(R9TEXTURE target)
{
	if(m_beginendscene) return false;
	m_primitivecount = 0;
	bool r = DoBeginScene(target);
	if(r)
		m_beginendscene = true;
	return r;
}

void r9Render::EndScene()
{
	if( !m_beginendscene ) return;
	if(NeedFlush()) Flush();
	m_beginendscene = false;
	DoEndScene();
}

BOOL r9Render::CheckDevice()								{ return TRUE; }
BOOL r9Render::ToggleVideoMode()							{ return FALSE; }

BOOL r9Render::SaveScreenShot( fRect* rect, BOOL full)							{ return TRUE; }
BOOL r9Render::TakeScreenShot( r9Img* img, fRect* rect, BOOL full )				{ return TRUE; }
BOOL r9Render::CopyTargetToImage( R9TEXTURE target, r9Img* img, fRect* rect )	{ return FALSE; }

///////////////////////////////////////////////////////////////////////////////////////////////////
void r9Render::DrawQuadRot(const fV2 & pos, const fV2 & size, const fV2 & center, float angle, const fRect & src, R9TEXTURE tex, dword color )
{
	SetTexture(tex);
	fV2 sz = size * 0.5f;
	fV2 d0 = -center - sz;
	fV2 d1 = -center + (sz.x, -sz.y);
	fV2 d2 = -center + sz;
	fV2 d3 = -center + (-sz.x, sz.y);

	float angsin = sinf(DEG2RAD(angle));
	float angcos = cosf(DEG2RAD(angle));
	d0 = Rotate(d0,angsin,angcos) + pos;
	d1 = Rotate(d1,angsin,angcos) + pos;
	d2 = Rotate(d2,angsin,angcos) + pos;
	d3 = Rotate(d3,angsin,angcos) + pos;

	r9Vertex vx[6] = {
		{ d0.x, d0.y, src.p1.x, src.p1.y, color },
		{ d1.x, d1.y, src.p2.x, src.p1.y, color },
		{ d3.x, d3.y, src.p1.x, src.p2.y, color },
		{ d1.x, d1.y, src.p2.x, src.p1.y, color },
		{ d2.x, d2.y, src.p2.x, src.p2.y, color },
		{ d3.x, d3.y, src.p1.x, src.p2.y, color }};
	Push(vx, 6, Primitive::Triangle);
}

void r9Render::DrawSprite( const fV2 & pos, const fRect & src, R9TEXTURE tex, dword color, dword flip, float scale )
{

	BOOL rotated = flip & R9_FLIPR;

	fRect dst(pos, pos + (rotated ? src.Size().Tran() : src.Size()) * scale);
	fRect src0 = src;
	if(flip & 1)	{ src0.p1.x=src.p2.x; src0.p2.x=src.p1.x; }
	if(flip & 2)	{ src0.p1.y=src.p2.y; src0.p2.y=src.p1.y; }
	if(rotated)		{ fRect src1 = src0; src0.p1.x=src1.p2.y; src0.p1.y=src1.p1.x; src0.p2.x=src1.p1.y; src0.p2.y=src1.p2.x; }

	// src: normal={x1y1,x2y2}; rotated={y2x1,y1x2};

	ClipQuad(dst,src0);
	if(dst.p2.x<=dst.p1.x || dst.p2.y<=dst.p1.y) return;

	SetTexture(tex);

	if(tex)
	{
		fV2 tx = rotated ? tex->realSize().Tran() : tex->realSize();
		src0.p1 /= tx;
		src0.p2 /= tx;
	}
	if(rotated)
	{
	r9Vertex vx[6] = {
		{ dst.p1.x, dst.p1.y, src0.p1.y, src0.p1.x, color },
		{ dst.p2.x, dst.p1.y, src0.p1.y, src0.p2.x, color },
		{ dst.p1.x, dst.p2.y, src0.p2.y, src0.p1.x, color },
		{ dst.p2.x, dst.p1.y, src0.p1.y, src0.p2.x, color },
		{ dst.p2.x, dst.p2.y, src0.p2.y, src0.p2.x, color }, 
		{ dst.p1.x, dst.p2.y, src0.p2.y, src0.p1.x, color }};
		Push(vx, 6, Primitive::Triangle);
	}
	else 
	{
	r9Vertex vx[6] = {
		{dst.p1.x, dst.p1.y, src0.p1.x, src0.p1.y, color },
		{dst.p2.x, dst.p1.y, src0.p2.x, src0.p1.y, color },
		{dst.p1.x, dst.p2.y, src0.p1.x, src0.p2.y, color },
		{dst.p2.x, dst.p1.y, src0.p2.x, src0.p1.y, color },
		{dst.p2.x, dst.p2.y, src0.p2.x, src0.p2.y, color },
		{dst.p1.x, dst.p2.y, src0.p1.x, src0.p2.y, color }};
		Push(vx, 6, Primitive::Triangle);
	}

}

BOOL r9Render::CreateFont()
{
	assert(m_font==NULL);
	char* filename; // name for the memory file
	dword memsize; // memory buffer (file) size
	byte* memfile; // memory buffer

	// create fixed font (courier new)
	m_font = new r9Font();
	m_font->Create(R9_CHRW,R9_CHRH-1);

	// create texture from memory
	R9TEXTURE tex;
	memsize = r9_fonttga_buffer[0];
	memfile = (byte*)malloc(memsize);
	if(!decompress_data((byte*)(r9_fonttga_buffer+2),r9_fonttga_buffer[1],memfile,memsize)) goto error;
	filename = F9_MakeFileName("font.tga",memfile,memsize);
	if(!filename) goto error;
	tex = TextureLoad(filename);
	if(!tex) goto error;
	m_font->SetTexture(tex);
	free(memfile);
	return TRUE;

	error:
	delete m_font; m_font=NULL;
	if(memfile) free(memfile);
	return FALSE;
}


///////////////////////////////////////////////////////////////////////////////////////////////////
// INTERFACE
///////////////////////////////////////////////////////////////////////////////////////////////////
r9Render* r9_render = NULL;

r9Render* R9_CreateRender( Api api )
{
	r9Render* render=NULL;
	if(api == Api::DirectX)	render = new r9RenderDX();
	if(api == Api::OpenGL)	render = new r9RenderGL();
	if(!render) return NULL;
	if(!render->LoadDll()) { delete render; return NULL; }
	return render;
}

std::vector<r9DisplayMode> r9Render::DisplayModes;

bool R9_InitInterface( Api api )
{
	dlog(LOGRND, L"Render init interface (api=%i).\n",api);
	r9Render* render = R9_CreateRender(api);
	if(!render) return false;

	// gather display modes
	render->GatherDisplayModes();
	if(r9Render::DisplayModes.empty()) return false;

	render->UnloadDll();
	delete render;
	return true;
}

bool R9_FilterCfg( r9Cfg& cfg, Api & api )
{
	if(r9Render::DisplayModes.empty()) return false;

	dlog(LOGRND, L"Filter config:\n");
	dlog(LOGRND, L"  Requested: "); R9_LogCfg(cfg,api);

	bool ok = false;
	r9Cfg cfgout = cfg;
	cfgout.m_refresh = 0;
	// search modes	
	for(const r9DisplayMode & mode: r9Render::DisplayModes)
	{
		if( cfgout.m_windowed != mode.windowed ) continue;
		
		if( cfgout.m_windowed ) // in windowed
		{
			// overwrite bpp
			cfgout.m_bpp = mode.bpp;
			// clamp resolution in windowed
			if( cfgout.m_width > mode.width || cfgout.m_height > mode.height )
			{
				cfgout.m_width = mode.width;
				cfgout.m_height =  mode.height;
			}
			// overwrite refresh
			cfgout.m_refresh = 0;
			ok = true;
			// got it
			break;
		}
		// in fullscreen
		// match bpp
		if(cfgout.m_bpp != mode.bpp) continue;
		// match resolution
		if(cfgout.m_width != mode.width ) continue;
		if(cfgout.m_height != mode.height ) continue;
		// select highest refresh found, but not higher then requested
		if(mode.refresh > cfgout.m_refresh && mode.refresh <= cfg.m_refresh )
			cfgout.m_refresh = mode.refresh; 
		ok = true;
		// continue search for better refresh
	}
	
	cfg = cfgout;
	if(ok)
	{ dlog(LOGRND, L"  Received:  "); R9_LogCfg(cfg, api); }
	else
	{ dlog(LOGRND, L"  Received:  FAILED\n"); return FALSE; }
	return ok;
}

void R9_LogCfg( r9Cfg& cfg, Api api )
{
	dlog(LOGRND, L"%S %S %ix%i %ibpp %iHz%S\n", 
		api == Api::OpenGL ? "OpenGL" : "DirectX",
		cfg.m_windowed?"windowed":"full-screen",
		cfg.m_width, cfg.m_height,
		cfg.m_bpp,
		cfg.m_refresh,
		cfg.m_vsync?" vsync":""
		);
}

void R9_DoneInterface()
{
	if(r9Render::DisplayModes.empty()) return;
	r9Render::DisplayModes.clear();
	dlog(LOGRND, L"Render done interface.\n");
}

BOOL R9_Init( HWND hwnd, r9Cfg* cfg, Api api )
{
	if(r9_render) return TRUE;
	dlog(LOGRND, L"Render init (api=%i).\n",api);
	r9_render = R9_CreateRender(api);
	if(!r9_render) return FALSE;
	if(!r9_render->Init(hwnd, cfg))
	{
		r9_render->UnloadDll();
		delete r9_render;
		r9_render = NULL;
		return FALSE;
	}
	return TRUE;
}

void R9_Done()
{
	if(!r9_render) return;
	r9_render->Done();
	r9_render->UnloadDll();
	delete r9_render;
	r9_render = NULL;
	dlog(LOGRND, L"Render done.\n");
}

void R9_DrawText( fV2& pos, const char* text, dword color, float scale )
{ 
	assert(r9_render); 
	if(!r9_render->m_font) return; 
	r9_render->m_font->SetColor(color); 
	r9_render->m_font->m_scale=scale; 
	r9_render->m_font->Print(pos.x,pos.y,text); 
}

///////////////////////////////////////////////////////////////////////////////////////////////////
