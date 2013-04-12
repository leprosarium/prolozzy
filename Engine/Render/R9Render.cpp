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

void r9Render::Clear( dword color )							{}				
BOOL r9Render::BeginScene( R9TEXTURE target )				{ return TRUE; }
void r9Render::EndScene()									{}
void r9Render::Present()									{}
BOOL r9Render::CheckDevice()								{ return TRUE; }
BOOL r9Render::ToggleVideoMode()							{ return FALSE; }

BOOL r9Render::SaveScreenShot( fRect* rect, BOOL full)							{ return TRUE; }
BOOL r9Render::TakeScreenShot( r9Img* img, fRect* rect, BOOL full )				{ return TRUE; }
BOOL r9Render::CopyTargetToImage( R9TEXTURE target, r9Img* img, fRect* rect )	{ return FALSE; }

///////////////////////////////////////////////////////////////////////////////////////////////////
void r9Render::DrawQuadRot( fV2& pos, fV2& size, fV2& center, float angle, fRect& src, R9TEXTURE tex, dword color )
{
	if(GetTexture()!=tex) SetTexture(tex);

	fV2 dst[4];
	dst[0].x = -center.x - size.x/2.0f;
	dst[0].y = -center.y - size.y/2.0f;
	dst[1].x = -center.x + size.x/2.0f;
	dst[1].y = -center.y - size.y/2.0f;
	dst[2].x = -center.x + size.x/2.0f;
	dst[2].y = -center.y + size.y/2.0f;
	dst[3].x = -center.x - size.x/2.0f;
	dst[3].y = -center.y + size.y/2.0f;

	float angsin = sinf(DEG2RAD(angle));
	float angcos = cosf(DEG2RAD(angle));
	dst[0] = Rotate(dst[0],angsin,angcos) + pos;
	dst[1] = Rotate(dst[1],angsin,angcos) + pos;
	dst[2] = Rotate(dst[2],angsin,angcos) + pos;
	dst[3] = Rotate(dst[3],angsin,angcos) + pos;

	r9Vertex vx[6];

	vx[0].x = dst[0].x;
	vx[0].y = dst[0].y;
	vx[0].u = src.x1;
	vx[0].v = src.y1;
	vx[0].color = color;

	vx[1].x = dst[1].x;
	vx[1].y = dst[1].y;
	vx[1].u = src.x2;
	vx[1].v = src.y1;
	vx[1].color = color;

	vx[2].x = dst[3].x;
	vx[2].y = dst[3].y;
	vx[2].u = src.x1;
	vx[2].v = src.y2;
	vx[2].color = color;

	vx[3].x = dst[1].x;
	vx[3].y = dst[1].y;
	vx[3].u = src.x2;
	vx[3].v = src.y1;
	vx[3].color = color;

	vx[4].x = dst[2].x;
	vx[4].y = dst[2].y;
	vx[4].u = src.x2;
	vx[4].v = src.y2;
	vx[4].color = color;

	vx[5].x = dst[3].x;
	vx[5].y = dst[3].y;
	vx[5].u = src.x1;
	vx[5].v = src.y2;
	vx[5].color = color;

	Push(vx, 6, Primitive::Triangle);
}

void r9Render::DrawSprite( const fV2 & pos, const fRect & src, R9TEXTURE tex, dword color, dword flip, float scale )
{

	BOOL rotated = flip & R9_FLIPR;

	fRect dst(pos, pos + (rotated ? src.Size().Tran() : src.Size()) * scale);
	fRect src0 = src;
	if(flip & 1)	{ src0.x1=src.x2; src0.x2=src.x1; }
	if(flip & 2)	{ src0.y1=src.y2; src0.y2=src.y1; }
	if(rotated)		{ fRect src1 = src0; src0.x1=src1.y2; src0.y1=src1.x1; src0.x2=src1.y1; src0.y2=src1.x2; }

	// src: normal={x1y1,x2y2}; rotated={y2x1,y1x2};

	ClipQuad(dst,src0);
	if(dst.x2<=dst.x1 || dst.y2<=dst.y1) return;

	if(GetTexture()!=tex) SetTexture(tex);

	if(tex)
	{
		if(rotated)
		{
			src0.x1 /= tex->m_realheight;
			src0.x2 /= tex->m_realheight;
			src0.y1 /= tex->m_realwidth;
			src0.y2 /= tex->m_realwidth;
		}
		else
		{
			src0.x1 /= tex->m_realwidth;
			src0.x2 /= tex->m_realwidth;
			src0.y1 /= tex->m_realheight;
			src0.y2 /= tex->m_realheight;
		}
	}

	r9Vertex vx[6];

	if(rotated)
	{
		vx[0].x = dst.x1;
		vx[0].y = dst.y1;
		vx[0].u = src0.y1;
		vx[0].v = src0.x1;
		vx[0].color = color;

		vx[1].x = dst.x2;
		vx[1].y = dst.y1;
		vx[1].u = src0.y1;
		vx[1].v = src0.x2;
		vx[1].color = color;

		vx[2].x = dst.x1;
		vx[2].y = dst.y2;
		vx[2].u = src0.y2;
		vx[2].v = src0.x1;
		vx[2].color = color;

		vx[3].x = dst.x2;
		vx[3].y = dst.y1;
		vx[3].u = src0.y1;
		vx[3].v = src0.x2;
		vx[3].color = color;

		vx[4].x = dst.x2;
		vx[4].y = dst.y2;
		vx[4].u = src0.y2;
		vx[4].v = src0.x2;
		vx[4].color = color;

		vx[5].x = dst.x1;
		vx[5].y = dst.y2;
		vx[5].u = src0.y2;
		vx[5].v = src0.x1;
		vx[5].color = color;
	}
	else
	{
		vx[0].x = dst.x1;
		vx[0].y = dst.y1;
		vx[0].u = src0.x1;
		vx[0].v = src0.y1;
		vx[0].color = color;

		vx[1].x = dst.x2;
		vx[1].y = dst.y1;
		vx[1].u = src0.x2;
		vx[1].v = src0.y1;
		vx[1].color = color;

		vx[2].x = dst.x1;
		vx[2].y = dst.y2;
		vx[2].u = src0.x1;
		vx[2].v = src0.y2;
		vx[2].color = color;

		vx[3].x = dst.x2;
		vx[3].y = dst.y1;
		vx[3].u = src0.x2;
		vx[3].v = src0.y1;
		vx[3].color = color;

		vx[4].x = dst.x2;
		vx[4].y = dst.y2;
		vx[4].u = src0.x2;
		vx[4].v = src0.y2;
		vx[4].color = color;

		vx[5].x = dst.x1;
		vx[5].y = dst.y2;
		vx[5].u = src0.x1;
		vx[5].v = src0.y2;
		vx[5].color = color;
	}

	Push(vx, 6, Primitive::Triangle);
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
