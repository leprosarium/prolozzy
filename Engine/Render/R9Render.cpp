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
	windowed(true),
	width(CfgWidth),
	height(CfgHeight),
	bpp(CfgBPP),
	refresh(),
	vsync()
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

	if(m_cfg.bpp!=16 && m_cfg.bpp!=32) return false;

	if(!Init()) return false;

	SetDefaultStates();

	// clear doublebuffer
	if(BeginScene()) { Clear(0xff000000); EndScene(); Present(); }
	if(BeginScene()) { Clear(0xff000000); EndScene(); Present(); }

	// font
	MakeFont();
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



R9TEXTURE r9Render::TextureLoad( const std::string & filename )
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

void r9Render::SetView( const iV2 & v, dword flip )
{
	viewOffs = v;
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
	SetView(iV2(), 0 );
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

bool r9Render::SaveScreenShot( fRect* rect, bool full )
{
	r9Img img;
	if(!TakeScreenShot(&img, rect, full)) return false;

	char file[64];
	char date[16];
	char time[16];
	
	SYSTEMTIME systime;
	GetSystemTime( &systime );
	GetDateFormat( NULL, 0, &systime, "yyMMdd", date, 16 );
	GetTimeFormat( NULL, 0, &systime, "_HHmm_ss", time, 16 );

	CreateDirectory("ScreenShots",NULL);
	strcpy( file, "ScreenShots\\" );
	strcat( file, date );
	strcat( file, time );
	strcat( file, ".png" ); // change this if you want (.tga)
	
	R9_ImgSaveFile(file,&img);
	R9_ImgDestroy(&img);
	
	dlog(LOGRND, L"ScreenShot saved!\n");

	return true;
}

bool r9Render::TakeScreenShot( r9Img* img, fRect* rect, bool full )
{
	if(img==nullptr) return false;
	if( IsBeginEndScene() ) { dlog(LOGRND, L"ScreenShot can not be taken inside Begin - End frame.\n"); return false; }
	if( m_cfg.bpp!=32 ) 	{ dlog(LOGRND, L"ScreenShot can be taken only in 32bit modes.\n"); return false; }
	R9_ImgDestroy(img);
	return DoTakeScreenShot(img, rect, full);
}

bool r9Render::CopyTargetToImage( R9TEXTURE target, r9Img* img, fRect* rect )
{
	assert(img); 
	assert(rect);
	assert(target);
	if(!img->isValid()) return FALSE;

	iV2 sz = rect->Size();
	if(sz.x > target->realwidth) return false;
	if(sz.y > target->realheight) return false;
	return CopyTargetToImage(target, img, rect->p1, sz);
}

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

	bool rotated = Is<Flip::R>(flip);

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

bool r9Render::MakeFont()
{
	assert(m_font==nullptr);

	// create fixed font (courier new)
	m_font = new r9Font();
	m_font->Create(ChrW,ChrH-1);

	// create texture from memory
	;
	dword memsize = r9_fonttga_buffer[0];
	byte * memfile = new byte[memsize];
	if(decompress_data((byte*)(r9_fonttga_buffer+2),r9_fonttga_buffer[1],memfile,memsize))
		if(R9TEXTURE tex = TextureLoad(F9_MakeFileName("font.tga",memfile,memsize)))
		{
			m_font->SetTexture(tex);
			delete [] memfile;
			return true;
		}
	delete m_font; m_font=nullptr;
	delete [] memfile;
	return false;
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
	cfgout.refresh = 0;
	// search modes	
	for(const r9DisplayMode & mode: r9Render::DisplayModes)
	{
		if( cfgout.windowed != mode.windowed ) continue;
		
		if( cfgout.windowed ) // in windowed
		{
			// overwrite bpp
			cfgout.bpp = mode.bpp;
			// clamp resolution in windowed
			if( cfgout.width > mode.width || cfgout.height > mode.height )
			{
				cfgout.width = mode.width;
				cfgout.height =  mode.height;
			}
			// overwrite refresh
			cfgout.refresh = 0;
			ok = true;
			// got it
			break;
		}
		// in fullscreen
		// match bpp
		if(cfgout.bpp != mode.bpp) continue;
		// match resolution
		if(cfgout.width != mode.width ) continue;
		if(cfgout.height != mode.height ) continue;
		// select highest refresh found, but not higher then requested
		if(mode.refresh > cfgout.refresh && mode.refresh <= cfg.refresh )
			cfgout.refresh = mode.refresh; 
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
		cfg.windowed?"windowed":"full-screen",
		cfg.width, cfg.height,
		cfg.bpp,
		cfg.refresh,
		cfg.vsync?" vsync":""
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

void R9_DrawText(const fV2 & pos, const char* text, dword color, float scale )
{ 
	assert(r9_render); 
	if(!r9_render->m_font) return; 
	r9_render->m_font->SetColor(color); 
	r9_render->m_font->m_scale=scale; 
	r9_render->m_font->Print(pos.x,pos.y,text); 
}

///////////////////////////////////////////////////////////////////////////////////////////////////
