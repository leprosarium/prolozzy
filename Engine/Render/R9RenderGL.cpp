///////////////////////////////////////////////////////////////////////////////////////////////////
// R9RenderGL.cpp
///////////////////////////////////////////////////////////////////////////////////////////////////
#include "stdafx.h"
#include "R9RenderGL.h"
#include "R9ImgLoader.h"

#ifndef R9_ENABLE_DLLGL
#pragma comment( lib, "opengl32.lib" )
#endif

#define R9_LOGERROR( prefix )	dlog( LOGERR, L"RENDER: %S\n", prefix );

r9RenderGL::twglCreateContext		r9RenderGL::m_wglCreateContext		= nullptr;
r9RenderGL::twglMakeCurrent			r9RenderGL::m_wglMakeCurrent		= nullptr;		
r9RenderGL::twglDeleteContext		r9RenderGL::m_wglDeleteContext		= nullptr;
r9RenderGL::tglGetString			r9RenderGL::m_glGetString			= nullptr;
r9RenderGL::tglGenTextures			r9RenderGL::m_glGenTextures			= nullptr;
r9RenderGL::tglBindTexture			r9RenderGL::m_glBindTexture			= nullptr;
r9RenderGL::tglTexImage2D			r9RenderGL::m_glTexImage2D			= nullptr;
r9RenderGL::tglDeleteTextures		r9RenderGL::m_glDeleteTextures		= nullptr;
r9RenderGL::tglTexParameteri		r9RenderGL::m_glTexParameteri		= nullptr;
r9RenderGL::tglBlendFunc			r9RenderGL::m_glBlendFunc			= nullptr;
r9RenderGL::tglTexEnvi				r9RenderGL::m_glTexEnvi				= nullptr;
r9RenderGL::tglEnable				r9RenderGL::m_glEnable				= nullptr;
r9RenderGL::tglShadeModel			r9RenderGL::m_glShadeModel			= nullptr;
r9RenderGL::tglViewport				r9RenderGL::m_glViewport			= nullptr;		
r9RenderGL::tglMatrixMode			r9RenderGL::m_glMatrixMode			= nullptr;
r9RenderGL::tglLoadIdentity			r9RenderGL::m_glLoadIdentity		= nullptr;		
r9RenderGL::tglOrtho				r9RenderGL::m_glOrtho				= nullptr;
r9RenderGL::tglClearColor			r9RenderGL::m_glClearColor			= nullptr;
r9RenderGL::tglClear				r9RenderGL::m_glClear				= nullptr;
r9RenderGL::tglEnableClientState	r9RenderGL::m_glEnableClientState	= nullptr;
r9RenderGL::tglColorPointer			r9RenderGL::m_glColorPointer		= nullptr;		
r9RenderGL::tglVertexPointer		r9RenderGL::m_glVertexPointer		= nullptr;
r9RenderGL::tglTexCoordPointer		r9RenderGL::m_glTexCoordPointer		= nullptr;
r9RenderGL::tglDrawArrays			r9RenderGL::m_glDrawArrays			= nullptr;
r9RenderGL::tglDisableClientState	r9RenderGL::m_glDisableClientState	= nullptr;
r9RenderGL::tglPixelStorei			r9RenderGL::m_glPixelStorei			= nullptr;
r9RenderGL::tglReadPixels			r9RenderGL::m_glReadPixels			= nullptr;
r9RenderGL::tglCopyTexImage2D		r9RenderGL::m_glCopyTexImage2D		= nullptr;
r9RenderGL::tglGetTexImage			r9RenderGL::m_glGetTexImage			= nullptr;

r9RenderGL::r9RenderGL() : 
	r9Render(Api::OpenGL),
	m_hdc(),
	m_hrc(),
	m_batchcount(),
	m_batchbuffer(),
	m_textarget()
{
	m_caps.m_texture_env_combine	= FALSE;
}

#define _GETDLLPROC( name )	m_##name = (t##name)GetProcAddress(m_dll,#name); if(!m_##name) { R9_LOGERROR("bad dll version."); goto error; }
#define _SETLIBPROC( name ) m_##name = name;

bool r9RenderGL::LoadDll()
{
#ifdef R9_ENABLE_DLLGL
	if(m_dll) return true;
	m_dll = LoadLibrary("opengl32.dll");
	if(!m_dll) { R9_LOGERROR("can't load opengl32.dll"); return false; }

	_GETDLLPROC(	wglCreateContext		);
	_GETDLLPROC(	wglMakeCurrent			);
	_GETDLLPROC(	wglDeleteContext		);
	_GETDLLPROC(	glGetString				);
	_GETDLLPROC(	glGenTextures			);
	_GETDLLPROC(	glBindTexture			);
	_GETDLLPROC(	glTexImage2D			);
	_GETDLLPROC(	glDeleteTextures		);
	_GETDLLPROC(	glTexParameteri			);
	_GETDLLPROC(	glBlendFunc				);
	_GETDLLPROC(	glTexEnvi				);
	_GETDLLPROC(	glEnable				);
	_GETDLLPROC(	glShadeModel			);
	_GETDLLPROC(	glViewport				);
	_GETDLLPROC(	glMatrixMode			);
	_GETDLLPROC(	glLoadIdentity			);
	_GETDLLPROC(	glOrtho					);
	_GETDLLPROC(	glClearColor			);
	_GETDLLPROC(	glClear					);
	_GETDLLPROC(	glEnableClientState		);
	_GETDLLPROC(	glColorPointer			);
	_GETDLLPROC(	glVertexPointer			);
	_GETDLLPROC(	glTexCoordPointer		);
	_GETDLLPROC(	glDrawArrays			);
	_GETDLLPROC(	glDisableClientState	);
	_GETDLLPROC(	glPixelStorei			);
	_GETDLLPROC(	glReadPixels			);
	_GETDLLPROC(	glCopyTexImage2D		);
	_GETDLLPROC(	glGetTexImage			);

	return true;
	error:
	UnloadDll();
	return false;

#else

	_SETLIBPROC(	wglCreateContext		);
	_SETLIBPROC(	wglMakeCurrent			);
	_SETLIBPROC(	wglDeleteContext		);
	_SETLIBPROC(	glGetString				);
	_SETLIBPROC(	glGenTextures			);
	_SETLIBPROC(	glBindTexture			);
	_SETLIBPROC(	glTexImage2D			);
	_SETLIBPROC(	glDeleteTextures		);
	_SETLIBPROC(	glTexParameteri			);
	_SETLIBPROC(	glBlendFunc				);
	_SETLIBPROC(	glTexEnvi				);
	_SETLIBPROC(	glEnable				);
	_SETLIBPROC(	glShadeModel			);
	_SETLIBPROC(	glViewport				);
	_SETLIBPROC(	glMatrixMode			);
	_SETLIBPROC(	glLoadIdentity			);
	_SETLIBPROC(	glOrtho					);
	_SETLIBPROC(	glClearColor			);
	_SETLIBPROC(	glClear					);
	_SETLIBPROC(	glEnableClientState		);
	_SETLIBPROC(	glColorPointer			);
	_SETLIBPROC(	glVertexPointer			);
	_SETLIBPROC(	glTexCoordPointer		);
	_SETLIBPROC(	glDrawArrays			);
	_SETLIBPROC(	glDisableClientState	);
	_SETLIBPROC(	glPixelStorei			);
	_SETLIBPROC(	glReadPixels			);
	_SETLIBPROC(	glCopyTexImage2D		);
	_SETLIBPROC(	glGetTexImage			);

	return true;
#endif
}

void r9RenderGL::UnloadDll()
{
#ifdef R9_ENABLE_DLLGL
	if(!m_dll) return;
	FreeLibrary(m_dll);	
	m_dll = nullptr;
#endif

	m_wglCreateContext		= nullptr;
	m_wglMakeCurrent		= nullptr;
	m_wglDeleteContext		= nullptr;
	m_glGetString			= nullptr;
	m_glGenTextures			= nullptr;
	m_glBindTexture			= nullptr;
	m_glTexImage2D			= nullptr;
	m_glDeleteTextures		= nullptr;
	m_glTexParameteri		= nullptr;
	m_glBlendFunc			= nullptr;
	m_glTexEnvi				= nullptr;
	m_glEnable				= nullptr;
	m_glShadeModel			= nullptr;
	m_glViewport			= nullptr;
	m_glMatrixMode			= nullptr;
	m_glLoadIdentity		= nullptr;
	m_glOrtho				= nullptr;
	m_glClearColor			= nullptr;
	m_glClear				= nullptr;
	m_glEnableClientState	= nullptr;
	m_glColorPointer		= nullptr;
	m_glVertexPointer		= nullptr;
	m_glTexCoordPointer		= nullptr;
	m_glDrawArrays			= nullptr;
	m_glDisableClientState	= nullptr;
	m_glPixelStorei			= nullptr;
	m_glReadPixels			= nullptr;
	m_glCopyTexImage2D		= nullptr;
	m_glGetTexImage			= nullptr;

}

void r9RenderGL::LogAdapterInfo() const
{
	DISPLAY_DEVICE  dd;
	memset(&dd,0,sizeof(dd));
	dd.cb = sizeof(DISPLAY_DEVICE);
	int i=0;
	while(TRUE)
	{
		if(!EnumDisplayDevices(NULL,i,&dd,0)) break;
		if(!(dd.StateFlags & DISPLAY_DEVICE_PRIMARY_DEVICE)) continue; // ignore other devices

		dlog(LOGRND, L"Video adapter #%i info:\n", i);
		dlog(LOGRND, L"  device name = %S\n", dd.DeviceName);
		dlog(LOGRND, L"  device string = %S\n", dd.DeviceString);
		dlog(LOGRND, L"  device flags = %x\n", dd.StateFlags);
		return;
	}

	dlog(LOGRND, L"Video adapter unknown.");
}



void r9RenderGL::GatherDisplayModes() const
{
	// current display mode (windowed)
	DEVMODE devmode;
	memset(&devmode,0,sizeof(devmode));
	devmode.dmSize = sizeof(DEVMODE);
	BOOL ok = EnumDisplaySettings(NULL,ENUM_CURRENT_SETTINGS,&devmode);
    if(ok) 
	{
		int bpp = devmode.dmBitsPerPel;
		if(bpp==16 || bpp==32) 
		{ 
			r9DisplayMode m = {1, bpp, devmode.dmPelsWidth, devmode.dmPelsHeight, devmode.dmDisplayFrequency, 0};
			DisplayModes.push_back(m);
		}
		else	
			R9_LOGERROR("invalid current display mode format.");
	}
	else 
		R9_LOGERROR("failed to get current display mode."); 
	
	LogAdapterInfo();

	// supported display modes (fullscreen)
	for(int idx=0;;++idx)
	{
		memset(&devmode,0,sizeof(devmode));
		devmode.dmSize = sizeof(DEVMODE);
		ok = EnumDisplaySettings(NULL,idx,&devmode);
		if(!ok) break; // got them all
		int bpp = devmode.dmBitsPerPel;
		if(bpp!=16 && bpp!=32) continue;
		r9DisplayMode m = {0, bpp, devmode.dmPelsWidth, devmode.dmPelsHeight, devmode.dmDisplayFrequency, 0};
		DisplayModes.push_back(m);
	}

	// sort modes by windowed, bpp, width, height, refresh
	std::sort(DisplayModes.begin(), DisplayModes.end());

	// log
	dlog(LOGRND, L"Display modes:\n");
	for(const r9DisplayMode &m: DisplayModes) m.log(LOGRND);
	dlog(LOGRND, L"\n");
}

bool r9RenderGL::Init()
{
	// prepare window
	PrepareParentWindow();
	if(!CreateRenderWindow()) { DestroyRenderWindow(); return false; }

	// create opengl device
	if(!GL_CreateDevice()) { DestroyRenderWindow(); return false; }

	// batch
	GL_BatchCreate();
	return true;
}

void r9RenderGL::Finish()
{
	free(m_batchbuffer);
	if(!m_cfg.windowed) ChangeDisplaySettings(NULL,0);
	if(m_hrc) { m_wglMakeCurrent(NULL,NULL); m_wglDeleteContext(m_hrc); m_hrc=NULL; }
	if(m_hdc) { ReleaseDC(m_hwnd,m_hdc); m_hdc=NULL; }
	DestroyRenderWindow();
}

bool r9RenderGL::IsReady()
{
	return m_hrc != nullptr;
}

R9TEXTURE r9RenderGL::TextureCreateImg( r9Img* img )
{
	int imgbpp = R9_PFBpp(img->m_pf);

	R9_ImgFlipRGB(img); // opengl is bgr

	// find accepted size, power of 2, etc
	int w = GetPow2HI(img->m_width);
	int h = GetPow2HI(img->m_height);
	if( w < 64 ) w = 64; // safe
	if( h < 64 ) h = 64; // safe

	void* imgdata = img->m_data;

	// resize canvas
	r9Img img2;
	if(w>img->m_width || h>img->m_height)
	{
		img2.m_pf = img->m_pf;
		img2.m_width = w;
		img2.m_height = h;
		R9_ImgCreate(&img2,TRUE); assert(img2.m_data);
		R9_ImgBitBlt(img,0,0,img->m_width,img->m_height,&img2,0,0);
		imgdata = img2.m_data; // use this data
	}
	else
	if(w<img->m_width || h<img->m_height) return nullptr; // it can't be smaller

	// create GL texture
	GLuint gltex;
	m_glGenTextures(1, &gltex);
	m_glBindTexture(GL_TEXTURE_2D, gltex);
	if(imgbpp==24)
		m_glTexImage2D(GL_TEXTURE_2D, 0, 3, w, h, 0, GL_RGB, GL_UNSIGNED_BYTE, imgdata);
	else
		m_glTexImage2D(GL_TEXTURE_2D, 0, 4, w, h, 0, GL_RGBA, GL_UNSIGNED_BYTE, imgdata);

	if(img2.isValid()) R9_ImgDestroy(&img2); // destroy resized if needed

	// create R9 texture
	r9Texture* tex	= new r9Texture;
	tex->width		= img->m_width;
	tex->height		= img->m_height;
	tex->realwidth	= w;
	tex->realheight	= h;
	tex->handler	= (void*)(intptr)gltex;
	tex->handlerex	= nullptr;

	GL_BindTexture(); // for wrap and filter

	return tex;
}

R9TEXTURE r9RenderGL::TextureCreateTarget(int width, int height)
{
	
	// find accepted size, power of 2, etc
	int w = GetPow2HI(width);
	int h = GetPow2HI(height);
	if( w < 64 ) w = 64; // safe
	if( h < 64 ) h = 64; // safe

	// data
	int spp = 4;
	byte* imgdata = new byte[w * h * spp]; assert(imgdata);
	memset(imgdata, 0, w*h*spp);

	// create GL texture
	GLuint gltex;
	m_glGenTextures(1, &gltex);
	m_glBindTexture(GL_TEXTURE_2D, gltex);
	m_glTexImage2D(GL_TEXTURE_2D, 0, spp, w, h, 0, GL_RGB, GL_UNSIGNED_BYTE, imgdata);

	delete [] imgdata;

	// create R9 texture
	r9Texture* tex		= new r9Texture;
	tex->width		= width;
	tex->height		= height;
	tex->realwidth	= w;
	tex->realheight	= h;
	tex->handler		= (void*)(intptr)gltex;
	tex->handlerex	= NULL;

	GL_BindTexture(); // for wrap and filter

	return tex;
}

void r9RenderGL::TextureDestroy( R9TEXTURE texture )
{
	if(!texture) return;
	GLuint gltex = (GLuint)(intptr)texture->handler;
	m_glDeleteTextures(1,&gltex);
	delete texture;
}

void r9RenderGL::ApplyTexture()
{
	GL_BindTexture();
}

void r9RenderGL::ApplyViewport()
{
	m_glViewport((GLint)m_viewport.p1.x,(GLint)m_viewport.p1.y,(GLint)m_viewport.Width(),(GLint)m_viewport.Height());
	m_glMatrixMode(GL_PROJECTION);
	m_glLoadIdentity();
	m_glOrtho(m_viewport.p1.x,m_viewport.p2.x,m_viewport.p2.y,m_viewport.p1.y,-1,1);
	m_glMatrixMode(GL_MODELVIEW);
	m_glLoadIdentity();
}

void r9RenderGL::ApplyView()
{
	fRect rect;
	rect.p1.x = (Is<Flip::X>(m_viewflip)) ? m_viewport.p2.x : m_viewport.p1.x;
	rect.p1.y = (Is<Flip::Y>(m_viewflip)) ? m_viewport.p2.y : m_viewport.p1.y;
	rect.p2.x = (Is<Flip::X>(m_viewflip)) ? m_viewport.p1.x : m_viewport.p2.x;
	rect.p2.y = (Is<Flip::Y>(m_viewflip)) ? m_viewport.p1.y : m_viewport.p2.y;
	rect.p1.x += m_viewx;		
	rect.p2.x += m_viewx;
	rect.p1.y += m_viewy;
	rect.p2.y += m_viewy;

	m_glMatrixMode(GL_PROJECTION);
	m_glLoadIdentity();
	m_glOrtho(rect.p1.x,rect.p2.x,rect.p2.y,rect.p1.y,-1,1);
}

void r9RenderGL::ApplyBlend()
{
	switch(GetBlend())
	{
	case Blend::Opaque:
		m_glBlendFunc(GL_ONE, GL_ZERO);
		break;
	case Blend::Alpha:
		m_glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
		break;
	case Blend::Add:
		m_glBlendFunc(GL_ONE, GL_ONE);
		break;
	case Blend::Mod:
		m_glBlendFunc(GL_DST_COLOR, GL_ZERO);
		break;
	case Blend::Mod2:
		m_glBlendFunc(GL_DST_COLOR, GL_SRC_COLOR);
		break;
	case Blend::AlphaRep:
		m_glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
		break;
	}
	GL_BindTexture(); // apply texture env states
}

void r9RenderGL::ApplyTAddress()
{
	GL_BindTexture();
}

void r9RenderGL::ApplyFilter()
{
	GL_BindTexture();
}

void r9RenderGL::ResetDefaultStates()
{
	// device default states
	m_glEnable(GL_BLEND);
	m_glEnable(GL_TEXTURE_2D);
	m_glShadeModel(GL_SMOOTH);

	m_glViewport(0,0,GetWidth(),GetHeight());						// Reset The Current Viewport
	m_glMatrixMode(GL_PROJECTION);
	m_glLoadIdentity();
	m_glOrtho(0,GetWidth(),GetHeight(),0,-1,1);
	m_glMatrixMode(GL_MODELVIEW);
	m_glLoadIdentity();

}

void r9RenderGL::DoClear(dword color)
{
	fColor fcolor(color);
	m_glClearColor(fcolor.r,fcolor.g,fcolor.b,fcolor.a);
	m_glClear(GL_COLOR_BUFFER_BIT);
}

bool r9RenderGL::DoBeginScene(R9TEXTURE target)
{
	if(target) // use render target
	{
		// the upside down is a mess, so we will render upside down :(
		m_glMatrixMode(GL_PROJECTION);
		m_glLoadIdentity();
		m_glOrtho(0,GetWidth(),0,GetHeight(),-1,1);
		m_textarget = target; // store for later use
	}
	return true;
}

void r9RenderGL::DoEndScene()
{
	// if render target, then copy from screen into it
	if(m_textarget)
	{
		GLuint gltarget = (GLuint)(intptr)m_textarget->handler;
		m_glBindTexture(GL_TEXTURE_2D,gltarget);
		m_glCopyTexImage2D(GL_TEXTURE_2D,0,GL_RGBA,0,0,m_textarget->realwidth,m_textarget->realheight,0);

		// restore projection for normap drawing
		m_glMatrixMode(GL_PROJECTION);
		m_glLoadIdentity();
		m_glOrtho(0,GetWidth(),GetHeight(),0,-1,1);
		m_textarget = nullptr; // clear
	}
}

void r9RenderGL::DoPresent()
{
	SwapBuffers(m_hdc);
}

//@OBSOLETE
bool r9RenderGL::ToggleVideoMode()
{
	dlog(LOGRND, L"Toggle video mode: ");
	bool ok = false;
	if(!m_cfg.windowed) // change to windowed
	{
		ok = (ChangeDisplaySettings(NULL,0)==DISP_CHANGE_SUCCESSFUL);
	}
	else // change to full screen
	{
		DEVMODE devmode;
		memset(&devmode,0,sizeof(DEVMODE));
		devmode.dmSize				= sizeof(DEVMODE);
		devmode.dmPelsWidth			= m_cfg.width;
		devmode.dmPelsHeight		= m_cfg.height;
		devmode.dmBitsPerPel		= m_cfg.bpp;
		devmode.dmDisplayFrequency	= m_cfg.refresh;
		devmode.dmFields			= DM_BITSPERPEL|DM_PELSWIDTH|DM_PELSHEIGHT|DM_DISPLAYFREQUENCY;
		ok = (ChangeDisplaySettings(&devmode,CDS_FULLSCREEN)==DISP_CHANGE_SUCCESSFUL);
	}

	if(ok) 
	{
		m_cfg.windowed = !m_cfg.windowed;
		PrepareParentWindow();
	}
	dlog(LOGRND,ok? L"successful\n" : L"failed\n");
	return ok;
}

void r9RenderGL::Push( r9Vertex* vx, int vxs, Primitive primitive )
{
	// set primitive
	SetPrimitive(primitive);

	// push
	int primitivevertexes = primitive == Primitive::Triangle ? 3 : 2;
	int batchsize = (R9_BATCHSIZE_GL / primitivevertexes) * primitivevertexes; // make multiple of primitive vertexes
	float ofs = (primitive == Primitive::Line) ? 0.5f : 0.0f; // pixel offset

	while(vxs>0)
	{
		// get count
		int count = vxs;
		if( m_batchcount + count > batchsize )
			count = batchsize - m_batchcount;

		// copy and adjust rgb>bgr
		memcpy(m_batchbuffer+m_batchcount,vx,count*sizeof(r9Vertex));
		r9Vertex* bvx = m_batchbuffer + m_batchcount;
		for(int i=0;i<count;i++)
		{
			// rgb to bgr
			byte* c = (byte*)&(bvx->color);
			byte t = c[0];
			c[0] = c[2];
			c[2] = t;
			bvx->y += ofs;
			bvx++;
		}

		vx += count;
		vxs -= count;
		m_batchcount += count;
		m_needflush = true;

		// flush if full
		if( m_batchcount==batchsize ) Flush();
	}
	
}

void r9RenderGL::Flush()
{
	m_needflush = false;
	if(m_batchcount==0) return;

	// set arrays
	m_glEnableClientState( GL_VERTEX_ARRAY );
	m_glEnableClientState( GL_COLOR_ARRAY );
	m_glEnableClientState( GL_TEXTURE_COORD_ARRAY );

	m_glColorPointer(4,GL_UNSIGNED_BYTE,sizeof(r9Vertex),&(m_batchbuffer->color));
	m_glVertexPointer(2,GL_FLOAT,sizeof(r9Vertex),&(m_batchbuffer->x));
	m_glTexCoordPointer(2,GL_FLOAT,sizeof(r9Vertex),&(m_batchbuffer->u));

	// draw
	m_glDrawArrays( GetPrimitive() == Primitive::Triangle ? GL_TRIANGLES : GL_LINES, 0, m_batchcount );

	m_glDisableClientState( GL_VERTEX_ARRAY );
	m_glDisableClientState( GL_COLOR_ARRAY );
	m_glDisableClientState( GL_TEXTURE_COORD_ARRAY );


	m_batchcount = 0;
}


///////////////////////////////////////////////////////////////////////////////////////////////////
// SCREEN SHOT
///////////////////////////////////////////////////////////////////////////////////////////////////

bool r9RenderGL::DoTakeScreenShot( r9Img* img, fRect* rect, bool full )
{
	if(full) // directly from screen; very slow
	{
		DEVMODE devmode;
		memset(&devmode,0,sizeof(devmode));
		devmode.dmSize = sizeof(DEVMODE);
		if(!EnumDisplaySettings(NULL,ENUM_CURRENT_SETTINGS,&devmode)) return false;
		iRect irect(0,0,devmode.dmPelsWidth,devmode.dmPelsHeight);

		img->m_pf = R9_PF_RGB;
		img->m_width = irect.Width();
		img->m_height = irect.Height();
		if(!R9_ImgCreate(img)) return false;

		HDC hdc = GetDC(NULL);
		if(!hdc) return false;
		for(int y=0;y<irect.Height();y++)
		{
			for(int x=0;x<irect.Width();x++)
			{
				dword dw = GetPixel(hdc,irect.p1.x+x,irect.p1.y+y);
				R9_ImgSetColor(img,x,y,dw);
			}
		}
		ReleaseDC(NULL,hdc);
	}
	else // from backbuffer
	{
		iRect irect = (rect) ? *rect : iRect(0,0,GetWidth(),GetHeight());

		img->m_pf = R9_PF_RGB;
		img->m_width = irect.Width();
		img->m_height = irect.Height();
		if(!R9_ImgCreate(img)) return false;

		m_glPixelStorei(GL_PACK_ALIGNMENT, 1);
		m_glReadPixels( irect.p1.x, GetHeight()-irect.p2.y, irect.Width(), irect.Height(), GL_RGB, GL_UNSIGNED_BYTE, img->m_data );
		R9_ImgFlipV(img);
	}

	R9_ImgFlipRGB(img);
	return true;
}

bool r9RenderGL::CopyTargetToImage( R9TEXTURE target, r9Img* img, const iV2 &p, const iV2 & sz)
{
	// temp image to read target data into
	r9Img imgtmp;
	imgtmp.m_pf = R9_PF_RGB;
	imgtmp.m_width = target->realwidth;
	imgtmp.m_height = target->realheight;
	if(!R9_ImgCreate(&imgtmp)) return false;
	SetTexture(target);
	m_glPixelStorei(GL_PACK_ALIGNMENT, 1);
	m_glGetTexImage(GL_TEXTURE_2D,0,GL_RGB,GL_UNSIGNED_BYTE,imgtmp.m_data);
	R9_ImgFlipRGB(&imgtmp);
	//R9_ImgSaveFile(sprint("map_%02i_%02i.png",y/h,x/w),&imgtmp); // test

	// bitblt
	int wc = sz.x; if(p.x+wc>img->m_width)	wc=img->m_width-p.x;
	int hc = sz.y; if(p.y+hc>img->m_height)	hc=img->m_height-p.y;
	if(!R9_ImgBitBltSafe(&imgtmp,0,0,wc,hc,img,p.x,p.y)) return false;

	R9_ImgDestroy(&imgtmp);
	return true;
}

///////////////////////////////////////////////////////////////////////////////////////////////////
// PRIVATE
///////////////////////////////////////////////////////////////////////////////////////////////////
BOOL r9RenderGL::CreateRenderWindow()
{
	HWND hwnd_parent = E9_GetHWND();
	if(!hwnd_parent) return FALSE;
	RECT rect_parent;
	GetClientRect(hwnd_parent,&rect_parent);

	// register child window
	WNDCLASSEX wcex;
	ZeroMemory(&wcex, sizeof(wcex));
	wcex.cbSize			= sizeof(WNDCLASSEX);
	wcex.style			= CS_HREDRAW | CS_VREDRAW | CS_DBLCLKS;
	wcex.lpfnWndProc	= (WNDPROC)WndProc;
	wcex.cbClsExtra		= 0;
	wcex.cbWndExtra		= 0;
	wcex.hInstance		= E9_GetHINSTANCE();
	wcex.hIcon			= NULL;	// let the user set the icon later
	wcex.hCursor		= NULL; // LoadCursor(NULL, IDC_ARROW);
	wcex.hbrBackground	= NULL;
	wcex.lpszMenuName	= NULL;
	wcex.lpszClassName	= "E9_RNDCLASS";
	wcex.hIconSm		= NULL;	// use small icon from default icon
	BOOL ok = RegisterClassEx(&wcex);
	if(!ok) { dlog(LOGERR, L"APP: failed to register render window class (%i).\n",GetLastError()); return FALSE; }

	// create child window
	int style = WS_CHILD | WS_VISIBLE;
	int width = rect_parent.right-rect_parent.left;
	int height= rect_parent.bottom-rect_parent.top;
	RECT rec = {0,0,width,height};
	AdjustWindowRectEx( &rec, style, FALSE, 0 );
	m_hwnd = CreateWindowEx(	0, 
								"E9_RNDCLASS", 
								"", 
								style,
								rec.left, rec.top, rec.right-rec.left, rec.bottom-rec.top, 
								hwnd_parent, NULL, E9_GetHINSTANCE(), 
								NULL );
	if(m_hwnd==NULL) { dlog(LOGERR, L"RENDER: failed to create render window (%i).\n",GetLastError()); return FALSE; }

	return TRUE;

}

BOOL r9RenderGL::DestroyRenderWindow()
{
	if(m_hwnd) 
	{ 
		DestroyWindow(m_hwnd); 
		m_hwnd=NULL; 
	}
	if(!UnregisterClass("E9_RNDCLASS",E9_GetHINSTANCE())) { dlog(LOGERR, L"RENDER: can't unregister window class."); }
	return TRUE;
}

LRESULT	CALLBACK r9RenderGL::WndProc( HWND hwnd, UINT msg, WPARAM wParam, LPARAM lParam )
{
	return DefWindowProc( hwnd, msg, wParam, lParam );
}

void r9RenderGL::PrepareParentWindow()
{
	HWND hwnd = E9_GetHWND();
	if(!hwnd) return;
	if( m_cfg.windowed )
	{
		int scrw = sys_desktopwidth();
		int scrh = sys_desktopheight();
		BOOL fulldesktop = (m_cfg.width==scrw) || (m_cfg.height==scrh);
		int cx = (sys_desktopwidth()-m_cfg.width) / 2;
		int cy = (sys_desktopheight()-m_cfg.height) / 2;
		RECT rec = {cx,cy,cx+m_cfg.width,cy+m_cfg.height};
		long style = fulldesktop ? (WS_POPUP|WS_SYSMENU) : (WS_OVERLAPPEDWINDOW & ~(WS_MAXIMIZEBOX|WS_SIZEBOX));
		style |= CS_OWNDC;
		AdjustWindowRectEx( &rec, style, FALSE, 0 );
		MoveWindow( hwnd, rec.left, rec.top, rec.right-rec.left, rec.bottom-rec.top, TRUE );
		SetWindowLong( hwnd, GWL_STYLE, style );
		SetWindowPos( hwnd, HWND_NOTOPMOST,
                      rec.left, rec.top, rec.right-rec.left, rec.bottom-rec.top,
                      SWP_SHOWWINDOW|SWP_FRAMECHANGED );
	}
	else
	{
		RECT rec = {0,0,m_cfg.width,m_cfg.height};
		long style = WS_POPUP|WS_SYSMENU|WS_VISIBLE;
		style |= CS_OWNDC;
		MoveWindow( hwnd, rec.left, rec.top, rec.right-rec.left, rec.bottom-rec.top, TRUE );
		SetWindowLong( hwnd, GWL_STYLE, style );
		SetWindowPos( hwnd, HWND_TOPMOST,
                      rec.left, rec.top, rec.right-rec.left, rec.bottom-rec.top,
                      SWP_SHOWWINDOW|SWP_FRAMECHANGED );
	}
	RECT r;
	GetWindowRect(hwnd,&r);
	dlog(LOGRND, L"window size %ix%i\n",r.right-r.left,r.bottom-r.top);
}

BOOL r9RenderGL::GL_CreateDevice()
{
	assert(!m_hrc);

	PIXELFORMATDESCRIPTOR pfd =
	{
		sizeof(PIXELFORMATDESCRIPTOR),				// Size Of This Pixel Format Descriptor
		1,											// Version Number
		PFD_DRAW_TO_WINDOW |						// Format Must Support Window
		PFD_SUPPORT_OPENGL |						// Format Must Support OpenGL
		PFD_DOUBLEBUFFER,							// Must Support Double Buffering
		PFD_TYPE_RGBA,								// Request An RGBA Format
		m_cfg.bpp,									// Select Our Color Depth
		0, 0, 0, 0, 0, 0,							// Color Bits Ignored
		0,											// No Alpha Buffer
		0,											// Shift Bit Ignored
		0,											// No Accumulation Buffer
		0, 0, 0, 0,									// Accumulation Bits Ignored
		0,											// Depth Buffer
		0,											// No Stencil Buffer
		0,											// No Auxiliary Buffer
		PFD_MAIN_PLANE,								// Main Drawing Layer
		0,											// Reserved
		0, 0, 0										// Layer Masks Ignored
	};

	// change mode to fullscreen if required
	if(!m_cfg.windowed)
	{
		DEVMODE devmode;
		memset(&devmode,0,sizeof(DEVMODE));
		devmode.dmSize				= sizeof(DEVMODE);
		devmode.dmPelsWidth			= m_cfg.width;
		devmode.dmPelsHeight		= m_cfg.height;
		devmode.dmBitsPerPel		= m_cfg.bpp;
		devmode.dmDisplayFrequency	= m_cfg.refresh;
		devmode.dmFields			= DM_BITSPERPEL|DM_PELSWIDTH|DM_PELSHEIGHT|DM_DISPLAYFREQUENCY;
		if( ChangeDisplaySettings(&devmode,CDS_FULLSCREEN)!=DISP_CHANGE_SUCCESSFUL)
		{
			R9_LOGERROR("fullscreen not supported for this mode."); 
			return FALSE; 
		}
	}

	GLuint pfgl;
	int glverhi=1,glverlo=1;
	char* glver;

	// get hdc
	m_hdc=GetDC(m_hwnd);
	if(!m_hdc) { R9_LOGERROR("can't create GL device context."); goto error; }

	// choose and set pixel format for hdc
	pfgl = ChoosePixelFormat(m_hdc,&pfd);
	if(!pfgl) {R9_LOGERROR("can't find a suitable pixel format."); goto error; }
	if(!DescribePixelFormat(m_hdc,pfgl,sizeof(PIXELFORMATDESCRIPTOR),&pfd)) { R9_LOGERROR("can't describe the pixel format."); goto error; }
	if(m_cfg.bpp != pfd.cColorBits) { R9_LOGERROR("choosed different pixel format."); goto error; }
	if(!SetPixelFormat(m_hdc,pfgl,&pfd)) { R9_LOGERROR("can't set the pixel format."); goto error; }

	// create and activate rendering context
	m_hrc = m_wglCreateContext(m_hdc);
	if(!m_hrc) { R9_LOGERROR("can't create a GL rendering context."); goto error; }
	if(!m_wglMakeCurrent(m_hdc,m_hrc)) { R9_LOGERROR("can't activate the GL rendering context."); goto error; }

	// get caps
	glver = (char*)m_glGetString(GL_VERSION);
	if(glver) sscanf(glver,"%i.%i",&glverhi,&glverlo);
	if(glverhi*10+glverlo>=13) m_caps.m_texture_env_combine = TRUE; // available from 1.3

	return TRUE;

	error:
	if(!m_cfg.windowed) ChangeDisplaySettings(NULL,0);
	if(m_hrc) { m_wglMakeCurrent(NULL,NULL); m_wglDeleteContext(m_hrc); m_hrc=NULL; }
	if(m_hdc) { ReleaseDC(m_hwnd,m_hdc); m_hdc=NULL; }
	return FALSE;
}

BOOL r9RenderGL::GL_BatchCreate()
{
	m_batchbuffer = (r9Vertex*)malloc(R9_BATCHSIZE_GL*sizeof(r9Vertex));
	memset(m_batchbuffer,0,R9_BATCHSIZE_GL*sizeof(r9Vertex)); // clear
	return TRUE;
}

///////////////////////////////////////////////////////////////////////////////////////////////////
