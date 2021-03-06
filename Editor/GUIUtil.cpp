//////////////////////////////////////////////////////////////////////////////////////////////////
// GUIUtil.cpp
//////////////////////////////////////////////////////////////////////////////////////////////////
#include "stdafx.h"
#include "GUIUtil.h"
#include "GUI.h"
#include "App.h"
#include "shlobj.h"

R9TEXTURE g_texdot = NULL;

void GUIInitResources()
{
	assert(R9_IsReady());
	// create chess texture for dot lines
	r9Img img;
	img.m_width = 64;
	img.m_height = 64;
	img.m_pf = R9_PF_ARGB;
	if(!R9_ImgCreate(&img)) return;
	for(int y=0;y<img.m_height;y++)
	{
		for(int x=0;x<img.m_width;x++)
		{
			BOOL chess = ( (x/8) + (y/8)%2 ) % 2;
			((dword*)img.m_data)[y*img.m_width+x] = chess ? 0xffffffff : 0xff000000;
		}
	}
	g_texdot = R9_TextureCreate(&img);
	R9_ImgDestroy(&img);
}

void GUIDoneResources()
{
	if(g_texdot) R9_TextureDestroy(g_texdot);
	g_texdot = NULL;
}

dword GUIColorShift( dword color, int delta )
{
	int r = (color>>16) & 0xff;
	int g = (color>>8) & 0xff;
	int b = color & 0xff;
	r += delta; 
	if(r>255) r = 255; 
	if(r<0)	  r = 0;
	g += delta; 
	if(g>255) g = 255; 
	if(g<0)	  g = 0;
	b += delta; 
	if(b>255) b = 255; 
	if(b<0)	  b = 0;
	return ( (color & 0xff000000) | (r<<16) | (g<<8) | b );
}

void GUIDrawGradient( const iRect & r, dword color1, dword color2 )
{
	if(R9_GetTexture()!=NULL) R9_SetTexture(NULL);

	r9Vertex vx[6];
	memset(vx,0,sizeof(vx));

	vx[0].x = (float)r.p1.x;
	vx[0].y = (float)r.p1.y;
	vx[0].color = color1;

	vx[1].x = (float)r.p2.x;
	vx[1].y = (float)r.p1.y;
	vx[1].color = color2;

	vx[2].x = (float)r.p2.x;
	vx[2].y = (float)r.p2.y;
	vx[2].color = color2;

	vx[3].x = (float)r.p1.x;
	vx[3].y = (float)r.p1.y;
	vx[3].color = color1;

	vx[4].x = (float)r.p2.x;
	vx[4].y = (float)r.p2.y;
	vx[4].color = color2;

	vx[5].x = (float)r.p1.x;
	vx[5].y = (float)r.p2.y;
	vx[5].color = color1;

	R9_Push(vx, 6, Primitive::Triangle);
}

void GUIDrawLineDot( int x1, int y1, int x2, int y2, dword color, float dx, float dy )
{
	if(!g_texdot) return;
	if(R9_GetTexture()!=g_texdot) R9_SetTexture(g_texdot);
	r9Vertex vx[2];
	int tw = g_texdot->width;
	int th = g_texdot->height;
	vx[0].x		= (float)x1;
	vx[0].y		= (float)y1;
	vx[0].color	= color;
	vx[0].u		= ((float)x1+dx) / (float)tw;
	vx[0].v		= ((float)y1+dy) / (float)th;
	vx[1].x		= (float)x2;
	vx[1].y		= (float)y2;
	vx[1].color	= color;
	vx[1].u		= ((float)x2+dx) / (float)tw;
	vx[1].v		= ((float)y2+dy) / (float)th;

	R9_Push(vx, 2, Primitive::Line);
}

void GUIDrawRectDot( const iRect & r, dword color )
{
	float d = (float)(sys_gettickcount()%1024); // offsets not too big
	GUIDrawLineDot( r.p1.x, r.p1.y,	 r.p2.x, r.p1.y,	  color, d, 0.0f );
	GUIDrawLineDot( r.p1.x, r.p1.y,	 r.p1.x, r.p2.y,	  color, 0.0f, d );
	GUIDrawLineDot( r.p2.x-1, r.p1.y, r.p2.x-1, r.p2.y, color, 0.0f, d );
	GUIDrawLineDot( r.p1.x,r.p2.y-1, r.p2.x,r.p2.y-1, color, d, 0.0f );
}

iV2 Align(const iRect & r, const iV2 & sz, int align, int offset)
{
	iV2 p = r.p1;
	if((align & GUIALIGN_CENTERX) == GUIALIGN_CENTERX)	
		p.x = (r.p1.x + r.p2.x - sz.x) / 2;
	else if(align & GUIALIGN_LEFT)		
		p.x = r.p1.x + offset;
	else if(align & GUIALIGN_RIGHT)
		p.x = r.p2.x - sz.x - offset;
	if((align & GUIALIGN_CENTERY) == GUIALIGN_CENTERY)	
		p.y = (r.p1.y + r.p2.y - sz.y) / 2;
	else if(align & GUIALIGN_TOP)
		p.y = r.p1.y;
	else if(align & GUIALIGN_BOTTOM)
		p.y = r.p2.y - sz.y;
	return p;
}


void GUIDrawText( const iRect & r, const std::wstring & text, dword color, int align, int offset )
{
	if(text.empty()) return;
	if(color==0) return;
	dword oldcolor = g_gui->m_font->GetColor(); 
	g_gui->m_font->SetColor(color);
	fV2 sz = g_gui->m_font->GetTextBox(text);
	iV2 p = Align(r, iV2(sz), align, offset);
	
	// clip not needed
	g_gui->m_font->Print(p, text);  

	g_gui->m_font->SetColor(oldcolor);
}

void GUIDrawImg(const iRect & r, int img, dword color, int align )
{
	if(img==-1) return;
	if(color==0) return;
	R9TEXTURE tex = g_gui->m_texturepool.Get(img);
	if(!tex) return;
	iV2 sz(tex->width, tex->height);
	iV2 p = Align(r, sz, align, 0);

	R9_SetBlend(Blend::Alpha);
	R9_DrawSprite(p, fRect(0, sz),tex,color);
}



//////////////////////////////////////////////////////////////////////////////////////////////////
BOOL WinDlgOpenFile( LPWSTR filename, LPWSTR ext, int mode )
{
	if(mode!=0 && mode!=1) return FALSE;
	
	dword flags=0;
	WCHAR title[32]; title[0]=0;

	if(mode==0)
	{
		wcscpy(title, L"Open File");
		flags |= OFN_FILEMUSTEXIST;
		flags |= OFN_NOCHANGEDIR;
		flags |= OFN_PATHMUSTEXIST;
	} 
	else
	if(mode==1)
	{
		flags |= OFN_NOCHANGEDIR;
		flags |= OFN_OVERWRITEPROMPT;
		wcscpy(title, L"Save File");
	}
	// other flags ? OFN_EXTENSIONDIFFERENT

	WCHAR filter[64];
	if(ext)
	{
		size_t extsize = wcslen(ext);
		wcscpy(filter, L"*.");
		wcscat(filter, ext);
		wcscpy(filter + extsize + 3, L"*.");
		wcscat(filter + extsize + 3, ext);
		filter[(extsize+3)*2] = 0;
	}

	OPENFILENAMEW ofn;
	memset(&ofn,0,sizeof(ofn));
	ofn.lStructSize			= sizeof(ofn);
	ofn.hwndOwner			= App::Wnd();
	ofn.hInstance			= App::Instance();
	ofn.lpstrFilter			= ext ? filter : NULL;
	ofn.lpstrCustomFilter	= NULL;
	ofn.nMaxCustFilter		= 0;
	ofn.nFilterIndex		= 0;
	ofn.lpstrFile			= filename; // in+out
	ofn.nMaxFile			= 256;
	ofn.lpstrFileTitle		= NULL;
	ofn.nMaxFileTitle		= 0;
	ofn.lpstrInitialDir		= NULL;
	ofn.lpstrTitle			= title;
	ofn.Flags				= flags;
	ofn.nFileExtension		= 0; //out
	ofn.lpstrDefExt			= ext;

	BOOL ok;
	if(mode==0)
		ok = GetOpenFileNameW( &ofn );
	else
	if(mode==1)
		ok = GetSaveFileNameW( &ofn );
	return ok;
}


//////////////////////////////////////////////////////////////////////////////////////////////////
BOOL WinDlgOpenFolder( LPWSTR foldername )
{
	foldername[0]=0;

	BROWSEINFOW bi;
	bi.hwndOwner = App::Wnd();
	bi.pidlRoot = NULL;
	bi.pszDisplayName = foldername;
	bi.lpszTitle = L"Select folder";
	bi.ulFlags = BIF_DONTGOBELOWDOMAIN | BIF_RETURNFSANCESTORS | BIF_RETURNONLYFSDIRS;
	bi.lpfn = NULL;
	bi.lParam = NULL;
	bi.iImage = 0;

	LPITEMIDLIST pidl;
	pidl=SHBrowseForFolderW(&bi);
	if(pidl==NULL) return 0;
	if(!SHGetPathFromIDListW(pidl, foldername)) return 0;

	size_t s = wcslen(foldername);
	if (s == 0) return 0;
	if (foldername[s - 1] == L'\\') foldername[s-1] = 0;
	return 1;
}

inline dword RGB2BGR(dword argb)
{
	return ((argb & 0xff00ff00) | ((argb & 0x00ff0000) >> 16) | ((argb & 0x000000ff) << 16));
}

//////////////////////////////////////////////////////////////////////////////////////////////////
BOOL WinDlgOpenColor( dword* color, BOOL extended )
{

	static COLORREF custom[16];

	CHOOSECOLOR cc;
	memset(&cc,0,sizeof(cc));
	cc.lStructSize			= sizeof(cc);
	cc.hwndOwner			= App::Wnd();
	cc.hInstance			= NULL;//App()->GetHINSTANCE();
	cc.rgbResult			= RGB2BGR(*color) & 0x00ffffff;
	cc.lpCustColors			= custom;
	cc.Flags				= CC_ANYCOLOR | CC_RGBINIT | (extended ? CC_FULLOPEN : 0);


	BOOL ok = ChooseColor( &cc );
	if( ok ) // keep alpha and reverse red and blue
		*color = (*color & 0xff000000) | RGB2BGR(cc.rgbResult);
	return ok;
}


//////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////
