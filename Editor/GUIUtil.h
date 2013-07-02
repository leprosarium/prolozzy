//////////////////////////////////////////////////////////////////////////////////////////////////
// GUIUtil.h
//////////////////////////////////////////////////////////////////////////////////////////////////
#ifndef __GUIUTIL_H__
#define __GUIUTIL_H__

#include "E9System.h"
#include "E9Math.h"
#include "I9Input.h"
#include "R9Font.h"
#include "R9TexturePool.h"
#include "GUIDef.h"

inline void BEEP_OK() { MessageBeep(MB_OK); }
inline void BEEP_ERROR() { MessageBeep(MB_ICONHAND); }

void GUIInitResources();	// init static gui resources (like the dot texture)
void GUIDoneResources();	// done static gui resources

dword GUIColorShift(dword color, int delta);
inline void GUIDrawLine (const iV2 & p1, const iV2 & p2, dword color)
{
	R9_DrawLine(p1, p2, color);
}

inline void GUIDrawHLine (const iV2 & p, int x2, dword color)
{
	R9_DrawBar(fRect(p.x, p.y, x2, p.y + 1), color);
} 

inline void GUIDrawVLine (const iV2 & p, int y2, dword color)
{
	R9_DrawBar(fRect(p.x, p.y, p.x + 1, y2), color);
} 

inline void GUIDrawRect(const iRect & r, dword color)
{
	if(r.p1.x == r.p2.x || r.p1.y==r.p2.y) return;
	GUIDrawHLine(r.p1, r.p2.x, color);
	GUIDrawHLine(iV2(r.p1.x, r.p2.y - 1), r.p2.x, color);
	GUIDrawVLine(iV2(r.p1.x, r.p1.y + 1), r.p2.y - 1, color);
	GUIDrawVLine(iV2(r.p2.x - 1, r.p1.y + 1), r.p2.y - 1, color);
}

inline void GUIDrawRect3D(const iRect & r, dword color, BOOL pressed)
{
	dword c1 = GUIColorShift(color,pressed?-GUICOLORSHIFT:GUICOLORSHIFT);
	dword c2 = GUIColorShift(color,pressed?GUICOLORSHIFT:-GUICOLORSHIFT);
	GUIDrawHLine(r.p1, r.p2.x, c1);
	GUIDrawHLine(iV2(r.p1.x, r.p2.y - 1), r.p2.x, c2);
	GUIDrawVLine(iV2(r.p1.x, r.p1.y + 1), r.p2.y - 1, c1);
	GUIDrawVLine(iV2(r.p2.x - 1, r.p1.y + 1), r.p2.y-1, c2);
}
inline void GUIDrawBar(const iRect & r, dword color )
{
	R9_DrawBar( fRect(r), color);
}

void GUIDrawGradient(const iRect & r, dword color1, dword color2);
void GUIDrawRectDot(const iRect & r, dword color);
void GUIDrawText(const iRect & r, const std::string & text, dword color, int align = GUIALIGN_CENTERXY, int offset = 0);
void GUIDrawImg(const iRect & r, int img, dword color = 0xffffffff, int align = GUIALIGN_CENTERXY);
iV2 Align(const iRect & r, const iV2 & sz, int align, int offset);

BOOL WinDlgOpenFile(LPWSTR filename, LPWSTR ext, int mode); // filenname must have at least 256 chars; mode 0=open 1=save
BOOL WinDlgOpenFolder(LPWSTR foldername); // foldername muse have at least 256 chars
BOOL WinDlgOpenColor(dword* color, BOOL extended=FALSE);

#endif
//////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////
