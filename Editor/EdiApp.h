//////////////////////////////////////////////////////////////////////////////////////////////////
// EdiApp.h
//////////////////////////////////////////////////////////////////////////////////////////////////
#ifndef __EDIAPP_H__
#define __EDIAPP_H__

#include "E9System.h"
#include "App.h"
#include "EdiDef.h"
#include "EdiPaint.h"
#include "EdiTool.h"

//////////////////////////////////////////////////////////////////////////////////////////////////
// defines
//////////////////////////////////////////////////////////////////////////////////////////////////

// tools
#define TOOL_PAINT	0
#define TOOL_EDIT	1
#define TOOL_MAX	2

// layers
#define LAYER_MAX	8

// edi data scripted access
#define EDI_TOOL			0					// current tool
#define EDI_AXES			1					// show axes
#define EDI_SNAP			2					// snap to grid
#define EDI_GRID			3					// show grid
#define EDI_GRIDSIZE		4					// grid size
#define EDI_SCRW			5					// map width
#define EDI_SCRH			6					// map height
#define EDI_MAPW			7					// map width
#define EDI_MAPH			8					// map height
#define EDI_ROOMW			9					// room width
#define EDI_ROOMH			10					// room height
#define EDI_ROOMGRID		11					// room grid
#define EDI_CAMX			12					// camera y
#define EDI_CAMY			13					// camera x
#define EDI_AXEX			14					// axe y (currsor)
#define EDI_AXEY			15					// axe x (currsor)
#define EDI_ZOOM			16					// camera zoom scale
#define EDI_SELECT			17					// count selection
#define EDI_BRUSHRECT		18					// draw brush rects, for blind visibility debug

#define EDI_COLOR			20					// dummy
#define EDI_COLORBACK1		20					// editor background
#define EDI_COLORBACK2		21					// editor background sliders
#define EDI_COLORGRID1		22					// grid
#define EDI_COLORGRID2		23					// room grid
#define EDI_COLORGRID3		24					// axes
#define EDI_COLORMAP		25					// map background
#define EDI_COLORMAX		26					// dummy

// tile scripted access 
#define TILE_ID				0
#define TILE_W				1
#define TILE_H				2
#define TILE_FRAMES			3
#define	TILE_NAME			4

// undo
#define UNDOOP_NONE		0
#define UNDOOP_ADD		1
#define UNDOOP_DEL		2

class Editor : public App
{
	void Init();
	bool InitApp();
	bool InitFiles();
	bool InitInput();
	bool InitVideo();
	bool Update();
	void Draw();
	void Done();

public:
	int	m_mscrollx;								// must scroll x msg (0=no)
	int m_mscrolly;								// must scroll y msg (0=no)

	Editor(HINSTANCE hinstance, LPCTSTR cmdline);
	~Editor();

	void OnActivate(bool);
	bool OnClose();

	void OnMsg(UINT msg, WPARAM wparam, LPARAM lparam);

	void DropFile( LPCWSTR filepath );				// called when file is dropped
	void Scroll( int dx, int dy );				// called on scroll messages
	static void HandleReset();							// handle render device reset; used to repaint map's render target

	// utils
	int GetMouseX();
	int	GetMouseY();
	int GetScrW() const { return R9_GetWidth(); }
	int GetScrH() const { return R9_GetHeight(); }
	int GetAxeX() const { return m_tool[m_toolcrt]->m_ax; }
	int GetAxeY() const { return m_tool[m_toolcrt]->m_ay; }

	// settings
	int			m_exit;									// exit app!
	int			m_axes;									// draw axes
	int			m_snap;									// snap mode to tile grid
	int			m_grid;									// grid visible
	int			m_gridsize;								// grid size
	dword		m_color[EDI_COLORMAX-EDI_COLOR];		// editor colors
	dword		GetColor( int idx )						{ return m_color[idx-EDI_COLOR]; }
	void		WaitCursor( BOOL on );					// set cursor wait on/off


		// tools
		void		ToolSet				( int tool );		// set current tool
		int			m_toolcrt;								// current tool
		cEdiTool*	m_tool[TOOL_MAX];						// editor tools 
		tBrush		m_brush;								// current tool brush

		// layers
	int			LayerGet			( int layer )				{ return m_layer[layer]; }
	void		LayerSet			( int layer, int status )	{ m_layer[layer]=status; }
	int			LayerActive			()							{ for(int i=LAYER_MAX-1;i>=0;i--) if(m_layer[i]==2) return i; return -1; }
	int			m_layer[LAYER_MAX];						// layers states 0=invisible, 1=visibil, 2=active)

protected:
		void		DrawStats			();					// draw stats info
		BOOL		m_drawstats;

public: // access
	static	Editor * app;
};				

#endif
//////////////////////////////////////////////////////////////////////////////////////////////////
