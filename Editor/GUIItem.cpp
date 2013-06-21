//////////////////////////////////////////////////////////////////////////////////////////////////
// GUIItem.cpp
//////////////////////////////////////////////////////////////////////////////////////////////////
#include "stdafx.h"
#include "GUIItem.h"
#include "GUI.h"
#include "E9String.h"

//////////////////////////////////////////////////////////////////////////////////////////////////
// cGUIItem
//////////////////////////////////////////////////////////////////////////////////////////////////
cGUIItem::cGUIItem() : id(), style(), hidden(), disable(), txtAlign(), 
	txtColor(GUICOLOR_TEXT), 
	txtOffset(6), 
	img0(-1), img1(-1), 
	imgColor(0xffffffff), 
	imgAlign(GUIALIGN_CENTERXY),
	mode(),
	cmdActionParam(),
	value(),
	group(),
	m_dlg()
{
	color[0] = GUICOLOR_GUI;
	color[1] = color[2] = color[3] = 0;
}

cGUIItem::~cGUIItem()
{
}


void cGUIItem::Build()
{
}

void cGUIItem::Update()
{
	RECT rc;
	GetScrRect(rc);
	m_mousein = INRECT( g_gui->m_mousex, g_gui->m_mousey, rc);
	if(m_mousein)
	{
		cmdActionParam = 0;
		if(I9_GetKeyDown(I9_MOUSE_B1))	cmdActionParam = 1;
		if(I9_GetKeyDown(I9_MOUSE_B2))	cmdActionParam = 2;
		if(cmdActionParam != 0)		Action();
	}
}

void cGUIItem::Draw()
{
	RECT rc; 
	GetScrRect(rc);

	// background
	if( style & GUISTYLE_BACKGR )
		GUIDrawBar( rc.left, rc.top, rc.right, rc.bottom, color[0]);
	else
	if( style & GUISTYLE_GRADIENT )
		GUIDrawGradient( rc.left, rc.top, rc.right, rc.bottom, color[0], color[1]);
	
	// image
	GUIDrawImg( rc.left, rc.top, rc.right, rc.bottom, img0, imgColor, imgAlign);

	// text
	GUIDrawText( rc.left, rc.top, rc.right, rc.bottom, txt.c_str(), txtColor, txtAlign, txtOffset);
	
	// border
	if( style & GUISTYLE_BORDER )
		GUIDrawRect( rc.left, rc.top, rc.right, rc.bottom, color[2]);
	else
	if( style & GUISTYLE_BORDER3D )
		GUIDrawRect3D( rc.left, rc.top, rc.right, rc.bottom, color[2], style & GUISTYLE_PRESSED );

}

//////////////////////////////////////////////////////////////////////////////////////////////////
// Utils
//////////////////////////////////////////////////////////////////////////////////////////////////

void cGUIItem::GetScrRect( RECT &rc )
{
	rc = rect;
	if(m_dlg)
	{
		rc.left	  += m_dlg->rect.p1.x;
		rc.top	  += m_dlg->rect.p1.y;
		rc.right  += m_dlg->rect.p1.x;
		rc.bottom += m_dlg->rect.p1.y;
	}
}


int cGUIItem::SetParent( cGUIDlg* dlg)
{
	assert(m_dlg==NULL); 
	if(dlg!=NULL) return dlg->ItemAdd(this);
	return -1;
}

void cGUIItem::Capture( bool on )
{
	if(on)
	{
		if(g_gui->m_capture==NULL) 
			g_gui->m_capture = this;	
	}
	else
	{
		if(g_gui->m_capture==this) 
			g_gui->m_capture = NULL;
	}
}

BOOL cGUIItem::IsCaptured()
{
	return (g_gui->m_capture==this);
}


void cGUIItem::Select()	
{
	g_gui->m_lastdlg = g_gui->DlgFind(m_dlg);
	g_gui->m_lastitem = m_dlg->ItemFind(this);
}


void cGUIItem::Action()
{
	if(cmdAction.empty()) return;
	Select();	
	g_gui->ScriptPrologDo(cmdAction);
}




//////////////////////////////////////////////////////////////////////////////////////////////////
// cGUITitle
//////////////////////////////////////////////////////////////////////////////////////////////////
cGUITitle::cGUITitle()
{
	value = 1;
	m_movex = m_movey = 0;
}

cGUITitle::~cGUITitle()
{
}

void cGUITitle::Update()
{
	RECT rc;
	GetScrRect(rc);
	m_mousein = INRECT( g_gui->m_mousex, g_gui->m_mousey, rc);

	if(I9_GetKeyDown(I9_MOUSE_B1))
	{
		if(m_mousein)
		{
			m_movex = g_gui->m_mousex-rc.left;
			m_movey = g_gui->m_mousey-rc.top;
			Capture(true);
		}
	}
	else
	if(!I9_GetKeyValue(I9_MOUSE_B1))
	{
		if(IsCaptured()) 
		{ 
			Capture(false);
		}
	}

	if(IsCaptured() && m_dlg) // move parent dialog
	{
		iV2 p(g_gui->m_mousex, g_gui->m_mousey);
		if(p.x < 0) p.x = 0;
		if(p.x > R9_GetWidth()-1) p.x = R9_GetWidth()-1;
		if(p.y < 0) p.y = 0;
		if(p.y > R9_GetHeight()-1) p.y = R9_GetHeight()-1;
		p -= iV2(m_movex, m_movey);
		p -= rect.p1;
		m_dlg->rect = iRect(p, p + m_dlg->rect.Size());
	}
	
}


//////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////
