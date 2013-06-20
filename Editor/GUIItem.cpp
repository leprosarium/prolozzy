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
cGUIItem::cGUIItem()
{
	memset(&m_var,0,sizeof(m_var));
	// defaults
	m_var[IV_IMG].m_int = -1;
	m_var[IV_IMG+1].m_int = -1;
	SetInt(IV_COLOR,		GUICOLOR_GUI);
	SetInt(IV_TXTCOLOR,	GUICOLOR_TEXT);
	SetInt(IV_TXTOFFSET, 6 );
	SetInt(IV_IMGCOLOR,	0xffffffff);
	SetInt(IV_IMGALIGN,	GUIALIGN_CENTERXY);
	m_dlg = NULL;
}

cGUIItem::~cGUIItem()
{
	assert(m_dlg==NULL);
	SetTxt(IV_TXT,NULL);		
	SetTxt(IV_CMDACTION,NULL);	
	SetTxt(IV_TOOLTIP,NULL);
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
		SetInt(IV_CMDACTIONPARAM,0);
		if(I9_GetKeyDown(I9_MOUSE_B1))	SetInt(IV_CMDACTIONPARAM,1);
		if(I9_GetKeyDown(I9_MOUSE_B2))	SetInt(IV_CMDACTIONPARAM,2);
		if(GetInt(IV_CMDACTIONPARAM))		Action();
	}
}

void cGUIItem::Draw()
{
	RECT rc; 
	GetScrRect(rc);

	int style = GetInt(IV_STYLE);

	// background
	if( style & GUISTYLE_BACKGR )
		GUIDrawBar( rc.left, rc.top, rc.right, rc.bottom, GetInt(IV_COLOR) );
	else
	if( style & GUISTYLE_GRADIENT )
		GUIDrawGradient( rc.left, rc.top, rc.right, rc.bottom, GetInt(IV_COLOR), GetInt(IV_COLOR+1) );
	
	// image
	GUIDrawImg( rc.left, rc.top, rc.right, rc.bottom, GetInt(IV_IMG), GetInt(IV_IMGCOLOR), GetInt(IV_IMGALIGN));

	// text
	GUIDrawText( rc.left, rc.top, rc.right, rc.bottom, GetTxt(IV_TXT), GetInt(IV_TXTCOLOR), GetInt(IV_TXTALIGN), GetInt(IV_TXTOFFSET) );
	
	// border
	if( style & GUISTYLE_BORDER )
		GUIDrawRect( rc.left, rc.top, rc.right, rc.bottom, GetInt(IV_COLOR+2) );
	else
	if( style & GUISTYLE_BORDER3D )
		GUIDrawRect3D( rc.left, rc.top, rc.right, rc.bottom, GetInt(IV_COLOR+2), style & GUISTYLE_PRESSED );

}

/////////////////////////////////////////////////////////////////////////////////////////////////
// Access
//////////////////////////////////////////////////////////////////////////////////////////////////

void cGUIItem::SetInt( int idx, int val )
{
	assert(0<=idx && idx<IV_MAX);
	m_var[idx].m_int=val;
}

int cGUIItem::GetInt( int idx )
{
	assert(0<=idx && idx<IV_MAX);
	return m_var[idx].m_int;	
}

void cGUIItem::SetTxt( int idx, char* text )
{
	assert(0<=idx && idx<IV_MAX);
	char* sz = m_var[idx].m_str;
	delete [] sz;
	m_var[idx].m_str = sstrdup(text);
}

char* cGUIItem::GetTxt( int idx )
{
	assert(0<=idx && idx<IV_MAX);
	return m_var[idx].m_str;
}

void cGUIItem::SetPoint( int idx, POINT point )
{
	assert(0<=idx && idx<IV_MAX-1);
	m_var[idx+0].m_int = point.x;
	m_var[idx+1].m_int = point.y;
}

POINT cGUIItem::GetPoint( int idx )
{
	assert(0<=idx && idx<IV_MAX-1);
	POINT point;
	point.x = m_var[idx+0].m_int;
	point.y = m_var[idx+1].m_int;
	return point;
}

void cGUIItem::SetRect( int idx, RECT rect )
{
	assert(0<=idx && idx<IV_MAX-3);
	m_var[idx+0].m_int = rect.left;
	m_var[idx+1].m_int = rect.top;
	m_var[idx+2].m_int = rect.right;
	m_var[idx+3].m_int = rect.bottom;
}

RECT cGUIItem::GetRect( int idx )
{
	assert(0<=idx && idx<IV_MAX-3);
	RECT rect;
	rect.left = m_var[idx+0].m_int;
	rect.top = m_var[idx+1].m_int;
	rect.right = m_var[idx+2].m_int;
	rect.bottom = m_var[idx+3].m_int;
	return rect;
}

//////////////////////////////////////////////////////////////////////////////////////////////////
// Utils
//////////////////////////////////////////////////////////////////////////////////////////////////

void cGUIItem::GetScrRect( RECT &rc )
{
	rc = GetRect(IV_RECT);
	cGUIDlg *p = m_dlg;
	if(p!=NULL)
	{
		rc.left	  += p->rect.p1.x;
		rc.top	  += p->rect.p1.y;
		rc.right  += p->rect.p1.x;
		rc.bottom += p->rect.p1.y;
	}
}


int cGUIItem::SetParent( cGUIDlg* dlg)
{
	assert(m_dlg==NULL); 
	if(dlg!=NULL) return dlg->ItemAdd(this);
	return -1;
}

void cGUIItem::Capture( BOOL on )
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
	char* cmd = GetTxt(IV_CMDACTION);
	if(cmd==NULL || cmd[0]==0) return;
	Select();	
	g_gui->ScriptPrologDo(cmd);
}




//////////////////////////////////////////////////////////////////////////////////////////////////
// cGUITitle
//////////////////////////////////////////////////////////////////////////////////////////////////
cGUITitle::cGUITitle()
{
	SetInt(IV_VALUE,1);
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
			Capture(TRUE);
		}
	}
	else
	if(!I9_GetKeyValue(I9_MOUSE_B1))
	{
		if(IsCaptured()) 
		{ 
			Capture(FALSE);
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
		p -= iV2(GetInt(IV_X), GetInt(IV_Y));
		m_dlg->rect = iRect(p, p + m_dlg->rect.Size());
	}
	
}


//////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////
