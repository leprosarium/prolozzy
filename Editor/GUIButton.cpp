//////////////////////////////////////////////////////////////////////////////////////////////////
// GUIButton.cpp
//////////////////////////////////////////////////////////////////////////////////////////////////
#include "stdafx.h"
#include "GUIButton.h"
#include "GUI.h"
#include "E9App.h"

//////////////////////////////////////////////////////////////////////////////////////////////////
// cGUIButton
//////////////////////////////////////////////////////////////////////////////////////////////////
cGUIButton::cGUIButton()
{
}

cGUIButton::~cGUIButton()
{
}

void cGUIButton::Update()
{
	RECT rc;
	GetScrRect(rc);
	m_mousein = INRECT( g_gui->m_mousex, g_gui->m_mousey, rc);

	if(I9_GetKeyDown(I9_MOUSE_B1))
	{
		if(m_mousein)
			Capture(TRUE);
	}
	else
	if(!I9_GetKeyValue(I9_MOUSE_B1))
	{
		if(IsCaptured()) 
		{ 
			Capture(FALSE);
			if(m_mousein)
			{
				SetInt(IV_CMDACTIONPARAM,1);
				Action();
			}
		}
	}

	if(!IsCaptured() && m_mousein && I9_GetKeyDown(I9_MOUSE_B2))
	{
		SetInt(IV_CMDACTIONPARAM,2);
		Action();
	}

	int style = GetInt(IV_STYLE);
	if( g_gui->m_capture == this ) 
		style |= GUISTYLE_PRESSED;
	else
		style &= ~GUISTYLE_PRESSED;
	SetInt(IV_STYLE,style);

}

void cGUIButton::Draw()
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
	int img = (style&GUISTYLE_PRESSED) ? GetInt(IV_IMG+1) : GetInt(IV_IMG);
	GUIDrawImg( rc.left, rc.top, rc.right, rc.bottom, img, GetInt(IV_IMGCOLOR), GetInt(IV_IMGALIGN));

	// text
	GUIDrawText( rc.left, rc.top, rc.right, rc.bottom, GetTxt(IV_TXT), GetInt(IV_TXTCOLOR), GetInt(IV_TXTALIGN), GetInt(IV_TXTOFFSET) );
	
	// border
	if( style & GUISTYLE_BORDER )
		GUIDrawRect( rc.left, rc.top, rc.right, rc.bottom, GetInt(IV_COLOR+2) );
	else
	if( style & GUISTYLE_BORDER3D )
		GUIDrawRect3D( rc.left, rc.top, rc.right, rc.bottom, GetInt(IV_COLOR+2), style & GUISTYLE_PRESSED );

}

//////////////////////////////////////////////////////////////////////////////////////////////////
// cGUICheck
//////////////////////////////////////////////////////////////////////////////////////////////////
cGUICheck::cGUICheck()
{
}

cGUICheck::~cGUICheck()
{
}

void cGUICheck::Update()
{
	cGUIButton::Update();
	int style = GetInt(IV_STYLE);
	if(GetInt(IV_VALUE))
		style |= GUISTYLE_PRESSED;
	else
		style &= ~GUISTYLE_PRESSED;
	SetInt(IV_STYLE,style);
}

void cGUICheck::Action()
{
	SetInt( IV_VALUE, (GetInt(IV_VALUE)?0:1) );
	cGUIItem::Action();		
}

//////////////////////////////////////////////////////////////////////////////////////////////////
// cGUIRadio
//////////////////////////////////////////////////////////////////////////////////////////////////
cGUIRadio::cGUIRadio()
{
}

cGUIRadio::~cGUIRadio()
{
}

void cGUIRadio::Update()
{
	cGUIButton::Update();
	int style = GetInt(IV_STYLE);
	if(GetInt(IV_VALUE))
		style |= GUISTYLE_PRESSED;
	else
		style &= ~GUISTYLE_PRESSED;
	SetInt(IV_STYLE,style);
}

void cGUIRadio::Action()
{
	assert(m_dlg!=NULL);
	int group = GetInt(IV_GROUP);
	if(group!=0)
	{
		for(int i=0;i<m_dlg->ItemCount();i++)
		{
			if(m_dlg->ItemGet(i)->GetInt(IV_GROUP) == group)
				m_dlg->ItemGet(i)->SetInt(IV_VALUE, 0);
		}
	}
	SetInt( IV_VALUE, 1 );
	cGUIItem::Action();		
}


//////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////
