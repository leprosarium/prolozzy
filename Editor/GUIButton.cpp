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

void cGUIButton::Update()
{
	m_mousein = scrRect().IsInside(g_gui->m_mouse);

	if(I9_GetKeyDown(I9_MOUSE_B1))
	{
		if(m_mousein)
			Capture(true);
	}
	else
	if(!I9_GetKeyValue(I9_MOUSE_B1))
	{
		if(IsCaptured()) 
		{ 
			Capture(false);
			if(m_mousein)
			{
				cmdActionParam = 1;
				Action();
			}
		}
	}

	if(!IsCaptured() && m_mousein && I9_GetKeyDown(I9_MOUSE_B2))
	{
		cmdActionParam = 2;
		Action();
	}

	if( g_gui->m_capture == this ) 
		style |= GUISTYLE_PRESSED;
	else
		style &= ~GUISTYLE_PRESSED;
}

void cGUIButton::Draw()
{
	iRect rc = scrRect();

	// background
	if( style & GUISTYLE_BACKGR )
		GUIDrawBar( rc.p1.x, rc.p1.y, rc.p2.x, rc.p2.y, color[0] );
	else
	if( style & GUISTYLE_GRADIENT )
		GUIDrawGradient( rc.p1.x, rc.p1.y, rc.p2.x, rc.p2.y, color[0], color[1] );
	
	// image
	int img = (style & GUISTYLE_PRESSED) ? img1 : img0;
	GUIDrawImg( rc.p1.x, rc.p1.y, rc.p2.x, rc.p2.y, img, imgColor, imgAlign);

	// text
	GUIDrawText( rc.p1.x, rc.p1.y, rc.p2.x, rc.p2.y, txt.c_str(), txtColor, txtAlign, txtOffset);
	
	// border
	if( style & GUISTYLE_BORDER )
		GUIDrawRect( rc.p1.x, rc.p1.y, rc.p2.x, rc.p2.y, color[2]);
	else
	if( style & GUISTYLE_BORDER3D )
		GUIDrawRect3D( rc.p1.x, rc.p1.y, rc.p2.x, rc.p2.y, color[2], style & GUISTYLE_PRESSED );

}

//////////////////////////////////////////////////////////////////////////////////////////////////
// cGUICheck
//////////////////////////////////////////////////////////////////////////////////////////////////

void cGUICheck::Update()
{
	cGUIButton::Update();
	if(value)
		style |= GUISTYLE_PRESSED;
	else
		style &= ~GUISTYLE_PRESSED;
}

void cGUICheck::Action()
{
	value = value != 0 ? 0 : 1;
	cGUIItem::Action();		
}

//////////////////////////////////////////////////////////////////////////////////////////////////
// cGUIRadio
//////////////////////////////////////////////////////////////////////////////////////////////////

void cGUIRadio::Update()
{
	cGUIButton::Update();
	if(value)
		style |= GUISTYLE_PRESSED;
	else
		style &= ~GUISTYLE_PRESSED;
}

void cGUIRadio::Action()
{
	assert(m_dlg!=NULL);
	if(group!=0)
		for(int i=0;i<m_dlg->ItemCount();i++)
			if(m_dlg->ItemGet(i)->group == group)
				m_dlg->ItemGet(i)->value = 0;
	value = 1;
	cGUIItem::Action();		
}


//////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////
