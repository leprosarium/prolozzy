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

void cGUIButton::OnUpdate()
{
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
				Action(1);
		}
	}

	if(!IsCaptured() && m_mousein && I9_GetKeyDown(I9_MOUSE_B2))
		Action(2);

	if( g_gui->m_capture == this ) 
		style |= GUISTYLE_PRESSED;
	else
		style &= ~GUISTYLE_PRESSED;
}

void cGUIButton::OnDraw()
{
	iRect rc = scrRect();

	// background
	if( style & GUISTYLE_BACKGR )
		GUIDrawBar(rc, color[0]);
	else
	if( style & GUISTYLE_GRADIENT )
		GUIDrawGradient( rc, color[0], color[1] );
	
	// image
	int img = (style & GUISTYLE_PRESSED) ? img1 : img0;
	GUIDrawImg( rc, img, imgColor, imgAlign);

	// text
	GUIDrawText( rc, txt, txtColor, txtAlign, txtOffset);
	
	// border
	if( style & GUISTYLE_BORDER )
		GUIDrawRect(rc, color[2]);
	else
	if( style & GUISTYLE_BORDER3D )
		GUIDrawRect3D(rc, color[2], style & GUISTYLE_PRESSED );

}

//////////////////////////////////////////////////////////////////////////////////////////////////
// cGUICheck
//////////////////////////////////////////////////////////////////////////////////////////////////

void cGUICheck::OnUpdate()
{
	cGUIButton::OnUpdate();
	if(value)
		style |= GUISTYLE_PRESSED;
	else
		style &= ~GUISTYLE_PRESSED;
}

void cGUICheck::OnAction()
{
	value = value != 0 ? 0 : 1;
}

//////////////////////////////////////////////////////////////////////////////////////////////////
// cGUIRadio
//////////////////////////////////////////////////////////////////////////////////////////////////

void cGUIRadio::OnUpdate()
{
	cGUIButton::OnUpdate();
	if(value)
		style |= GUISTYLE_PRESSED;
	else
		style &= ~GUISTYLE_PRESSED;
}

void cGUIRadio::OnAction()
{
	assert(m_dlg!=NULL);
	if(group)
		for(cGUIItem * i: m_dlg->Items)
			if(i->group == group)
				i->value = 0;
	value = 1;
}


//////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////
