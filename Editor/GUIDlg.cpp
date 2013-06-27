//////////////////////////////////////////////////////////////////////////////////////////////////
// GUIDlg.cpp
//////////////////////////////////////////////////////////////////////////////////////////////////
#include "stdafx.h"
#include <algorithm>
#include "GUIDlg.h"
#include "E9String.h"

//////////////////////////////////////////////////////////////////////////////////////////////////
// cGUIDlg
//////////////////////////////////////////////////////////////////////////////////////////////////
cGUIDlg::cGUIDlg() : id(), hidden(), disable(), modal(), closeOut(), closeRet(), m_mustclose()
{
}

cGUIDlg::~cGUIDlg()
{
}

void cGUIDlg::Update()
{
	bool m_mousein = rect.IsInside(g_gui->m_mouse);
	Items.Update();

	// test key
	Keys.Test(this, m_mousein);

	// test click
	if( closeOut && I9_GetKeyDown(I9_MOUSE_B1) && !m_mousein )
		Close(-1);

}

cGUIItem * cGUIDlg::Add(const char * classname)
{
	cGUIItem * i = GUICreateItem(this, classname);
	if(i)
		Items.Add(i);
	return i;
}


void cGUIDlg::Close(int ret)
{
	// select this dialog when we call Close
	g_gui->SetLast(this);
	closeRet = ret ;
	if(!closeCmd.empty())
		g_gui->ScriptPrologDo(closeCmd);		
	
	g_gui->SetLast();
	m_mustclose = true;
	// from here the dialog should not be used anymore (it will be destroyed asp)
}

bool DlgKey::Active() const
{
	if(!I9_GetKeyDown(m_key))
		return false;
	int flags = 0;
	if( I9_GetKeyValue(I9K_LSHIFT) || I9_GetKeyValue(I9K_RSHIFT) )		flags |= GUIKEYFLAG_SHIFT;
	if( I9_GetKeyValue(I9K_LCONTROL) || I9_GetKeyValue(I9K_RCONTROL) )	flags |= GUIKEYFLAG_CTRL;
	if( I9_GetKeyValue(I9K_LALT) || I9_GetKeyValue(I9K_RALT) )			flags |= GUIKEYFLAG_ALT;
	return m_flags == flags;
}


void DlgKeys::Test(cGUIDlg * dlg)
{
	auto k = std::find_if(begin(), end(), [](const DlgKey & k) { return k.Active(); });
	if(k != end())
	{
		g_gui->SetLast(dlg);
		g_gui->ScriptPrologDo(k->cmd);	
	}


}

//////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////
