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
cGUIDlg::cGUIDlg() : id(), hidden(), disable(), modal(), testKey(TestKeyMode::always), closeOut(), closeRet(), m_mustclose(), m_mousein()
{
}

cGUIDlg::~cGUIDlg()
{
	std::for_each(m_item.begin(), m_item.end(), [](cGUIItem *i) { i->m_dlg = NULL; delete i; });
	m_item.clear();
	m_keys.clear();
}

void cGUIDlg::Update()
{
	// mouse in	
	m_mousein = rect.IsInside(g_gui->m_mouse);

	// update children
	std::for_each(m_item.begin(), m_item.end(), [](cGUIItem*i)
	{
		if(!i->disable)
		{
			i->Update();
			if( i->m_mousein && !i->hidden && !i->disable && !i->tooltip.empty())
				g_gui->ToolTip = i->tooltip;
		}
	});

	// test key
	if( (testKey == TestKeyMode::always) || 
		(testKey == TestKeyMode::mousein && m_mousein) )
		TestKey();

	// test click
	if( closeOut && I9_GetKeyDown(I9_MOUSE_B1) && !m_mousein )
		Close(-1);

}

void cGUIDlg::Draw()
{
	std::for_each(m_item.begin(), m_item.end(), [](cGUIItem *i){ if(!i->hidden) i->Draw(); } );
}

void cGUIDlg::Close(int ret)
{
	// select this dialog when we call Close
	g_gui->m_lastdlg = g_gui->DlgFind(this);
	g_gui->m_lastitem = -1;

	closeRet = ret ;
	if(!closeCmd.empty())
		g_gui->ScriptPrologDo(closeCmd);		
	
	g_gui->m_lastdlg = -1;
	g_gui->m_lastitem = -1;
	m_mustclose = true;
	// from here the dialog should not be used anymore (it will be destroyed asp)
}

int cGUIDlg::ItemFind( int id )
{
	auto i = std::find_if(m_item.begin(), m_item.end(), [id](cGUIItem *i){ return i->id == id; });
	return i == m_item.end() ? -1 : i - m_item.begin();
}

int cGUIDlg::ItemFind( cGUIItem* item )
{
	auto i = std::find(m_item.begin(), m_item.end(), item);
	return i == m_item.end() ? -1 : i - m_item.begin();
}

void cGUIDlg::AddKey( int key, int flags, const std::string & cmd )
{
	m_keys.push_back(tDlgKey(key, flags, cmd));
}

void cGUIDlg::TestKey()
{
	auto k = std::find_if(m_keys.begin(), m_keys.end(), [](const tDlgKey & k) -> bool
	{
		if(!I9_GetKeyDown(k.m_key))
			return false;
		int flags = 0;
		if( I9_GetKeyValue(I9K_LSHIFT) || I9_GetKeyValue(I9K_RSHIFT) )		flags |= GUIKEYFLAG_SHIFT;
		if( I9_GetKeyValue(I9K_LCONTROL) || I9_GetKeyValue(I9K_RCONTROL) )	flags |= GUIKEYFLAG_CTRL;
		if( I9_GetKeyValue(I9K_LALT) || I9_GetKeyValue(I9K_RALT) )			flags |= GUIKEYFLAG_ALT;
		return k.m_flags == flags; // flags match exactly
	});
	if (k != m_keys.end())
	{
		// select this dialog when we call OnKey command
		g_gui->m_lastdlg = g_gui->DlgFind(this);
		g_gui->m_lastitem = -1;
		g_gui->ScriptPrologDo(k->cmd);	
	}
}

//////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////
