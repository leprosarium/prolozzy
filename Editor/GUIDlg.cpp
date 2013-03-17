//////////////////////////////////////////////////////////////////////////////////////////////////
// GUIDlg.cpp
//////////////////////////////////////////////////////////////////////////////////////////////////
#include "stdafx.h"
#include "GUIDlg.h"
#include "E9String.h"

//////////////////////////////////////////////////////////////////////////////////////////////////
// cGUIDlg
//////////////////////////////////////////////////////////////////////////////////////////////////
cGUIDlg::cGUIDlg()
{
	memset(&m_var,0,sizeof(m_var));
	SetInt(DV_TESTKEY, 1);
	m_mustclose = FALSE;
	m_mousein = FALSE;
}

cGUIDlg::~cGUIDlg()
{
	SetTxt(DV_CLOSECMD,NULL);
	for(; !m_item.empty(); m_item.pop_back()) {
		m_item.back()->m_dlg = NULL;
		delete m_item.back();
	}
	m_item.clear();
	m_keys.clear();
}

void cGUIDlg::Update()
{
	// mouse in	
	RECT rc;
	rc = GetRect(DV_RECT);
	m_mousein = INRECT( g_gui->m_mousex, g_gui->m_mousey, rc);

	// update children
	for(size_t i=0;i<m_item.size();i++)
	{
		cGUIItem* item = m_item[i];
		if(!item->GetInt(IV_DISABLE))
		{
			item->Update();
			if( item->m_mousein && 
				item->GetInt(IV_HIDDEN)==0 &&
				item->GetInt(IV_DISABLE)==0 &&
				item->GetTxt(IV_TOOLTIP) )
				g_gui->SetTooltip(item->GetTxt(IV_TOOLTIP));
		}
	}

	// test key
	if( (GetInt(DV_TESTKEY) == 1) || 
		(GetInt(DV_TESTKEY) == 2 && m_mousein) )
		TestKey();

	// test click
	if( GetInt(DV_CLOSEOUT) && I9_GetKeyDown(I9_MOUSE_B1) && !m_mousein )
		Close(-1);

}

void cGUIDlg::Draw()
{
	for(size_t i=0;i<m_item.size();i++)
		if(!m_item[i]->GetInt(IV_HIDDEN))
			m_item[i]->Draw();
}

void cGUIDlg::Close(int ret)
{
	// select this dialog when we call Close
	g_gui->m_lastdlg = g_gui->DlgFind(this);
	g_gui->m_lastitem = -1;

	SetInt(DV_CLOSERET,ret);
	if(char * cmd = GetTxt(DV_CLOSECMD))
		g_gui->ScriptPrologDo(cmd);		
	
	g_gui->m_lastdlg = -1;
	g_gui->m_lastitem = -1;
	m_mustclose = TRUE;
	// from here the dialog should not be used anymore (it will be destroyed asp)
}

/////////////////////////////////////////////////////////////////////////////////////////////////
// Access
//////////////////////////////////////////////////////////////////////////////////////////////////

void cGUIDlg::SetInt( int idx, int val )
{
	assert(0<=idx && idx<DV_MAX);
	m_var[idx].m_int=val;
}

int cGUIDlg::GetInt( int idx )
{
	assert(0<=idx && idx<DV_MAX);
	return m_var[idx].m_int;	
}

void cGUIDlg::SetTxt( int idx, char* text )
{
	assert(0<=idx && idx<DV_MAX);
	char* sz = m_var[idx].m_str;
	if(sz) free(sz);
	m_var[idx].m_str = sstrdup(text);
}

char* cGUIDlg::GetTxt( int idx )
{
	assert(0<=idx && idx<DV_MAX);
	return m_var[idx].m_str;
}

void cGUIDlg::SetPoint( int idx, POINT point )
{
	assert(0<=idx && idx<DV_MAX-1);
	m_var[idx+0].m_int=point.x;
	m_var[idx+1].m_int=point.y;
}

POINT cGUIDlg::GetPoint( int idx )
{
	assert(0<=idx && idx<DV_MAX-1);
	POINT point;
	point.x = m_var[idx+0].m_int;
	point.y = m_var[idx+1].m_int;
	return point;
}

void cGUIDlg::SetRect( int idx, RECT rect )
{
	assert(0<=idx && idx<DV_MAX-3);
	m_var[idx+0].m_int = rect.left;
	m_var[idx+1].m_int = rect.top;
	m_var[idx+2].m_int = rect.right;
	m_var[idx+3].m_int = rect.bottom;
}

RECT cGUIDlg::GetRect( int idx )
{
	assert(0<=idx && idx<DV_MAX-3);
	RECT rect;
	rect.left = m_var[idx+0].m_int;
	rect.top = m_var[idx+1].m_int;
	rect.right = m_var[idx+2].m_int;
	rect.bottom = m_var[idx+3].m_int;
	return rect;
}

//////////////////////////////////////////////////////////////////////////////////////////////////
// Inheritance
//////////////////////////////////////////////////////////////////////////////////////////////////

int cGUIDlg::ItemFind( int id )
{
	for(size_t i=0;i<m_item.size();i++)
		if(m_item[i]->GetInt(IV_ID) == id)
			return i;
	return -1;
}

int cGUIDlg::ItemFind( cGUIItem* item )
{
	for(size_t i=0;i<m_item.size();i++)
		if(m_item[i] == item)
			return i;
	return -1;
}

void cGUIDlg::AddKey( int key, int flags, const std::string & cmd )
{
	m_keys.push_back(tDlgKey(key, flags, cmd));
}

void cGUIDlg::TestKey()
{

	for(std::vector<tDlgKey>::const_iterator k = m_keys.begin(), e = m_keys.end(); k != e; ++k)
	{
		if(I9_GetKeyDown(k->m_key))
		{
			int keyflags = k->m_flags;
			int flags = 0;
			if( I9_GetKeyValue(I9K_LSHIFT) || I9_GetKeyValue(I9K_RSHIFT) )		flags |= GUIKEYFLAG_SHIFT;
			if( I9_GetKeyValue(I9K_LCONTROL) || I9_GetKeyValue(I9K_RCONTROL) )	flags |= GUIKEYFLAG_CTRL;
			if( I9_GetKeyValue(I9K_LALT) || I9_GetKeyValue(I9K_RALT) )			flags |= GUIKEYFLAG_ALT;

			if( keyflags == flags ) // flags match exactly
			{
				// select this dialog when we call OnKey command
				g_gui->m_lastdlg = g_gui->DlgFind(this);
				g_gui->m_lastitem = -1;
				g_gui->ScriptPrologDo(k->cmd);	
				return;
			}
		}
	}
}

//////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////
