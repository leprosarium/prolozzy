//////////////////////////////////////////////////////////////////////////////////////////////////
// GUIDlg.cpp
//////////////////////////////////////////////////////////////////////////////////////////////////
#include "stdafx.h"
#include <algorithm>
#include "GUIDlg.h"
#include "E9String.h"
#include "eInput.h"

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
	if( closeOut && einput->isMouseDown(0) && !m_mousein )
		Close(-1);

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

namespace Dlg
{

void Cmd::KeyboardKey(int key) { push_back([key](){ return einput->isKeyDown(key); }); }
void Cmd::MouseKey(int key) { push_back([key](){ return einput->isMouseDown(key); }); }
void Cmd::Shift() { push_back([]() { return einput->shift(); }); }
void Cmd::Ctrl() { push_back([]() { return einput->ctrl(); }); }
void Cmd::Alt() { push_back([]() { return einput->alt(); }); }
void Cmd::WheelUp() { push_back([](){ return einput->mouse.axe[2].delta < -25; }); }
void Cmd::WheelDown() { push_back([](){ return einput->mouse.axe[2].delta > 25; }); }

bool Cmd::Active() const
{
	for(auto f: *this) if(!f()) return false;
	return true;
}

void Keys::Test(cGUIDlg * dlg)
{
	auto k = std::find_if(begin(), end(), [](const Dlg::Cmd & k) { return k.Active(); });
	if(k != end())
	{
		g_gui->SetLast(dlg);
		g_gui->ScriptPrologDo(k->cmd);	
	}


}

}


//////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////
