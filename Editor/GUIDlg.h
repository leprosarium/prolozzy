//////////////////////////////////////////////////////////////////////////////////////////////////
// GUIDlg.h
//////////////////////////////////////////////////////////////////////////////////////////////////
#ifndef __GUIDLG_H__
#define __GUIDLG_H__

#include "GUIItem.h"
#include "GUI.h"

#include <algorithm>

class cGUIDlg;

namespace Dlg
{

class Cmd : std::vector<std::function<bool()>>
{
public:
	std::string cmd;
	Cmd(const std::string & cmd) : cmd(cmd) {}
	void KeyboardKey(int key);
	void MouseKey(int key);
	void Shift();	
	void Ctrl();	
	void Alt();	
	void WheelUp();
	void WheelDown();
	bool Active() const;
};

class Keys : std::vector<Cmd>
{
	void Test(cGUIDlg *);
public:
	enum class Mode { none, always, mousein } mode;

	Keys() : mode(Mode::always) {}
	void Test(cGUIDlg * dlg, bool mousein) { if( mode == Mode::always || mode == Mode::mousein && mousein) Test(dlg); }

	void AddCmd(const std::string & cmd) { push_back(Cmd(cmd)); }
	Cmd & Last() { return back(); }
};

}

//////////////////////////////////////////////////////////////////////////////////////////////////
// cGUIDlg
//////////////////////////////////////////////////////////////////////////////////////////////////

class DlgItems : std::vector<cGUIItem *> 
{
	typedef std::vector<cGUIItem *> Items;
	void Add(cGUIItem * i) { push_back(i); }
	void Update() { for(cGUIItem * i: *this) i->Update(); }
	void Draw() { for(cGUIItem * i: *this) i->Draw(); }
public:
	friend class cGUIDlg;
	using Items::begin;
	using Items::end;
	using Items::size;
	using Items::operator[];
	~DlgItems() { for(cGUIItem * i: *this) { delete i; }; }
	cGUIItem * Find(const std::string & id) { auto i = std::find_if(begin(), end(), [id](cGUIItem *i){ return i->id == id; }); return i == end() ? nullptr : *i; }
};

class cGUIDlg
{
public:
	std::string id;
	bool hidden;
	bool disable;
	iRect rect;
	bool modal;
	bool closeOut;
	int closeRet;
	std::string closeCmd;
	bool m_mustclose;

	cGUIDlg();
	virtual ~cGUIDlg();

	virtual	void Update();
	virtual	void Draw() { Items.Draw(); }
	virtual	void Close(int ret);

	template<class Item>
	cGUIItem * Add()
	{
		cGUIItem * i = new Item(this);
		Items.Add(i);
		return i;
	}
	DlgItems Items;
	Dlg::Keys Keys;
};

#endif
//////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////
