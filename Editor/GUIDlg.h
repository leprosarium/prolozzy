//////////////////////////////////////////////////////////////////////////////////////////////////
// GUIDlg.h
//////////////////////////////////////////////////////////////////////////////////////////////////
#ifndef __GUIDLG_H__
#define __GUIDLG_H__

#include "GUIItem.h"
#include "GUI.h"

#include <algorithm>

struct  DlgKey
{
	DlgKey()	{};
	DlgKey(int key, int flags, const std::string & cmd) : m_key(key), m_flags(flags), cmd(cmd) {}
	int		m_key;
	byte	m_flags;			// bit 0 = shift, bit 1 = ctrl, bit 2 = alt etc
	std::string cmd;
	bool Active() const;
};

class cGUIDlg;

class DlgKeys : std::vector<DlgKey>
{
	void Test(cGUIDlg *);
public:
	enum class Mode { none, always, mousein } mode;

	DlgKeys() : mode(Mode::always) {}
	void Add(int key, int flags, const std::string & cmd) { push_back(DlgKey(key, flags, cmd)); }
	void Test(cGUIDlg * dlg, bool mousein) { if( mode == Mode::always || mode == Mode::mousein && mousein) Test(dlg); }
};

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
	~DlgItems() { for(cGUIItem * i: *this) { delete i; }; }
	cGUIItem * Find(int id) { auto i = std::find_if(begin(), end(), [id](cGUIItem *i){ return i->id == id; }); return i == end() ? nullptr : *i; }
};

class cGUIDlg
{
public:
	int id;
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
	DlgKeys Keys;
};

#endif
//////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////
