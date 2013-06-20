//////////////////////////////////////////////////////////////////////////////////////////////////
// GUIDlg.h
//////////////////////////////////////////////////////////////////////////////////////////////////
#ifndef __GUIDLG_H__
#define __GUIDLG_H__

#include "GUIItem.h"
#include "GUI.h"

struct  tDlgKey
{
	tDlgKey()	{};
	tDlgKey(int key, int flags, const std::string & cmd) : m_key(key), m_flags(flags), cmd(cmd) {}
	int		m_key;
	byte	m_flags;			// bit 0 = shift, bit 1 = ctrl, bit 2 = alt etc
	std::string cmd;
};

//////////////////////////////////////////////////////////////////////////////////////////////////
// cGUIDlg
//////////////////////////////////////////////////////////////////////////////////////////////////

class cGUIDlg
{
public:
	int id;
	bool hidden;
	bool disable;
	iRect rect;
	bool modal;
	enum class TestKeyMode { none, always, mousein } testKey;
	bool closeOut;
	int closeRet;
	std::string closeCmd;

	cGUIDlg();
	virtual ~cGUIDlg();

	virtual	void Update();
	virtual	void Draw();
	virtual	void Close(int ret);

	int ItemCount() const { return m_item.size(); }
	cGUIItem * ItemGet(int idx) { if(0<=idx && idx<ItemCount()) return m_item[idx]; return 0; }
	int ItemAdd(cGUIItem * item){ if(item) { item->m_dlg=this; int idx = m_item.size(); m_item.push_back(item); return idx; } return -1; }
	void ItemDel(int idx) { if(0<=idx && idx<ItemCount()) { delete m_item[idx]; m_item.erase(m_item.begin() + idx); } }
	int ItemFind(int id);		
	int ItemFind(cGUIItem * item);

	std::vector<cGUIItem *> m_item;
	bool m_mustclose;
	bool m_mousein;

	std::vector<tDlgKey> m_keys;
	void AddKey(int key, int flags, const std::string & cmd);
	void TestKey();
					
};

#endif
//////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////
