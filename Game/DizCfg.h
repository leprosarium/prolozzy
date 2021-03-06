//////////////////////////////////////////////////////////////////////////////////////////////////
// DizCfg.h
//////////////////////////////////////////////////////////////////////////////////////////////////
#ifndef __DIZCFG_H__
#define __DIZCFG_H__

#include "R9Render.h"
#include <map>

struct r9Cfg;

const std::wstring & GetIniFile();		// gets .ini file name, based on the executable's name
const std::wstring & GetPakFile();		// gets .pak file name, based on the executable's name
const std::wstring & GetLogFile();		// gets .log file name, based on the executable's name

class cDizCfg
{
	typedef std::map<std::wstring, std::wstring> strings;
	strings info;
	void LoadInfo();
public:
	enum key { left = 0, right, up, down, jump, action, menu, max };
	cDizCfg();
	void Init();
	bool Load();
	bool Save();

	void	LoadRenderCfg	( r9Cfg& cfg, Api & api );
	std::wstring Info(const std::wstring & name);

	int		m_scale;						// scale (0=full)
	int		m_volfx;						// percent
	int		m_volmusic;						// percent
	int		m_key[key::max][2];				// I9K values, two sets
	int		m_rumble;						// rumble
	int		m_deadzone[2];					// joystick dead zones x, y
	int		m_joy[6];						// joystick mapping 4 keys and 2 axes
};

extern	cDizCfg		g_cfg;

#endif
//////////////////////////////////////////////////////////////////////////////////////////////////