//////////////////////////////////////////////////////////////////////////////////////////////////
// DizCfg.h
//////////////////////////////////////////////////////////////////////////////////////////////////
#ifndef __DIZCFG_H__
#define __DIZCFG_H__

#include "R9Render.h"
#include <map>

#define KEY_LEFT			0
#define KEY_RIGHT			1
#define KEY_UP				2
#define KEY_DOWN			3
#define KEY_JUMP			4
#define KEY_ACTION			5
#define KEY_MENU			6
#define KEY_MAX				7

struct r9Cfg;

const char*	GetIniFile();		// gets .ini file name, based on the executable's name
const char*	GetPakFile();		// gets .pak file name, based on the executable's name
const char*	GetLogFile();		// gets .log file name, based on the executable's name

class cDizCfg
{
	typedef std::map<std::string, std::string> strings;
	strings info;
	void LoadInfo();
public:
	cDizCfg();
	void Init();
	bool Load();
	bool Save();

	void	LoadRenderCfg	( r9Cfg& cfg, Api & api );
	std::string	Info(const std::string & name);

	int		m_scale;						// scale (0=full)
	int		m_volfx;						// percent
	int		m_volmusic;						// percent
	int		m_key[KEY_MAX][2];				// I9K values, two sets
	int		m_rumble;						// rumble
	int		m_deadzone[2];					// joystick dead zones x, y
	int		m_joy[6];						// joystick mapping 4 keys and 2 axes
};

extern	cDizCfg		g_cfg;

#endif
//////////////////////////////////////////////////////////////////////////////////////////////////