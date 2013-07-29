//////////////////////////////////////////////////////////////////////////////////////////////////
// DizCfg.cpp
//////////////////////////////////////////////////////////////////////////////////////////////////
#include "stdafx.h"
#include "DizCfg.h"
#include "DizDef.h"

cDizCfg g_cfg;

std::string GetFileName(const std::string & ext)
{
	char apppath[MAX_PATH];
	if(GetModuleFileName( NULL, apppath, MAX_PATH ))
		return file_path2name(apppath) + "." + ext;
	return std::string("dizzy.") + ext;		
}

const char* GetIniFile()
{
	static std::string name = GetFileName("ini");
	return name.c_str();
}

const char* GetPakFile()
{
	static std::string name = GetFileName("pak");
	return name.c_str();
}

const char* GetLogFile()
{
	static std::string name = GetFileName("log");
	return name.c_str();
}

//////////////////////////////////////////////////////////////////////////////////////////////////
// INIT
//////////////////////////////////////////////////////////////////////////////////////////////////
cDizCfg::cDizCfg()
{

	m_scale					= 0;

	m_volfx					= 100;
	m_volmusic				= 100;
	
	m_key[key::left][0]		= I9K_LEFT;
	m_key[key::right][0]	= I9K_RIGHT;
	m_key[key::up][0]		= I9K_UP;
	m_key[key::down][0]		= I9K_DOWN;
	m_key[key::jump][0]		= I9K_SPACE;
	m_key[key::action][0]	= I9K_RETURN;
	m_key[key::menu][0]		= I9K_ESCAPE;
	
	m_key[key::left][1]		= I9K_Z;
	m_key[key::right][1]	= I9K_X;
	m_key[key::up][1]		= I9K_K;
	m_key[key::down][1]		= I9K_M;
	m_key[key::jump][1]		= I9K_SPACE;
	m_key[key::action][1]	= I9K_RETURN;
	m_key[key::menu][1]		= I9K_Q;

	m_rumble				= true;
	m_deadzone[0]			= 500;
	m_deadzone[1]			= 500;
	m_joy[0]				= 0;
	m_joy[1]				= 1;
	m_joy[2]				= 2;
	m_joy[3]				= 3;
	m_joy[4]				= 0;
	m_joy[5]				= 1;
}

void cDizCfg::Init()
{
	Load();
	Save();
	LoadInfo();
}

void cDizCfg::LoadInfo()
{
	F9FILE f = files->OpenFile("data\\dizzy.inf");
	if(!f) { dlog(LOGAPP, L"dizzy.inf not found\n"); return; }
	auto size = f->Size();
	if(!size) { files->FileClose(f); return; }

	char * buf = new char[static_cast<int>(size)];
	f->Read(buf, size);
	files->FileClose(f);

	std::istringstream in;
	in.str(std::string(buf, buf + size));
	delete [] buf;

	std::string key, val, eq;
	while(in)
	{
		if(!(in >> key >> eq) || eq != "=")
			break;
		std::getline(in, val);
		info.insert(strings::value_type(key, trim(val)));
	}
}

//////////////////////////////////////////////////////////////////////////////////////////////////
// LOAD SAVE
//////////////////////////////////////////////////////////////////////////////////////////////////
bool cDizCfg::Load()
{
	std::string inifile = file_getfullpath(GetIniFile());

	// VIDEO
	ini_get(inifile, "VIDEO",	"scale") >> m_scale;

	// AUDIO
	ini_get(inifile, "AUDIO",	"volfx") >> m_volfx;
	ini_get(inifile, "AUDIO",	"volmusic") >> m_volmusic;

	// INPUT
	ini_get(inifile, "INPUT",	"key1_left")	>> m_key[key::left][0];
	ini_get(inifile, "INPUT",	"key1_right")	>> m_key[key::right][0];
	ini_get(inifile, "INPUT",	"key1_up")		>> m_key[key::up][0];
	ini_get(inifile, "INPUT",	"key1_down")	>> m_key[key::down][0];
	ini_get(inifile, "INPUT",	"key1_jump")	>> m_key[key::jump][0];
	ini_get(inifile, "INPUT",	"key1_action")	>> m_key[key::action][0];
	ini_get(inifile, "INPUT",	"key1_menu")	>> m_key[key::menu][0];

	ini_get(inifile, "INPUT",	"key2_left")	>> m_key[key::left][1];
	ini_get(inifile, "INPUT",	"key2_right")	>> m_key[key::right][1];
	ini_get(inifile, "INPUT",	"key2_up")		>> m_key[key::up][1];
	ini_get(inifile, "INPUT",	"key2_down")	>> m_key[key::down][1];
	ini_get(inifile, "INPUT",	"key2_jump")	>> m_key[key::jump][1];
	ini_get(inifile, "INPUT",	"key2_action")	>> m_key[key::action][1];
	ini_get(inifile, "INPUT",	"key2_menu")	>> m_key[key::menu][1];

	ini_get(inifile, "INPUT",	"rumble")		>> m_rumble;
	ini_get(inifile, "INPUT",	"joystick_dx")	>> m_deadzone[0];
	ini_get(inifile, "INPUT",	"joystick_dy")	>> m_deadzone[1];
	ini_get(inifile, "INPUT",	"joystick_b0")	>> m_joy[0];
	ini_get(inifile, "INPUT",	"joystick_b1")	>> m_joy[1];
	ini_get(inifile, "INPUT",	"joystick_b2")	>> m_joy[2];
	ini_get(inifile, "INPUT",	"joystick_b3")	>> m_joy[3];
	ini_get(inifile, "INPUT",	"joystick_ax")	>> m_joy[4];
	ini_get(inifile, "INPUT",	"joystick_ay")	>> m_joy[5];

	return true;
}

bool cDizCfg::Save()
{
	std::string inifile = file_getfullpath(GetIniFile());

	// VIDEO
	ini_set( inifile, "VIDEO",	"scale",		m_scale					);

	// AUDIO
	ini_set( inifile, "AUDIO",	"volfx",		m_volfx					);
	ini_set( inifile, "AUDIO",	"volmusic",		m_volmusic				);

	// INPUT
	ini_set( inifile, "INPUT",	"key1_left",	m_key[key::left][0]		);
	ini_set( inifile, "INPUT",	"key1_right",	m_key[key::right][0]	);
	ini_set( inifile, "INPUT",	"key1_up",		m_key[key::up][0]		);
	ini_set( inifile, "INPUT",	"key1_down",	m_key[key::down][0]		);
	ini_set( inifile, "INPUT",	"key1_jump",	m_key[key::jump][0]		);
	ini_set( inifile, "INPUT",	"key1_action",	m_key[key::action][0]	);
	ini_set( inifile, "INPUT",	"key1_menu",	m_key[key::menu][0]		);

	ini_set( inifile, "INPUT",	"key2_left",	m_key[key::left][1]		);
	ini_set( inifile, "INPUT",	"key2_right",	m_key[key::right][1]	);
	ini_set( inifile, "INPUT",	"key2_up",		m_key[key::up][1]		);
	ini_set( inifile, "INPUT",	"key2_down",	m_key[key::down][1]		);
	ini_set( inifile, "INPUT",	"key2_jump",	m_key[key::jump][1]		);
	ini_set( inifile, "INPUT",	"key2_action",	m_key[key::action][1]	);
	ini_set( inifile, "INPUT",	"key2_menu",	m_key[key::menu][1]		);

	ini_set( inifile, "INPUT",	"rumble",		m_rumble				);
	ini_set( inifile, "INPUT",	"joystick_dx",	m_deadzone[0]			);
	ini_set( inifile, "INPUT",	"joystick_dy",	m_deadzone[1]			);
	ini_set( inifile, "INPUT",	"joystick_b0",	m_joy[0]				);
	ini_set( inifile, "INPUT",	"joystick_b1",	m_joy[1]				);
	ini_set( inifile, "INPUT",	"joystick_b2",	m_joy[2]				);
	ini_set( inifile, "INPUT",	"joystick_b3",	m_joy[3]				);
	ini_set( inifile, "INPUT",	"joystick_ax",	m_joy[4]				);
	ini_set( inifile, "INPUT",	"joystick_ay",	m_joy[5]				);

	return true;
}

void cDizCfg::LoadRenderCfg( r9Cfg& cfg, Api & api )
{
	std::string inifile = file_getfullpath(GetIniFile());

	api				= Api::Default;
	cfg.windowed	= 1;
	cfg.bpp			= 32;
	cfg.width		= CfgWidth;
	cfg.height		= CfgHeight;
	cfg.refresh		= 85;
	cfg.vsync		= 0;

	int apiv;
	if(ini_get(inifile, "VIDEO", "api") >> apiv)
		if(apiv == static_cast<int>(Api::DirectX))
			api = Api::DirectX;
		else if(apiv == static_cast<int>(Api::OpenGL))
			api = Api::OpenGL;
	ini_get(inifile, "VIDEO", "windowed")>> cfg.windowed;
	ini_get(inifile, "VIDEO", "bpp") >> cfg.bpp;
	ini_get(inifile, "VIDEO", "width") >> cfg.width;
	ini_get(inifile, "VIDEO", "height") >>	cfg.height;
	ini_get(inifile, "VIDEO", "refresh") >>	cfg.refresh;
	ini_get(inifile, "VIDEO", "vsync") >> cfg.vsync;

}

std::string cDizCfg::Info(const std::string & name )
{
	auto it = info.find(name);
	return it == info.end() ? std::string() : it->second;
}

//////////////////////////////////////////////////////////////////////////////////////////////////
