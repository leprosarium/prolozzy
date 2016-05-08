//////////////////////////////////////////////////////////////////////////////////////////////////
// DizCfg.cpp
//////////////////////////////////////////////////////////////////////////////////////////////////
#include "stdafx.h"
#include "DizCfg.h"
#include "DizDef.h"
#include "F9Files.h"
#include "dinput.h"

cDizCfg g_cfg;

std::wstring GetFileName(const std::wstring & ext)
{
	wchar_t apppath[MAX_PATH];
	if(GetModuleFileNameW( NULL, apppath, MAX_PATH ))
		return file_path2name(apppath) + L"." + ext;
	return std::wstring(L"dizzy.") + ext;		
}

const std::wstring & GetIniFile()
{
	static std::wstring name = GetFileName(L"ini");
	return name;
}

const std::wstring & GetPakFile()
{
	static std::wstring name = GetFileName(L"pak");
	return name;
}

const std::wstring & GetLogFile()
{
	static std::wstring name = GetFileName(L"log");
	return name;
}

//////////////////////////////////////////////////////////////////////////////////////////////////
// INIT
//////////////////////////////////////////////////////////////////////////////////////////////////
cDizCfg::cDizCfg()
{

	m_scale					= 0;

	m_volfx					= 100;
	m_volmusic				= 100;
	
	m_key[key::left][0]		= DIK_LEFT;
	m_key[key::right][0]	= DIK_RIGHT;
	m_key[key::up][0]		= DIK_UP;
	m_key[key::down][0]		= DIK_DOWN;
	m_key[key::jump][0]		= DIK_SPACE;
	m_key[key::action][0]	= DIK_RETURN;
	m_key[key::menu][0]		= DIK_ESCAPE;
	
	m_key[key::left][1]		= DIK_Z;
	m_key[key::right][1]	= DIK_X;
	m_key[key::up][1]		= DIK_K;
	m_key[key::down][1]		= DIK_M;
	m_key[key::jump][1]		= DIK_SPACE;
	m_key[key::action][1]	= DIK_RETURN;
	m_key[key::menu][1]		= DIK_Q;

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
	F9FILE f = files->OpenFile(L"data\\dizzy.inf");
	if(!f) { elog::app() << "dizzy.inf not found" << std::endl; return; }
	auto size = f->Size();
	if(!size) { files->FileClose(f); return; }

	char * buf = new char[static_cast<int>(size)];
	f->Read(buf, size);
	files->FileClose(f);

	std::wistringstream in;
	in.str(std::wstring(buf, buf + size));
	delete [] buf;

	std::wstring key, val, eq;
	while(in)
	{
		if(!(in >> key >> eq) || eq != L"=")
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
	std::wstring inifile = file_getfullpath(GetIniFile());

	// VIDEO
	ini_get(inifile, L"VIDEO",	L"scale") >> m_scale;

	// AUDIO
	ini_get(inifile, L"AUDIO",	L"volfx") >> m_volfx;
	ini_get(inifile, L"AUDIO",	L"volmusic") >> m_volmusic;

	// INPUT
	ini_get(inifile, L"INPUT",	L"key1_left")	>> m_key[key::left][0];
	ini_get(inifile, L"INPUT",	L"key1_right")	>> m_key[key::right][0];
	ini_get(inifile, L"INPUT",	L"key1_up")		>> m_key[key::up][0];
	ini_get(inifile, L"INPUT",	L"key1_down")	>> m_key[key::down][0];
	ini_get(inifile, L"INPUT",	L"key1_jump")	>> m_key[key::jump][0];
	ini_get(inifile, L"INPUT",	L"key1_action")	>> m_key[key::action][0];
	ini_get(inifile, L"INPUT",	L"key1_menu")	>> m_key[key::menu][0];

	ini_get(inifile, L"INPUT",	L"key2_left")	>> m_key[key::left][1];
	ini_get(inifile, L"INPUT",	L"key2_right")	>> m_key[key::right][1];
	ini_get(inifile, L"INPUT",	L"key2_up")		>> m_key[key::up][1];
	ini_get(inifile, L"INPUT",	L"key2_down")	>> m_key[key::down][1];
	ini_get(inifile, L"INPUT",	L"key2_jump")	>> m_key[key::jump][1];
	ini_get(inifile, L"INPUT",	L"key2_action")	>> m_key[key::action][1];
	ini_get(inifile, L"INPUT",	L"key2_menu")	>> m_key[key::menu][1];

	ini_get(inifile, L"INPUT",	L"rumble")		>> m_rumble;
	ini_get(inifile, L"INPUT",	L"joystick_dx")	>> m_deadzone[0];
	ini_get(inifile, L"INPUT",	L"joystick_dy")	>> m_deadzone[1];
	ini_get(inifile, L"INPUT",	L"joystick_b0")	>> m_joy[0];
	ini_get(inifile, L"INPUT",	L"joystick_b1")	>> m_joy[1];
	ini_get(inifile, L"INPUT",	L"joystick_b2")	>> m_joy[2];
	ini_get(inifile, L"INPUT",	L"joystick_b3")	>> m_joy[3];
	ini_get(inifile, L"INPUT",	L"joystick_ax")	>> m_joy[4];
	ini_get(inifile, L"INPUT",	L"joystick_ay")	>> m_joy[5];

	return true;
}

bool cDizCfg::Save()
{
	std::wstring inifile = file_getfullpath(GetIniFile());

	// VIDEO
	ini_set( inifile, L"VIDEO",	L"scale",		m_scale					);

	// AUDIO
	ini_set( inifile, L"AUDIO",	L"volfx",		m_volfx					);
	ini_set( inifile, L"AUDIO",	L"volmusic",		m_volmusic				);

	// INPUT
	ini_set( inifile, L"INPUT",	L"key1_left",	m_key[key::left][0]		);
	ini_set( inifile, L"INPUT",	L"key1_right",	m_key[key::right][0]	);
	ini_set( inifile, L"INPUT",	L"key1_up",		m_key[key::up][0]		);
	ini_set( inifile, L"INPUT",	L"key1_down",	m_key[key::down][0]		);
	ini_set( inifile, L"INPUT",	L"key1_jump",	m_key[key::jump][0]		);
	ini_set( inifile, L"INPUT",	L"key1_action",	m_key[key::action][0]	);
	ini_set( inifile, L"INPUT",	L"key1_menu",	m_key[key::menu][0]		);

	ini_set( inifile, L"INPUT",	L"key2_left",	m_key[key::left][1]		);
	ini_set( inifile, L"INPUT",	L"key2_right",	m_key[key::right][1]	);
	ini_set( inifile, L"INPUT",	L"key2_up",		m_key[key::up][1]		);
	ini_set( inifile, L"INPUT",	L"key2_down",	m_key[key::down][1]		);
	ini_set( inifile, L"INPUT",	L"key2_jump",	m_key[key::jump][1]		);
	ini_set( inifile, L"INPUT",	L"key2_action",	m_key[key::action][1]	);
	ini_set( inifile, L"INPUT",	L"key2_menu",	m_key[key::menu][1]		);

	ini_set( inifile, L"INPUT",	L"rumble",		m_rumble				);
	ini_set( inifile, L"INPUT",	L"joystick_dx",	m_deadzone[0]			);
	ini_set( inifile, L"INPUT",	L"joystick_dy",	m_deadzone[1]			);
	ini_set( inifile, L"INPUT",	L"joystick_b0",	m_joy[0]				);
	ini_set( inifile, L"INPUT",	L"joystick_b1",	m_joy[1]				);
	ini_set( inifile, L"INPUT",	L"joystick_b2",	m_joy[2]				);
	ini_set( inifile, L"INPUT",	L"joystick_b3",	m_joy[3]				);
	ini_set( inifile, L"INPUT",	L"joystick_ax",	m_joy[4]				);
	ini_set( inifile, L"INPUT",	L"joystick_ay",	m_joy[5]				);

	return true;
}

void cDizCfg::LoadRenderCfg( r9Cfg& cfg, Api & api )
{
	std::wstring inifile = file_getfullpath(GetIniFile());

	api				= Api::Default;
	cfg.windowed	= 1;
	cfg.bpp			= 32;
	cfg.width		= CfgWidth;
	cfg.height		= CfgHeight;
	cfg.refresh		= 85;
	cfg.vsync		= 0;

	int apiv;
	if(ini_get(inifile, L"VIDEO", L"api") >> apiv)
		if(apiv == static_cast<int>(Api::DirectX))
			api = Api::DirectX;
		else if(apiv == static_cast<int>(Api::OpenGL))
			api = Api::OpenGL;
	ini_get(inifile, L"VIDEO", L"windowed")>> cfg.windowed;
	ini_get(inifile, L"VIDEO", L"bpp") >> cfg.bpp;
	ini_get(inifile, L"VIDEO", L"width") >> cfg.width;
	ini_get(inifile, L"VIDEO", L"height") >>	cfg.height;
	ini_get(inifile, L"VIDEO", L"refresh") >>	cfg.refresh;
	ini_get(inifile, L"VIDEO", L"vsync") >> cfg.vsync;

}

std::wstring cDizCfg::Info(const std::wstring & name )
{
	auto it = info.find(name);
	return it == info.end() ? std::wstring() : it->second;
}

//////////////////////////////////////////////////////////////////////////////////////////////////
