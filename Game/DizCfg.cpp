//////////////////////////////////////////////////////////////////////////////////////////////////
// DizCfg.cpp
//////////////////////////////////////////////////////////////////////////////////////////////////
#include "stdafx.h"
#include "DizCfg.h"
#include "DizDef.h"

cDizCfg g_cfg;

const char* GetIniFile()
{
	static char name[64] = "dizzy.ini";
	char apppath[MAX_PATH];
	if(!GetModuleFileName( NULL, apppath, MAX_PATH )) return name;
	strcpy(name,file_path2file(apppath));
	strcpy(const_cast<char *>(file_path2ext(name)),"ini");
	return name;
}

const char* GetPakFile()
{
	static char name[64] = "dizzy.pak";
	char apppath[MAX_PATH];
	if(!GetModuleFileName( NULL, apppath, MAX_PATH )) return name;
	strcpy(name,file_path2file(apppath));
	strcpy(const_cast<char *>(file_path2ext(name)),"pak");
	return name;
}

const char* GetLogFile()
{
	static char name[64] = "dizzy.log";
	char apppath[MAX_PATH];
	if(!GetModuleFileName( NULL, apppath, MAX_PATH )) return name;
	strcpy(name,file_path2file(apppath));
	strcpy(const_cast<char *>(file_path2ext(name)),"log");
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
	
	m_key[KEY_LEFT][0]		= I9K_LEFT;
	m_key[KEY_RIGHT][0]		= I9K_RIGHT;
	m_key[KEY_UP][0]		= I9K_UP;
	m_key[KEY_DOWN][0]		= I9K_DOWN;
	m_key[KEY_JUMP][0]		= I9K_SPACE;
	m_key[KEY_ACTION][0]	= I9K_RETURN;
	m_key[KEY_MENU][0]		= I9K_ESCAPE;
	
	m_key[KEY_LEFT][1]		= I9K_Z;
	m_key[KEY_RIGHT][1]		= I9K_X;
	m_key[KEY_UP][1]		= I9K_K;
	m_key[KEY_DOWN][1]		= I9K_M;
	m_key[KEY_JUMP][1]		= I9K_SPACE;
	m_key[KEY_ACTION][1]	= I9K_RETURN;
	m_key[KEY_MENU][1]		= I9K_Q;

	m_rumble				= true;
	m_deadzone[0]			= 500;
	m_deadzone[1]			= 500;
	m_joy[0]				= 0;
	m_joy[1]				= 1;
	m_joy[2]				= 2;
	m_joy[3]				= 3;
	m_joy[4]				= 0;
	m_joy[5]				= 1;

	m_info					= 0;

}

void cDizCfg::Init()
{
	Load(); Save();
	// read info file
	F9FILE f = F9_FileOpen("Data\\dizzy.inf");
	if(!f) { dlog(LOGAPP, L"dizzy.inf not found\n"); return; }
	int size = static_cast<int>(f->Size());
	if(size==0) { F9_FileClose(f); return; }
	m_info = new char[size+1]; m_info[size]=0;
	f->Read(m_info, size);
	F9_FileClose(f);
}

void cDizCfg::Done()
{
	delete [] m_info;
}

//////////////////////////////////////////////////////////////////////////////////////////////////
// LOAD SAVE
//////////////////////////////////////////////////////////////////////////////////////////////////
bool cDizCfg::Load()
{
	char inifile[256];

	// USER
	strcpy( inifile, file_getfullpath(GetIniFile()) );

	// VIDEO
	ini_getint( inifile, "VIDEO",	"scale",		&m_scale				);

	// AUDIO
	ini_getint( inifile, "AUDIO",	"volfx",		&m_volfx				);
	ini_getint( inifile, "AUDIO",	"volmusic",		&m_volmusic				);

	// INPUT
	ini_getint( inifile, "INPUT",	"key1_left",	&m_key[KEY_LEFT][0]		);
	ini_getint( inifile, "INPUT",	"key1_right",	&m_key[KEY_RIGHT][0]	);
	ini_getint( inifile, "INPUT",	"key1_up",		&m_key[KEY_UP][0]		);
	ini_getint( inifile, "INPUT",	"key1_down",	&m_key[KEY_DOWN][0]		);
	ini_getint( inifile, "INPUT",	"key1_jump",	&m_key[KEY_JUMP][0]		);
	ini_getint( inifile, "INPUT",	"key1_action",	&m_key[KEY_ACTION][0]	);
	ini_getint( inifile, "INPUT",	"key1_menu",	&m_key[KEY_MENU][0]		);

	ini_getint( inifile, "INPUT",	"key2_left",	&m_key[KEY_LEFT][1]		);
	ini_getint( inifile, "INPUT",	"key2_right",	&m_key[KEY_RIGHT][1]	);
	ini_getint( inifile, "INPUT",	"key2_up",		&m_key[KEY_UP][1]		);
	ini_getint( inifile, "INPUT",	"key2_down",	&m_key[KEY_DOWN][1]		);
	ini_getint( inifile, "INPUT",	"key2_jump",	&m_key[KEY_JUMP][1]		);
	ini_getint( inifile, "INPUT",	"key2_action",	&m_key[KEY_ACTION][1]	);
	ini_getint( inifile, "INPUT",	"key2_menu",	&m_key[KEY_MENU][1]		);

	ini_getint( inifile, "INPUT",	"rumble",		&m_rumble				);
	ini_getint( inifile, "INPUT",	"joystick_dx",	&m_deadzone[0]			);
	ini_getint( inifile, "INPUT",	"joystick_dy",	&m_deadzone[1]			);
	ini_getint( inifile, "INPUT",	"joystick_b0",	&m_joy[0]				);
	ini_getint( inifile, "INPUT",	"joystick_b1",	&m_joy[1]				);
	ini_getint( inifile, "INPUT",	"joystick_b2",	&m_joy[2]				);
	ini_getint( inifile, "INPUT",	"joystick_b3",	&m_joy[3]				);
	ini_getint( inifile, "INPUT",	"joystick_ax",	&m_joy[4]				);
	ini_getint( inifile, "INPUT",	"joystick_ay",	&m_joy[5]				);

	return true;
}

bool cDizCfg::Save()
{
	char inifile[256];

	// USER
	strcpy( inifile, file_getfullpath(GetIniFile()) );

	// VIDEO
	ini_setint( inifile, "VIDEO",	"scale",		m_scale					);

	// AUDIO
	ini_setint( inifile, "AUDIO",	"volfx",		m_volfx					);
	ini_setint( inifile, "AUDIO",	"volmusic",		m_volmusic				);

	// INPUT
	ini_setint( inifile, "INPUT",	"key1_left",	m_key[KEY_LEFT][0]		);
	ini_setint( inifile, "INPUT",	"key1_right",	m_key[KEY_RIGHT][0]		);
	ini_setint( inifile, "INPUT",	"key1_up",		m_key[KEY_UP][0]		);
	ini_setint( inifile, "INPUT",	"key1_down",	m_key[KEY_DOWN][0]		);
	ini_setint( inifile, "INPUT",	"key1_jump",	m_key[KEY_JUMP][0]		);
	ini_setint( inifile, "INPUT",	"key1_action",	m_key[KEY_ACTION][0]	);
	ini_setint( inifile, "INPUT",	"key1_menu",	m_key[KEY_MENU][0]		);

	ini_setint( inifile, "INPUT",	"key2_left",	m_key[KEY_LEFT][1]		);
	ini_setint( inifile, "INPUT",	"key2_right",	m_key[KEY_RIGHT][1]		);
	ini_setint( inifile, "INPUT",	"key2_up",		m_key[KEY_UP][1]		);
	ini_setint( inifile, "INPUT",	"key2_down",	m_key[KEY_DOWN][1]		);
	ini_setint( inifile, "INPUT",	"key2_jump",	m_key[KEY_JUMP][1]		);
	ini_setint( inifile, "INPUT",	"key2_action",	m_key[KEY_ACTION][1]	);
	ini_setint( inifile, "INPUT",	"key2_menu",	m_key[KEY_MENU][1]		);

	ini_setint( inifile, "INPUT",	"rumble",		m_rumble				);
	ini_setint( inifile, "INPUT",	"joystick_dx",	m_deadzone[0]			);
	ini_setint( inifile, "INPUT",	"joystick_dy",	m_deadzone[1]			);
	ini_setint( inifile, "INPUT",	"joystick_b0",	m_joy[0]				);
	ini_setint( inifile, "INPUT",	"joystick_b1",	m_joy[1]				);
	ini_setint( inifile, "INPUT",	"joystick_b2",	m_joy[2]				);
	ini_setint( inifile, "INPUT",	"joystick_b3",	m_joy[3]				);
	ini_setint( inifile, "INPUT",	"joystick_ax",	m_joy[4]				);
	ini_setint( inifile, "INPUT",	"joystick_ay",	m_joy[5]				);

	return true;
}

void cDizCfg::LoadRenderCfg( r9Cfg& cfg, Api & api )
{
	char inifile[256];
	strcpy( inifile, file_getfullpath(GetIniFile()) );

	api				= Api::Default;
	cfg.windowed	= 1;
	cfg.bpp			= 32;
	cfg.width		= CfgWidth;
	cfg.height		= CfgHeight;
	cfg.refresh		= 85;
	cfg.vsync		= 0;

	int apiv;
	if(ini_getint( inifile, "VIDEO", "api",		&apiv ))
		if(apiv == static_cast<int>(Api::DirectX))
			api = Api::DirectX;
		else if(apiv == static_cast<int>(Api::OpenGL))
			api = Api::OpenGL;
	ini_get( inifile, "VIDEO", "windowed",	cfg.windowed );
	ini_getint( inifile, "VIDEO", "bpp",		&cfg.bpp );
	ini_getint( inifile, "VIDEO", "width",		&cfg.width );
	ini_getint( inifile, "VIDEO", "height",		&cfg.height );
	ini_getint( inifile, "VIDEO", "refresh",	&cfg.refresh );
	ini_getint( inifile, "VIDEO", "vsync",		&cfg.vsync );

}

const char* cDizCfg::GetInfoValue( const char* name )
{
	int p;
	const char* sz;
	static char szret[128];
	szret[0]=0;
	if(!m_info) return szret;
	if(!name || !name[0]) return szret;

	sz = parser_skiptotoken(m_info,name,p);
	if(!sz) return szret;
	sz = parser_skiptochar(sz,"=\r\n",p);
	if(!sz) return szret;
	if(*sz!='=') return szret;
	sz = parser_skipchar(sz+1," \t",p);
	parser_readline(sz,szret,127,p);
	parser_trimbackspace(szret,p);
	
	return szret;
}

//////////////////////////////////////////////////////////////////////////////////////////////////
