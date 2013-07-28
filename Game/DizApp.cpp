///////////////////////////////////////////////////////////////////////////////////////////////////
// DizApp.cpp
///////////////////////////////////////////////////////////////////////////////////////////////////
#include "StdAfx.h"
#include "Resource.h"
#include "E9App.h"
#include "DizApp.h"
#include "DizDebug.h"

#include "DizCfg.h"
#include "DizGame.h"
#include "DizSound.h"

///////////////////////////////////////////////////////////////////////////////////////////////////
BOOL CALLBACK DialogProcInfo( HWND hwndDlg, UINT uMsg, WPARAM wParam, LPARAM lParam ) 
{
	if(uMsg == WM_INITDIALOG) 
	{
		// create info content
		std::ostringstream o1;
		o1 << g_cfg.Info("game_title") << " v" << g_cfg.Info("game_version") << "\r\n" << "by "
			<< g_cfg.Info("game_author") << "\r\n" 
			<< g_cfg.Info("game_website");
		SetDlgItemText(hwndDlg, IDC_TEXT1, o1.str().c_str());
		std::ostringstream o2;
		o2 << "Created with DizzyAGE v" << g_cfg.Info("dizzyage_version") << "\r\n"
			<< "by Alexandru and Cristina Simion\r\nhttp://www.yolkfolk.com/dizzyage";
		SetDlgItemText(hwndDlg, IDC_TEXT2, o2.str().c_str());
		return true; // autofocus
	}
	if(uMsg == WM_CLOSE || (uMsg == WM_COMMAND && (LOWORD(wParam) == IDOK || LOWORD(wParam) == IDCANCEL)))
	{ 
		EndDialog(hwndDlg, 0); 
		return true;
	}
	return false;
}

///////////////////////////////////////////////////////////////////////////////////////////////////
cDizApp::cDizApp() : gamefps(), drawstats(), musicwaspaused()
{
}

bool cDizApp::Init()
{
	dlog(LOGAPP, L"App init.\n");
	
	// engine
	if(!InitApp())	 { ERRORMESSAGE(L"Init app error.");   return false; }
	if(!InitFiles()) { ERRORMESSAGE(L"Init files error."); return false; }
	if(!InitInput()) { ERRORMESSAGE(L"Init input device error."); return false; }
	if(!InitAudio()) { ERRORMESSAGE(L"Init audio device error."); } // allow to run without audio
	if(!InitVideo()) { ERRORMESSAGE(L"Init video device error."); return false; }

	// game init
	g_cfg.Init();
	g_dizdebug.Init();
	g_paint.Init();
	if(!g_game.Init()) { return false; }
	if(!g_script.Init()) { ERRORMESSAGE(L"Script compiling error."); return false; }

	// game start
	g_game.Start();

	return true;
}

bool cDizApp::InitApp()
{
	App.Name(GAME_NAME);
	App.Icon(MAKEINTRESOURCE(IDI_ICON));

	bool cool = true;
	ini_get( file_getfullpath(GetIniFile()), "ADVANCED", "cool") >> cool;
	App.Cool(cool);
	
	return true;
}

bool cDizApp::InitFiles()
{
	if(!F9_Init()) return false;
	files->MakeIndex("data\\");
	return true;
}

bool cDizApp::InitInput()
{
	std::string inifile = file_getfullpath(GetIniFile());

	int input_enabled = 1;
	ini_get(inifile, "INPUT", "enabled") >> input_enabled;
	if(!input_enabled) return true; // no input
	
	BOOL ok = I9_Init(E9_GetHWND(),E9_GetHINSTANCE(),I9_API_DEFAULT);
	if(!ok) return false;

	// init devices
	int keyboard	= 1;
	int mouse		= 0;
	int joystick	= 1;
	ini_get(inifile, "INPUT", "keyboard") >> keyboard;
	ini_get(inifile, "INPUT", "mouse") >> mouse;
	ini_get(inifile, "INPUT", "joystick") >> joystick;
	if(keyboard)	ok = I9_DeviceInit(I9_DEVICE_KEYBOARD);
	if(mouse)		ok = I9_DeviceInit(I9_DEVICE_MOUSE);
	if(joystick)	ok = I9_DeviceInit(I9_DEVICE_JOYSTICK1);

	// rumble support
	if(I9_DeviceIsPresent(I9_DEVICE_JOYSTICK1))
		I9_DeviceFFInit(I9_DEVICE_JOYSTICK1);
	
	return true;
}

bool cDizApp::InitAudio()
{
	int audio_enabled = 1;
	ini_get(file_getfullpath(GetIniFile()), "AUDIO", "enabled") >> audio_enabled;
	if(!audio_enabled) return true; // no audio
	
	if(!A9_Init(E9_GetHWND(),A9_API_DEFAULT)) return false;
	return true;	
}

bool cDizApp::InitVideo()
{
	// load config
	Api api;
	r9Cfg cfg;
	g_cfg.LoadRenderCfg(cfg, api);

	// init interface
	if(!R9_InitInterface(api)) return false;

	BOOL ok = R9_Init(E9_GetHWND(),&cfg,api);
	if(!ok) // try the other api
	{
		dlog(LOGERR, L"RENDER: init %S failed, try the other api.\n", api == Api::OpenGL ? "OpenGL":"DirectX9");
		api = api == Api::DirectX ? Api::OpenGL : Api::DirectX;
		ok = R9_Init(E9_GetHWND(),&cfg, api);
		if(!ok)	return false;
	}

	R9_SetFilter(Filter::Point);
	App.Windowed(cfg.windowed);

	return true;
}

void cDizApp::Done()
{
	// must be able to destroy partial init too, in case Init has failed

	// game
	g_sound.Done();
	g_paint.Done();

	// engine
	R9_Done();
	R9_DoneInterface();
	A9_Done();
	I9_Done();
	F9_Done();

	dlog(LOGAPP, L"App done.\n");
}

void cDizApp::Activate( bool active )
{
	if(active)
	{
		if(I9_IsReady()) I9_Acquire();
		g_game.fffx.Update();
	}
	else
	{
		if(I9_IsReady()) I9_Unacquire();
		musicwaspaused = g_sound.music.paused();
	}
	if(!musicwaspaused) g_sound.music.Pause(!active);
}

bool cDizApp::ToggleVideo()
{
	if(!R9_GetCfg().windowed) return false; // toggle only in windowed mode (not a hw restriction though)

	dlog(LOGAPP, L"Toggle video.\n");
	int scrwidth = sys_desktopwidth();
	int scrheight = sys_desktopheight();
	static bool maximized = false;

	maximized = !maximized; // toggle

	// make cfg
	Api api;
	r9Cfg cfg;
	g_cfg.LoadRenderCfg(cfg, api);

	if(maximized) // overwrite width and height - pseudo full screen
	{
		cfg.width	= scrwidth;
		cfg.height	= scrheight;
	}

	//unaquire
	g_paint.Unacquire();
	R9_Done();
	
	// re-init render
	BOOL ok = R9_Init(E9_GetHWND(), &cfg, api);
	if(!ok) // try to go back
	{
		dlog(LOGERR, L"RENDER: re-init failed; trying to restore original cfg.\n");
		g_cfg.LoadRenderCfg(cfg, api);
		if(!R9_Init(E9_GetHWND(), &cfg, api))	{ dlog(LOGERR, L"RENDER: critical error!\n"); return false; }
	}

	g_cfg.m_scale = 0; // full scale

	// reacquire
	R9_SetFilter(Filter::Point);
	App.Windowed(R9_GetCfg().windowed);
	App.SetCursor(R9_GetCfg().windowed ? Cursor::Arrow : Cursor::None);
	g_dizdebug.Layout();
	g_paint.Reacquire();
	g_paint.Layout();

	return true;
}

bool cDizApp::Update()
{
	// timing
	static int timergame = 0;	// timer for game
	static int timersec = 0;		// timer for one sec
	static int gameframecount = 0;
	
	timersec += App.DeltaTime();
	if(timersec >= 1000)
	{
		timersec %= 1000;
		gamefps = gameframecount;
		gameframecount = 0;
	}

	// input
	float dtime = App.DeltaTime() / 1000.0f;
	if(I9_IsReady()) I9_Update(dtime);

	g_sound.Update(); // update sounds

	timergame += App.DeltaTime();
	int gamefps = g_game.fps();
	if(gamefps < 1) gamefps = 1;
	int gameframetime = 1000 / gamefps;
	if(timergame >= gameframetime)
	{
		timergame %= gameframetime;
		gameframecount++;

		if(!g_game.Update()) return false;

		if(g_dizdebug.active())
			g_script.debug(); // debug script callback
	}

	// debug
	if(!g_dizdebug.Update()) return false;

	// functional keys
	if( I9_IsReady() )
	{
		bool ctrl = I9_GetKeyValue(I9K_LCONTROL) || I9_GetKeyValue(I9K_RCONTROL);
		if(I9_GetKeyDown(I9K_F1) && App.Windowed()) DialogBox(E9_GetHINSTANCE(), MAKEINTRESOURCE(IDD_INFO), E9_GetHWND(), DialogProcInfo);
		if(I9_GetKeyDown(I9K_F10) && !ToggleVideo()) return false;
		if(I9_GetKeyDown(I9K_F11) ) drawstats = !drawstats;
		if(I9_GetKeyDown(I9K_F9) && A9_IsReady())	// toggle volume
		{
			static int volume = -1;
			int vol;
			if(volume == -1)
			{
				volume = A9_VolumeDecibelToPercent(A9_Get(A9_MASTERVOLUME));
				vol = 0;
				dlog(LOGAPP, L"sound off\n");
			}
			else
			{
				vol = volume;
				volume = -1;
				dlog(LOGAPP, L"sound on\n");
			}
			A9_Set(A9_MASTERVOLUME, A9_VolumePercentToDecibel(vol));
		}
		if(I9_GetKeyDown(I9K_SYSRQ)) // print screen
		{
			fRect r(0,0,R9_GetWidth(),R9_GetHeight());
			R9_SaveScreenShot(&r, !ctrl);
		}
	}
	return true;
}

void cDizApp::Draw()
{
	if(!R9_IsReady()) return; // avoid painting if render is not ready
	R9_CheckDevice(); // check for lost device
	if(R9_BeginScene())
	{
		R9_Clear(g_game.mapColor() | 0xff000000);
		g_game.Draw();
		if(drawstats) DrawStats();
		g_dizdebug.Draw();
		R9_EndScene();
		R9_Present();
	}
}

void cDizApp::DrawStats()
{
	std::ostringstream o;
	o << "obj:"<<g_game.m_obj.size() << ", "
		 "brs:"<< g_game.m_visible_brushes << ", "
		 "fps:"<< gamefps<< "/" << App.FPS();

	std::string str = o.str();
	fV2 sz = fV2(ChrW * str.size(), ChrH) + 4;
	fV2 p(R9_GetWidth() - sz.x - 2, 2.0f);

	R9_DrawBar(fRect(p, p + sz), 0xa0000000);
	R9_DrawText(p + 2, str, 0xffffff80);
	R9_Flush();
}

void cDizApp::ErrorMessage(LPCWSTR msg)
{
	dlog(LOGERR, L"DizzyAGE ERROR:\n%s\n", msg);
	sys_msgbox( E9_GetHWND(), msg, L"DizzyAGE ERROR", MB_OK );
}

///////////////////////////////////////////////////////////////////////////////////////////////////
