#ifndef __GAMEAPP_H__
#define __GAMEAPP_H__
#include "StdAfx.h"
#include "App.h"

class DizApp : public App
{
	int gamefps;			// game updates per second (logic fps)
	int drawstats;			// show stats
	bool musicwaspaused;	// if music was paused when application was deactivated

	bool InitApp();
	bool InitFiles();
	bool InitInput();
	bool InitAudio();
	bool InitVideo();
	void DrawStats();
public:
	DizApp(HINSTANCE hinstance, LPCTSTR cmdline);
	~DizApp();

	void OnActivate(bool);

	bool ToggleVideo();  

	bool Update();
	void Draw();
	static	DizApp * app;
};




#endif
///////////////////////////////////////////////////////////////////////////////////////////////////
