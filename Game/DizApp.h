///////////////////////////////////////////////////////////////////////////////////////////////////
// GameApp.h
///////////////////////////////////////////////////////////////////////////////////////////////////
#ifndef __GAMEAPP_H__
#define __GAMEAPP_H__

#include "E9System.h"
#include "D9Debug.h"
#include "R9Render.h"

///////////////////////////////////////////////////////////////////////////////////////////////////
// cDizApp
///////////////////////////////////////////////////////////////////////////////////////////////////
class cDizApp
{
public:
				cDizApp();

		bool	Init();
		bool	InitApp();
		bool	InitFiles();
		bool	InitInput();
		bool	InitAudio();
		bool	InitVideo();

		void	Done();
		void	Activate( BOOL active );
		bool	ToggleVideo();  

		bool	Update();
		void	Draw();
		void	DrawStats();
static	void	ErrorMessage(const char* msg );	// error message box

public:
		int		m_gamefps;					// game updates per second (logic fps)
		int		m_drawstats;				// show stats

private:
		bool	m_musicwaspaused;			// if music was paused when application was deactivated
};

inline void ERRORMESSAGE(const char*  msg )	 { cDizApp::ErrorMessage( msg ); }


#endif
///////////////////////////////////////////////////////////////////////////////////////////////////
