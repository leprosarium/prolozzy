///////////////////////////////////////////////////////////////////////////////////////////////////
// DizDebug.cpp
///////////////////////////////////////////////////////////////////////////////////////////////////
#include "StdAfx.h"
#include "DizDebug.h"
#include "DizGame.h"
#include "SWI-cpp-m.h"

#include "SWI-Stream.h"

#include "E9App.h"

cDizDebug g_dizdebug;

BOOL cDizDebug::m_developer = false;

bool IS_WORD_CHAR(char c)
{
	return ('0'<=c && c<='9') || ('A'<=c && c<='Z') || ('a'<=c && c<='z') || c=='_';
}
#define COLOR_INFO		0xff408040
#define COLOR_SLOTS		0xff004080
#define COLOR_INPUT		0xff808080
#define COLOR_BBKGR		0xa0000000


int PrologDebugDraw()
{
	g_dizdebug.PrologDraw();
	return 0;
}

void cDizDebug::PrologDraw()
{
	if(!R9_IsReady()) return; // avoid painting if render is not ready
	R9_CheckDevice(); // check for lost device
	if(R9_BeginScene())
	{
		R9_Clear(g_game.mapColor()|0xff000000);
		g_dizdebug.ConsoleDraw();
		g_dizdebug.SlotDraw();
		g_dizdebug.InputDraw();

		R9_EndScene();
		R9_Present();
	}
}

void cDizDebug::PrologUpdate()
{
	float dtime = (float)E9_AppGetInt(E9_APP_DELTATIME) / 1000.0f;
	if(I9_IsReady()) I9_Update(dtime);
	ConsoleUpdate();
}

ssize_t cDizDebug::PrologRead(char *buffer, size_t size)
{

	e9AppCallback pnt = E9_AppSetCallback(E9_APP_ONPAINT, PrologDebugDraw);
	MSG	msg;

	char* szline = m_con_lines + m_con_nextline * CON_LINESIZE + 1;
	strcpy(m_input_cmd, szline);
	size_t sz = strlen(m_input_cmd);
	m_input_open = true;
	m_input_crt = sz;
 	while(true)
	{
		while( PeekMessage( &msg, NULL, 0, 0, PM_NOREMOVE ) )
		{
			if( GetMessage( &msg, NULL, 0, 0) )
			{
				TranslateMessage( &msg );
				DispatchMessage( &msg );
			}
			else
			{
				return 0;
			}
		}
		PrologDraw();
		PrologUpdate();
		bool shift = I9_GetKeyValue(I9K_LSHIFT) || I9_GetKeyValue(I9K_RSHIFT);
		size_t pos = 0;
		for(int i=0;i<I9_GetKeyQCount();i++)
		{
			if(!I9_GetKeyQValue(i))
				continue;
			int key = I9_GetKeyQCode(i);
			char ascii = shift ? I9_GetKeyShifted(key) : I9_GetKeyAscii(key);
			if(key == I9K_RETURN)
				ascii = '\n';
			if(!ascii) 
				continue;
			buffer[pos++]=ascii;
			m_input_cmd[sz] = ascii;
			m_input_cmd[++sz] = 0;
			m_input_crt = sz;
			m_input_complete = false;
		}
		if(pos)
		{
			D9_LogBuf(LOGDBG, buffer, pos);
			E9_AppSetCallback(E9_APP_ONPAINT, pnt);
			return pos;
		}
		Sleep(10);
	}
	return 0;
}


static ssize_t Log_read(void *handle, char *buffer, size_t size)
{
	return g_dizdebug.PrologRead(buffer, size);

}

///////////////////////////////////////////////////////////////////////////////////////////////////
cDizDebug::cDizDebug()
{
	Soutput->functions->read = Log_read;

	m_con_lines = 0;
	m_con_pagetop = 0;
	m_con_nextline = 0;
	memset(m_slot,0,sizeof(m_slot));
	m_input_open = 0;
	m_input_crt = 0;
	m_input_complete = 0;
	memset(m_input_cmd,0,sizeof(m_input_cmd));
	m_input_historycrt = 0;
	m_input_historycnt = 0;
	memset(m_input_history,0,sizeof(m_input_history));
	m_renderw = 0;
	m_renderh = 0;
	m_console = false;
}


bool cDizDebug::Init()
{
	Layout();
	
	// config
	char inifile[256];
	strcpy( inifile, file_getfullpath(GetIniFile()) );
	ini_getint( inifile, "ADVANCED",	"dev",		&m_developer );
	if(*g_cfg.GetInfoValue("game_protection")) 
		m_developer=false; // no developer for protected games

	// console
	m_console = false;
	m_con_lines = new char[CON_LINES*CON_LINESIZE];
	memset(m_con_lines,0,sizeof(CON_LINES*CON_LINESIZE));
	m_con_pagetop = 0;
	m_con_nextline = 0;
	D9_LogSetCallback( Con_LogCallback );

	//log
	dlog(LOGAPP, L"%S v%S\n",GAME_NAME,GAME_VERSION);
	R9_LogCfg(R9_GetCfg(),R9_GetApi());
	if(m_developer) dlog(LOGAPP, L"Developer mode on.\n");

	return true;
}

void cDizDebug::Done()
{
	// console
	delete [] m_con_lines;
	m_con_lines = 0;
}

bool cDizDebug::Update()
{
	if(!I9_IsReady()) return true;

	// debug developer hidden key
	if(DeveloperKey() && !InputIsOpened())
	{
		m_developer = !m_developer; 
		g_paint.Layout();
	}
	if(!m_developer) return true;

	// console input
	InputUpdate(); // update console input command
	if(InputIsOpened()) // if during console input command
	{
		g_player.m_debug = 1; // don't update player
		return true;
	}

	// console
	ConsoleUpdate();

	// navigation
	NavigationUpdate();

	// draw mode
	if(I9_GetKeyDown(I9K_F4))
	{
		g_game.m_drawmode++;
		if(g_game.m_drawmode>=DRAWMODE_MAX) g_game.m_drawmode=0;
		dlog( LOGAPP, L"draw mode %i\n", g_game.m_drawmode );
	}

	// map reload full WARNING: all items are reloaded too, so your inventory...
	if(I9_GetKeyDown(I9K_F6))
	{
		g_map.Reload();
		g_game.ObjGather();
	}
	
	// resize
	bool ctrl  = I9_GetKeyValue(I9K_LCONTROL) || I9_GetKeyValue(I9K_RCONTROL);
	bool shift = I9_GetKeyValue(I9K_LSHIFT) || I9_GetKeyValue(I9K_RSHIFT);
	if(ctrl && I9_GetKeyDown(I9K_MINUS)) // -
	{
		if(g_cfg.m_scale==0) g_cfg.m_scale = g_paint.m_scale;
		g_cfg.m_scale--;
		if(g_cfg.m_scale==0) g_cfg.m_scale=1;
		g_paint.Layout();
	}
	else
	if(ctrl && I9_GetKeyDown(I9K_EQUALS)) // +
	{
		g_cfg.m_scale++;
		if( g_game.m_screen_bw * g_cfg.m_scale > R9_GetWidth() ||
			g_game.m_screen_bh * g_cfg.m_scale > R9_GetHeight() )
			g_cfg.m_scale--;
		g_paint.Layout();
	}

	return true;
}

#ifdef _DEBUG
//#define GAME_DRAWDEBUG				// draw debug (only on developper mode)
#endif

void cDizDebug::Draw()
{
	if(!m_developer) return;
	if(!m_console)
	{
		R9_DrawBar(fRect(2,2,4+8*R9_CHRW,4+R9_CHRH),0xffff0000);
		R9_DrawText(fV2(4,4),"DEV-MODE",0xffffffff);
		return;
	}
	InfoDraw();
	ConsoleDraw();
	SlotDraw();
	InputDraw();

}

void cDizDebug::Layout()
{
	m_renderw = R9_GetWidth();
	m_renderh = R9_GetHeight();
}

bool cDizDebug::DeveloperKey()
{
	// quick type D E V E L O P E R to toggle
	static int tickold = 0;
	static char keybuf[16] = {32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,0};
	int keycnt = 16;
	if( I9_GetKeyQCount() )
	{
		for(int i=0;i<I9_GetKeyQCount();i++)
		{
			if(!I9_GetKeyQValue(i)) // up
			{
				char chr = I9_GetKeyAscii(I9_GetKeyQCode(i));
				if(chr<=0) chr=32;
				keybuf[keycnt-1] = chr; 
				memmove(keybuf,keybuf+1,keycnt-1);
				keybuf[keycnt-1]=0;
			}
		}
		if(strstr(keybuf,"developer"))
		{
			memset(keybuf,32,keycnt-1);
			return true;
		}
	}
	// erase each 10 sec
	int tick = sys_gettickcount();
	if(tick-tickold<10000)
	{
		tickold=tick;
		memset(keybuf,32,keycnt-1);
	}
	return false;
}

///////////////////////////////////////////////////////////////////////////////////////////////////
// INFO
///////////////////////////////////////////////////////////////////////////////////////////////////
void cDizDebug::InfoDraw()
{
	float x = 0.0f;
	float y = (float)m_renderh - INFO_LINES*R9_CHRH;
	char sz[256]; sz[0]=0;

	R9_DrawBar(fRect(x,y,(float)m_renderw,(float)m_renderh),COLOR_BBKGR);

	x+=(float)R9_CHRW;

	// script
	std::string stack = g_script.UpdateStack();

//	gs_cpycall(g_script.m_vm1,sz,256); sz[255]=0;
//	char sz2[64]; sprintf(sz2," sp=%i", g_script.m_vm1->m_sp);
//	strncat(sz,sz2,strlen(sz2));
//	R9_DrawText( fV2(x, y), sz, COLOR_INFO); y+=R9_CHRH;
	R9_DrawText( fV2(x, y), stack.c_str(), COLOR_INFO); y+=R9_CHRH;

	// player
	sprintf( sz, "rx=%i, ry=%i, px=%i, py=%i", g_game.roomX(), g_game.roomY(), g_player.x(), g_player.y() );
	R9_DrawText( fV2(x, y), sz, COLOR_INFO); y+=R9_CHRH;

}

///////////////////////////////////////////////////////////////////////////////////////////////////
// NAVIGATION
///////////////////////////////////////////////////////////////////////////////////////////////////
void cDizDebug::NavigationUpdate()
{
	bool ctrl  = I9_GetKeyValue(I9K_LCONTROL) || I9_GetKeyValue(I9K_RCONTROL);
	bool shift = I9_GetKeyValue(I9K_LSHIFT) || I9_GetKeyValue(I9K_RSHIFT);

	int mw = g_map.Width();
	int mh = g_map.Height();
	int rw = Room::Width;
	int rh = Room::Height;
	int px = g_player.x();
	int py = g_player.y();
	if(ctrl)
	{
		if(I9_GetKeyDown(I9K_LEFT))		px-=rw;
		if(I9_GetKeyDown(I9K_RIGHT))	px+=rw;
		if(I9_GetKeyDown(I9K_UP))		py-=rh;
		if(I9_GetKeyDown(I9K_DOWN))		py+=rh;
	}
	else
	if(shift)
	{
		if(I9_GetKeyDown(I9K_LEFT))		px-=4;
		if(I9_GetKeyDown(I9K_RIGHT))	px+=4;
		if(I9_GetKeyDown(I9K_UP))		py-=4;
		if(I9_GetKeyDown(I9K_DOWN))		py+=4;
	}
	
	if(px>=mw*rw) px=mw*rw-1;
	if(py>=mh*rh) py=mh*rh-1;
	if(px<0) px=0;
	if(py<0) py=0;
	g_player.x(px);
	g_player.y(py);
	g_player.m_debug = (ctrl||shift);
}

///////////////////////////////////////////////////////////////////////////////////////////////////
// CONSOLE
///////////////////////////////////////////////////////////////////////////////////////////////////
iRect cDizDebug::ConsoleGetRect()
{
	int scrw = g_game.m_screen_w * g_paint.m_scale;
	int scrh = g_game.m_screen_h * g_paint.m_scale;
	iRect rect;
	rect.x1 = 0;
	rect.y1 = 0;//scrh;
	rect.x2 = m_renderw;
	rect.y2 = m_renderh - (INFO_LINES+1)*R9_CHRH;
	if(rect.y1>rect.y2) rect.y1=rect.y2;
	return rect;
}

void cDizDebug::ConsoleUpdate()
{
	if(I9_GetKeyDown(I9K_GRAVE) || I9_GetKeyDown(I9K_F2)) // show-hide
	{
		m_console=!m_console;
		g_paint.Layout();
	}
	if(!m_console) return;

	iRect rect = ConsoleGetRect();
	int pageh=rect.Height() / R9_CHRH - 1; // page height in lines
	int step = (I9_GetKeyValue(I9K_LSHIFT) || I9_GetKeyValue(I9K_RSHIFT)) ? 1 : pageh;
	if(I9_GetKeyDown(I9K_PGUP))		m_con_pagetop -= step; else
	if(I9_GetKeyDown(I9K_PGDN))		m_con_pagetop += step; else
	if(I9_GetKeyDown(I9K_HOME))		m_con_pagetop = 0; else
	if(I9_GetKeyDown(I9K_END))		m_con_pagetop = CON_LINES;
	// clamp
	if(m_con_pagetop>m_con_nextline-pageh) m_con_pagetop=m_con_nextline-pageh;
	if(m_con_pagetop<0) m_con_pagetop=0;
}

void cDizDebug::ConsoleDraw()
{
	fRect rect = ConsoleGetRect();
	fRect oldclip = R9_GetClipping();
	R9_SetClipping(rect);
	R9_DrawBar(rect,COLOR_BBKGR);
	rect.x1 += R9_CHRW;
	rect.y1 += R9_CHRH;
	int line = m_con_pagetop;
	float y=rect.y1;
	while(y<rect.y2) // show as many as you can
	{
		if(line==m_con_nextline) break;
		char* szline = m_con_lines + line*CON_LINESIZE;
		int ch = szline[0];
		dword color = D9_LogGetChannelColor(ch);
		R9_DrawText(fV2(rect.x1,y),szline+1,color);
		line++;
		y += (float)R9_CHRH;
	}
	R9_SetClipping(oldclip);
}

void cDizDebug::ConsolePush( int ch, LPCWSTR msg )
{
	if(msg==NULL) return;
	char* szline = &m_con_lines[m_con_nextline*CON_LINESIZE];
	if(szline[1]==0) szline[0] = ch; // set line channel for first messages on a line
	int m=0; // msg pos
	int l=1+(int)strlen(szline+1); // line position
	iRect rect = ConsoleGetRect();
	int pageh = rect.Height() / R9_CHRH - 1; // page height in lines
	while(true)
	{
		if(msg[m]==0) break; 
		if(msg[m]=='\n' || l==CON_LINESIZE-1) // new line
		{
			szline[l]=0; // eol
			// next line
			bool bottom = ( m_con_nextline == m_con_pagetop+pageh );
			m_con_nextline++;
			if(m_con_nextline==CON_LINES) // full, lose begining
			{
				int loose = (CON_LINES/4);
				memcpy(m_con_lines, m_con_lines+loose*CON_LINESIZE, (CON_LINES-loose)*CON_LINESIZE);
				m_con_nextline = CON_LINES-loose;
			}
			if(bottom) m_con_pagetop = (m_con_nextline-pageh);
			szline = m_con_lines + m_con_nextline*CON_LINESIZE;
			szline[0] = ch; // set line channel
			szline[1] = 0; // clear line
			l=1;
			m++;
			continue;
		}
		else
		{
			szline[l] = msg[m];
			l++;
			m++;
		}
	}
	szline[l] = 0; // eol
}

void cDizDebug::Con_LogCallback( int ch, LPCWSTR msg )
{
	if(g_dizdebug.m_con_lines==NULL) return; // not yet
	g_dizdebug.ConsolePush( ch, msg );
}

///////////////////////////////////////////////////////////////////////////////////////////////////
// SLOTS
///////////////////////////////////////////////////////////////////////////////////////////////////
iRect cDizDebug::SlotGetRect()
{
	int scrw = g_game.m_screen_w*g_paint.m_scale;
	int scrh = g_game.m_screen_h*g_paint.m_scale;
	iRect rect;
	rect.x1 = scrw;
	rect.y1 = 0;
	rect.x2 = m_renderw;
	rect.y2 = MIN(rect.y1+R9_CHRH*SLOT_COUNT,scrh);
	if(rect.x1>rect.x2) rect.x1=rect.x2;
	if(rect.y1>rect.y2) rect.y1=rect.y2;
	return rect;
}

void cDizDebug::SlotDraw()
{
	fRect rect = SlotGetRect();
	fRect oldclip = R9_GetClipping();
	R9_SetClipping(rect);
	R9_DrawBar(rect,COLOR_BBKGR);
	for(int i=0;i<SLOT_COUNT;i++)
		R9_DrawText(fV2(rect.x1+R9_CHRW,rect.y1+i*R9_CHRH),m_slot[i],0xff0040a0);
	R9_SetClipping(oldclip);
}

void cDizDebug::SlotSet( int slot, char* text )
{
	if(slot<0 || slot>=SLOT_COUNT) return;
	strncpy(m_slot[slot],text?text:"",SLOT_SIZE);
	m_slot[slot][SLOT_SIZE-1]=0;
}

///////////////////////////////////////////////////////////////////////////////////////////////////
// INPUT
///////////////////////////////////////////////////////////////////////////////////////////////////
void cDizDebug::InputUpdate()
{
	if(!m_console) return;
	if(I9_GetKeyDown(I9K_INSERT) && !m_input_open) 
	{ 
		m_input_open = true; 
		m_input_complete = 0; 
		m_input_historycrt=-1; 
		return;
	}
	if(!m_input_open) return;

	int i;
	int len = (int)strlen(m_input_cmd);

	// action keys
	bool ctrl = I9_GetKeyValue(I9K_LCONTROL) || I9_GetKeyValue(I9K_RCONTROL);
	bool shift = I9_GetKeyValue(I9K_LSHIFT) || I9_GetKeyValue(I9K_RSHIFT);

	if(I9_GetKeyUp(I9K_ESCAPE) || I9_GetKeyDown(I9K_INSERT))
	{	
		m_input_open = false; 
		m_input_complete = 0; 
		return; 
	}
	else
	if(I9_GetKeyUp(I9K_RETURN)) 
	{ 
		m_input_open = false; 
		m_input_complete = 0; 
		InputExecute(); 
		return; 
	}
	else
	if(I9_GetKeyDown(I9K_LEFT) && m_input_crt>0)	
	{
		if(ctrl) 
			InputSkipWord(-1); 
		else 
			m_input_crt--;
		m_input_complete = 0;
	}
	else
	if(I9_GetKeyDown(I9K_RIGHT) && m_input_crt<len)
	{
		if(ctrl) 
			InputSkipWord(1); 
		else 
			m_input_crt++;
		m_input_complete = 0;
	}
	else
	if(I9_GetKeyDown(I9K_BACKSPACE) && m_input_crt>0) // delete back - shift left
	{
		m_input_crt--;
		for(int j=m_input_crt;j<len;j++)
			m_input_cmd[j] = m_input_cmd[j+1];
		m_input_complete = 0;
	}
	else
	if(I9_GetKeyDown(I9K_DELETE) && m_input_crt<len) // delete - shift left
	{
		if(ctrl)
			m_input_cmd[m_input_crt]=0; // crop
		else
			for(int j=m_input_crt;j<len;j++)
				m_input_cmd[j] = m_input_cmd[j+1];
		m_input_complete = 0;
	}
	else
	if(I9_GetKeyDown(I9K_HOME)) { m_input_crt=0; m_input_complete = 0; }
	else
	if(I9_GetKeyDown(I9K_END)) { m_input_crt=len; m_input_complete = 0; }
	else
	if(I9_GetKeyDown(I9K_TAB) ) // autocomplete
	{
		InputAutoComplete();
	}
	else
	if(I9_GetKeyDown(I9K_DOWN))
	{
		if(m_input_historycrt==-1) m_input_historycrt=0; else m_input_historycrt--;
		if( m_input_historycrt<0 ) m_input_historycrt=0;
		strcpy(m_input_cmd,m_input_history[m_input_historycrt]);
		m_input_crt = (int)strlen(m_input_cmd);
		m_input_complete = 0;
	}
	else
	if(I9_GetKeyDown(I9K_UP))
	{
		if(m_input_historycrt==-1) m_input_historycrt=0; else m_input_historycrt++;
		if( m_input_historycrt>m_input_historycnt-1 ) m_input_historycrt=m_input_historycnt-1;
		strcpy(m_input_cmd,m_input_history[m_input_historycrt]);
		m_input_crt = (int)strlen(m_input_cmd);
		m_input_complete = 0;
	}
	else
	{
		// char keys
		if(len==INPUT_SIZE) return; // full
		for(i=0;i<I9_GetKeyQCount();i++)
		{
			if(!I9_GetKeyQValue(i)) continue;
			int key = I9_GetKeyQCode(i);
			char ascii = shift ? I9_GetKeyShifted(key) : I9_GetKeyAscii(key);
			if(!ascii) continue;
			// shift right (including 0)
			if(m_input_crt<len)	
				memmove( m_input_cmd+m_input_crt+1, m_input_cmd+m_input_crt, len-m_input_crt);
			// insert
			len++;
			m_input_cmd[m_input_crt]=ascii;
			m_input_cmd[len]=0;
			m_input_crt++;
			m_input_complete = 0;
		}
	}

	
}

void cDizDebug::InputDraw()
{
	if(!m_input_open) return;
	fRect rect;
	rect.x1 = 0.0f;
	rect.y1 = (float)m_renderh - (INFO_LINES+1)*R9_CHRH;
	rect.x2 = (float)m_renderw;
	rect.y2 = rect.y1+R9_CHRH;
	R9_DrawBar(rect,COLOR_BBKGR);
	R9_DrawText(fV2(rect.x1,rect.y1),">",COLOR_INPUT);
	rect.x1 += R9_CHRW;
	int t = sys_gettickcount()%800;
	float crtx = rect.x1+R9_CHRW*m_input_crt-1;
	if(t<500) R9_DrawLine(fV2(crtx,rect.y1-1),fV2(crtx,rect.y2+1));
	//if(t<400) R9_DrawLine(fV2(crtx,rect.y2),fV2(crtx+R9_CHRW,rect.y2));
	//if(t<500) R9_DrawBar(fRect(crtx,rect.y1-1,crtx+R9_CHRW,rect.y2+1),0xff808080);
	R9_DrawText(fV2(rect.x1,rect.y1),m_input_cmd,COLOR_INPUT);
}

void cDizDebug::InputExecute()
{
	if(m_input_cmd[0]==0) return; // empty
	// history
	memmove(m_input_history[1],m_input_history[0],(INPUT_HISTORY-1)*INPUT_SIZE);
	strcpy(m_input_history[0],m_input_cmd);
	if(m_input_historycnt<INPUT_HISTORY) m_input_historycnt++;

	// run
	//dlog(m_input_cmd); dlog("\n");
	//gsVM* vm = g_script.m_vm0;
	//int debug = gs_getdebug(vm);
	//gs_setdebug(vm,GSDBG_LOGDESC);
	//gs_dostring(vm,m_input_cmd);
	//gs_setdebug(vm,debug);


	try
	{
		PlCall(m_input_cmd);
	}
	catch(PlException const & e)
	{
		PlException ee(e);
		dlog(L"PlException: %s", static_cast<LPCWSTR>(ee));
	}

	// clear cmd
	m_input_cmd[0]=0;
	m_input_crt=0;
}

void cDizDebug::InputSkipWord( int dir )
{
	int len = (int)strlen(m_input_cmd);
	int cnt = 0;
	while( (dir<0 && 0<m_input_crt) || (dir>0 && m_input_crt<len) )
	{
		char c = m_input_cmd[m_input_crt];
		if(!IS_WORD_CHAR(c)) break;
		m_input_crt += dir;
		cnt++;
	}
	if(cnt==0)
	{
		m_input_crt += dir;
		if(m_input_crt<0) m_input_crt=0;
		if(m_input_crt>len) m_input_crt=len;
	}
}

int common(const char *s1, const char *s2, int insensitive)
{ int n = 0;

  if ( !insensitive )
  { while(*s1 && *s1 == *s2)
    { s1++, s2++;
      n++;
    }
    return n;
  } else
  { while(*s1)
    { if ( tolower(*s1) == tolower(*s2) )
      { s1++, s2++;
	n++;
      } else
	break;
    }
    return n;
  }
}

void cDizDebug::InputAutoComplete()
{
	int i;

	// find word begining and ending
	int start = m_input_crt;
	while(start>0 && IS_WORD_CHAR(m_input_cmd[start-1])) start--;
	if(m_input_crt-start<=0) return; // nothing to start with
	int end = m_input_crt;
	while(IS_WORD_CHAR(m_input_cmd[end])) end++;

	

	if ( islower(m_input_cmd[start]) )
	{
		char buf_handle[INPUT_SIZE];
		size_t patlen = m_input_crt - start;

		strncpy(buf_handle, m_input_cmd + start, patlen);
		buf_handle[patlen] = 0;
		if(char * name = PL_atom_generator(buf_handle, 0))
		{
			char match[INPUT_SIZE];
			int nmatches = 1;
			size_t replace_from = start;
			size_t ncommon = strlen(name);
			size_t patlen = m_input_crt - replace_from;
			strcpy(match, name); 

			while(name = PL_atom_generator(buf_handle, 1) )
			{ 
				ncommon = common(match, name, 1);//data->case_insensitive);
				match[ncommon] = 0;
				nmatches++;
			}
			// insert found
			int len=(int)strlen(m_input_cmd);
			if(start+ncommon+(len-end)>=INPUT_SIZE) return; // no room
	
			// remove old word and insert new one
			if(len-end)	memmove(m_input_cmd+start+ncommon,m_input_cmd+end,len-end);
			memcpy(m_input_cmd+start,match,ncommon);
			m_input_cmd[start+ncommon+len-end]=0;
			m_input_crt = end = start+ncommon;
			int complete = 0; // counter
			if(nmatches > 1 && (name = PL_atom_generator(match, 0))) {
				if(complete == m_input_complete) 
					goto found; 
				else 
					complete++;

				while(name = PL_atom_generator(match, 1))
					if(complete == m_input_complete) 
						goto found; 
					else 
						complete++;
			}
			m_input_complete = 0;
			return;
found:
			m_input_complete++;
			if(!name) return; // safe
			int nlen = (int)strlen(name);
			len=(int)strlen(m_input_cmd);
			if(start+nlen+(len-end)>=INPUT_SIZE) return; // no room
	
			// remove old word and insert new one
			if(len-end)	memmove(m_input_cmd+start+nlen,m_input_cmd+end,len-end);
			memcpy(m_input_cmd+start,name,nlen);
			m_input_cmd[start+nlen+len-end]=0;
			return;
		}
	}
	m_input_complete = 0;
}


void cDizDebug::ConsumeInput()
{
	// wait until latent keys are consummed
	while(	I9_GetKeyValue(I9K_ESCAPE) ||
			I9_GetKeyValue(I9K_RETURN) )
			I9_Update(0);
}

///////////////////////////////////////////////////////////////////////////////////////////////////
