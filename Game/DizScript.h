//////////////////////////////////////////////////////////////////////////////////////////////////
// DizScript.h
//////////////////////////////////////////////////////////////////////////////////////////////////
#ifndef __DIZSCRIPT_H__
#define __DIZSCRIPT_H__

#include "SWI-cpp-m.h"

#include <string>

//////////////////////////////////////////////////////////////////////////////////////////////////
// Script class
//////////////////////////////////////////////////////////////////////////////////////////////////
class cDizScript
{
		module_t		module;
		predicate_t		_event;
		functor_t _gameInit;
		functor_t _gameStart;
		functor_t _gameUpdate;
		functor_t _gameAfterUpdate;
		functor_t _roomOpen;
		functor_t _roomClose;
		functor_t _roomOut;
		functor_t _collision;
		functor_t _fall;
		functor_t _jump;
		functor_t _playerUpdate;
		functor_t _action;
		functor_t _menu;
		functor_t _drawHud;
		functor_t _musicLoop;
		functor_t _debug;		
		functor_t _reloadMap;

		void CallHandler (functor_t handler, const PlTermv &av = 0);
	
public:
						cDizScript			();
		bool			Init				();					// init
		void			Start				();					// call this to start a new game
		void			Update				();					// update and run current code

		void gameInit() { CallHandler(_gameInit); }
		void gameStart() { CallHandler(_gameStart); }
		void gameUpdate() { CallHandler(_gameUpdate); }
		void gameAfterUpdate() { CallHandler(_gameAfterUpdate); }
		void roomOpen() { CallHandler(_roomOpen); }
		void roomClose() { CallHandler(_roomClose); }
		void roomOut() { CallHandler(_roomOut); }
		void collision(int objIdx, int mode) { PlTermv av(2); av[0] = objIdx; av[1] = mode; CallHandler(_collision, av) ; }
		void fall() { CallHandler(_fall); }
		int jump(int mat, int clean) { PlTermv av(3); av[0] = mat; av[1] = clean; CallHandler(_jump, av); return av[2]; }
		void playerUpdate() { CallHandler(_playerUpdate); }
		void action() { CallHandler(_action); }
		void menu() { CallHandler(_menu); }
		void drawHud() { CallHandler(_drawHud); }
		void musicLoop() { CallHandler(_musicLoop); }
		void debug() { CallHandler(_debug); }		
		void reloadMap() { CallHandler(_reloadMap); }

		// register
//		void			ScriptRegister		( gsVM* vm );		// register script functions and constants
		std::string UpdateStack() const;
};

extern cDizScript g_script;

#endif
//////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////
