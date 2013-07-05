//////////////////////////////////////////////////////////////////////////////////////////////////
// DizScript.h
//////////////////////////////////////////////////////////////////////////////////////////////////
#ifndef __DIZSCRIPT_H__
#define __DIZSCRIPT_H__
#include <unordered_map>

#include "SWI-cpp-m.h"
#include "R9Render.h"
#include <string>

//////////////////////////////////////////////////////////////////////////////////////////////////
// Script class
//////////////////////////////////////////////////////////////////////////////////////////////////

template<class T>
class EnumAtom
{
protected:
	std::unordered_map<T, atom_t> EnumToAtom;
	std::unordered_map<atom_t, T> AtomToEnum;

	void Add(T v, const char *s)
	{
		atom_t a = PL_new_atom(s);
		EnumToAtom[v] = a;
		AtomToEnum[a] = v;
	}
public:
	bool Unify(PlTerm & t, T v) const 
	{
		auto i = EnumToAtom.find(v);
		if ( i == EnumToAtom.end())
			return false;
		return t = i->second;
	}
	T Get(PlTerm t) const 
	{
		auto i = AtomToEnum.find(PlAtom(t));
		if ( i == AtomToEnum.end())
			throw PlTypeError("enum", t.ref);
		return i->second;
	}
};


class BlendAtom : public EnumAtom<Blend>
{
public:
	BlendAtom() {
		Add(Blend::Opaque, "opaque");
		Add(Blend::Alpha, "alpha");
		Add(Blend::Add, "add");
		Add(Blend::Mod, "mod");
		Add(Blend::Mod2, "mod2");
		Add(Blend::AlphaRep, "alphaRep");
	}

};


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
	cDizScript();
	bool Init();
	void Start() { gameStart(); }

	void gameInit() { CallHandler(_gameInit); }
	void gameStart() { CallHandler(_gameStart); }
	void gameUpdate() { CallHandler(_gameUpdate); }
	void gameAfterUpdate() { CallHandler(_gameAfterUpdate); }
	void roomOpen() { CallHandler(_roomOpen); }
	void roomClose() { CallHandler(_roomClose); }
	void roomOut() { CallHandler(_roomOut); }
	void collision(const std::string & id, int mode) { PlTermv av(2); av[0] = id; av[1] = mode; CallHandler(_collision, av) ; }
	void fall() { CallHandler(_fall); }
	int jump(int mat, int clean) { PlTermv av(3); av[0] = mat; av[1] = clean; CallHandler(_jump, av); return av[2]; }
	void playerUpdate() { CallHandler(_playerUpdate); }
	void action() { CallHandler(_action); }
	void menu() { CallHandler(_menu); }
	void drawHud() { CallHandler(_drawHud); }
	void musicLoop() { CallHandler(_musicLoop); }
	void debug() { CallHandler(_debug); }		
	void reloadMap() { CallHandler(_reloadMap); }

	std::string UpdateStack() const;
};

extern cDizScript g_script;

#endif
//////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////
