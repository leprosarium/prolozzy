#ifndef __DIZDEF_H__
#define __DIZDEF_H__

#include "E9System.h"
#include "E9Math.h"
#include "E9String.h"
#include "F9Files.h"
#include "A9Audio.h"
#include "R9Img.h"
#include "R9ImgLoader.h"
#include "R9Render.h"
#include "SWI-cpp-m.h"

#include <unordered_map>

typedef std::unordered_map<atom_t, int> IntIndex;

//////////////////////////////////////////////////////////////////////////////////////////////////
// System
//////////////////////////////////////////////////////////////////////////////////////////////////
#define GAME_NAME			"DizzyAGE"					// application name
#define GAME_VERSION		"2.4"						// application version (change this with each change, at least 2 digits)

#define GAME_SCRWB			320							// default screen width with borders
#define GAME_SCRHB			240							// default screen height with borders
#define GAME_SCRW			256							// default screen width
#define GAME_SCRH			192							// default screen height
#define GAME_ROOMW			240							// default room width
#define GAME_ROOMH			136							// default room height
#define GAME_VIEWX			8							// view (room) pos x
#define GAME_VIEWY			48							// view (room) pos y

#define GAME_LAYERS			8							// as many as in editor

#define GAME_FPS			36							// update every 27 ms => logic fps=37

#endif
//////////////////////////////////////////////////////////////////////////////////////////////////
