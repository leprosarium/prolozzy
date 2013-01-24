//////////////////////////////////////////////////////////////////////////////////
// GAME MOD file
//////////////////////////////////////////////////////////////////////////////////

// shaders
#def SHADER_OPAQUE			0		// opaque
#def SHADER_BLEND			1		// alpha blend
#def SHADER_ADD				2		// additive color - used for light
#def SHADER_MOD				3		// modulate color - used for darken
#def SHADER_MOD2			4		// modulate 2 color - used for detail
#def SHADER_ALPHAREP		5		// internal use for material view
#def SHADER_MAX				6		// max shaders
tab g_shadername;					// list with shader names

// brush props (40 per brush)
#def BRUSH_TYPE				16		// type: 0=static(for map drawing), 1=dynamic(for objects)
#def BRUSH_ID				17		// object or brush id (for search in game); 0=no id
#def BRUSH_MATERIAL			18		// material that brushes will write in material map if draw set correctly
#def BRUSH_DRAW				19		// draw mode: 0=don't draw, 1=draw in view, 2=write material in material map (brush only), 3=both (brush only)
#def BRUSH_DISABLE			20		// 0=enable, 1=disabled (don't draw, don't update)
#def BRUSH_DELAY			21		// frame delay ( should be updated only once in BRUSH_DELAY frames )
#def BRUSH_ANIM				22		// animation mode: 0=none, 1=play once, 2=play loop
#def BRUSH_COLLIDER			23		// collider mode: 0=none, 1=call handler, 2=hard collision, 3=both
#def BRUSH_CLASS			24		// generic object class
#def BRUSH_STATUS			25		// generic status
#def BRUSH_TARGET			26		// generic target id (used to link objects)
#def BRUSH_DEATH			27		// cause of death
#def BRUSH_USER				32		// generic user props

// indexes in props table
#def BRUSHPROP_NAME			0		// prop name
#def BRUSHPROP_TOOLTIP		1		// prop tooltip
#def BRUSHPROP_BROWSE		2		// browser code
#def BRUSHPROP_BROWSESEL	3		// browser selection list (for browse select)

// browser codes (users may add more)
#def USERBROWSE_NONE		0		// no special browser
#def USERBROWSE_SELECT		1		// select browser (choose from pull down list of options)
#def USERBROWSE_COLOR		2		// color picker browser (for color props)
#def USERBROWSE_CUSTOM		3		// for user properties callback is used

// materials (up to 32 materials)
tab g_matdensitycolor;				// colors for each material density
tab g_matcolor;						// colors for each material
tab g_matdensity;					// density values for each material
tab g_matname;						// name for each material

// brush props table
tab g_brushprop; 					// brush props table { name, tooltip, browsetype, [browse info] }

// view modes (users may add more)
int g_view;							// current view mode
int g_viewcount;					// view modes count
tab g_viewname;						// view modes names

// room info
int	g_roominfo;						// what kind of room info should be displayed (0=name,1-4=text,5-12=prop)



//////////////////////////////////////////////////////////////////////////////////
// Current Tool Brush draw callback
// called from code for current brush to adjust some props like animation
// refers to a temporary ToolBrush, so it don't mess the real brush
//////////////////////////////////////////////////////////////////////////////////
func MOD_BrushToolDraw()
{
	// animation
	if(ToolBrushGet(BRUSH_ANIM)!=0)
	{
		delay = ToolBrushGet(BRUSH_DELAY); 
		frame = gs_time() / 25;
		if(delay>0) frame /= delay;
		ToolBrushSet(BRUSH_FRAME,frame);
	}
}

//////////////////////////////////////////////////////////////////////////////////
// Initialize props of the current tool brush, specific to the current type
// Called from Brush Type button in the menu bar
//////////////////////////////////////////////////////////////////////////////////
func MOD_BrushNew( type )
{
	ToolBrushSet(BRUSH_SHADER,SHADER_BLEND);	// shader default
	ToolBrushSet(BRUSH_SCALE,100);				// scale default
	ToolBrushSet(BRUSH_ID,0); 					// safety reset
	ToolBrushSet(BRUSH_MATERIAL,0); 			// material reset
	ToolBrushSet(BRUSH_DISABLE,0);				// not disabled
	ToolBrushSet(BRUSH_DELAY,3);				// animation speed
	ToolBrushSet(BRUSH_ANIM,2);					// animation loop
	ToolBrushSet(BRUSH_COLLIDER,0);				// no collider
	ToolBrushSet(BRUSH_CLASS,0);				// class reset
	ToolBrushSet(BRUSH_STATUS,0);				// status reset
	ToolBrushSet(BRUSH_TARGET,0);				// target reset
	ToolBrushSet(BRUSH_DEATH,0);				// death reset
	if(type==0) 
	{
		ToolBrushSet(BRUSH_DRAW,3); // draw in img+mat
	}
	else
	{
		ToolBrushSet(BRUSH_DRAW,1); // visible
	}
	for(i=BRUSH_USER;i<BRUSH_MAX;i++)
		ToolBrushSet(i,0);
}

//////////////////////////////////////////////////////////////////////////////////
// room  info
//////////////////////////////////////////////////////////////////////////////////
func MOD_GetRoomInfoName(roominfo)
{
	if(roominfo==0) return "show room name";
	if(roominfo<=4)	return "show room text "+(str)(roominfo-1);
	if(roominfo==5)	return "show room props 0-3";
	if(roominfo==6)	return "show room props 4-7";
	return "";
}

func MOD_GetRoomInfo(rx, ry)
{
	if(g_roominfo==0) 	return RoomNamesGet(rx,ry);
	if(g_roominfo<=4)	return RoomTextsGet(rx,ry,g_roominfo-1);
	if(g_roominfo==5 || g_roominfo==6)
	{
		p = (g_roominfo-5)*4;
		sz="";
		for(i=0;i<4;i++)
		{
			sz += (str)RoomPropsGet(rx,ry,p+i);
			if(i<3) sz+=", ";
		}
		return sz;
	}
	return "";
}

//////////////////////////////////////////////////////////////////////////////////
// initialize module
//////////////////////////////////////////////////////////////////////////////////
func MOD_Init()
{
	// shaders
	g_shadername = {"opaque","blend","add","mod","mod2"}; // only those shaders are available for users
	
	// materials ( in DizzyAGE engine only 4 densities are allowed, but users may add up to 32 materials )
	g_matdensitycolor	= { 0xff000000, 0xff606060, 0xffa0a0a0, 0xffffffff };
	g_matdensity 		= { 0, 			0, 			0, 			0, 			1, 			1, 			1, 			2, 			3, 			3 }; // 0=void, 1=soft, 2=hard and 3=jump 
	g_matcolor 			= { 0xFF000000, 0xFF0060FF, 0xFFFF8000, 0xFFD00000, 0xFFC0C0C0, 0xFF909090, 0xFF707070, 0xFF006000, 0xFF008000, 0xFF00B000 } ; // up to 32 materials
	g_matname 			= { "air",		"water",	"hurt",		"kill",		"cloud",	"climb",	"wind",		"block",	"jumpfix",	"jumppro" }; // up to 32 materials
	
	// brushes props table (40 properties per brush)
	g_brushprop = 
	{	
		//name			tooltip								browse_type			browse_info
		
		{ "layer",		"layer",							USERBROWSE_NONE },
		{ "x*",			"x readonly",						USERBROWSE_NONE },
		{ "y*",			"y readonly",						USERBROWSE_NONE },
		{ "w*",			"w readonly",						USERBROWSE_NONE },
		{ "h*",			"h readonly",						USERBROWSE_NONE },
		{ "tile",		"tile id",							USERBROWSE_NONE },
		{ "frame",		"tile frame",						USERBROWSE_NONE },
		{ "map x1",		"map left",							USERBROWSE_NONE },
		{ "map y1",		"map top",							USERBROWSE_NONE },
		{ "map x2",		"map right",						USERBROWSE_NONE },
		{ "map y2",		"map bottom",						USERBROWSE_NONE },
		{ "flip",		"flip",								USERBROWSE_SELECT, {"none","flip x","flip y","flip xy","flip r","flip xr","flip yr","flip xyr"} },
		{ "color",		"color",							USERBROWSE_COLOR },
		{ "shader",		"shader",							USERBROWSE_SELECT, g_shadername },
		{ "scale",		"scale",							USERBROWSE_NONE },
		{ "select*",	"select",							USERBROWSE_NONE },
		
		// 16                                                                  
		{ "type",		"brush type (0=static,1=dynamic)",	USERBROWSE_SELECT, {"static","dynamic"} },			                                                                  
		{ "id",			"object id",						USERBROWSE_NONE },
		{ "material",	"material",							USERBROWSE_SELECT, g_matname },
		{ "draw",		"draw mode (1=img,2=mat,3=both)",	USERBROWSE_SELECT, {"none","img","mat","img+mat"} },
		{ "disable",	"no update no draw",				USERBROWSE_SELECT, {"no","yes"} },
		{ "delay",		"frame delay",						USERBROWSE_NONE },
		{ "anim",		"animation mode",					USERBROWSE_SELECT, {"stop","play","loop" } },
		{ "collider",	"collider mode",					USERBROWSE_SELECT, {"none","call handler","hard collision"} },
		{ "class",		"generic class",					USERBROWSE_SELECT, MOD_GetClassNames() },
		{ "status",		"generic status",					USERBROWSE_NONE },
		{ "target",		"target id",						USERBROWSE_NONE },
		{ "death",		"death cause",						USERBROWSE_NONE },
		{ "reserved",	"reserved",							USERBROWSE_NONE },
		{ "reserved",	"reserved",							USERBROWSE_NONE },
		{ "reserved",	"reserved",							USERBROWSE_NONE },
		{ "reserved",	"reserved",							USERBROWSE_NONE }, // collision
		
		// 32
		{ "user",		"user",								USERBROWSE_CUSTOM },
		{ "user",		"user",								USERBROWSE_CUSTOM },
		{ "user",		"user",								USERBROWSE_CUSTOM },
		{ "user",		"user",								USERBROWSE_CUSTOM },
		{ "user",		"user",								USERBROWSE_CUSTOM },
		{ "user",		"user",								USERBROWSE_CUSTOM },
		{ "user",		"user",								USERBROWSE_CUSTOM },
		{ "user",		"user",								USERBROWSE_CUSTOM },
		{ "user",		"user",								USERBROWSE_CUSTOM },
		{ "user",		"user",								USERBROWSE_CUSTOM },
		{ "user",		"user",								USERBROWSE_CUSTOM },
		{ "user",		"user",								USERBROWSE_CUSTOM },
		{ "user",		"user",								USERBROWSE_CUSTOM },
		{ "user",		"user",								USERBROWSE_CUSTOM },
		{ "user",		"user",								USERBROWSE_CUSTOM },
		{ "user",		"user",								USERBROWSE_CUSTOM }
	};
	
	// view modes (users may add more)
	g_view = 0;
	g_viewname = { "default view", "select view", "game view", "matrial view", "density view" };
	g_viewcount = sizeof(g_viewname);
	
	// room info
	g_roominfo = 0;
}

//////////////////////////////////////////////////////////////////////////////////
// done module
//////////////////////////////////////////////////////////////////////////////////
func MOD_Done()
{
}

//////////////////////////////////////////////////////////////////////////////////
