//////////////////////////////////////////////////////////////////////////////////
// Brush Props 
// generic dialog with brush properties
// mode: 0=normal, 1=with check boxes for search, 2=with checkboxes for change
// brushidx: map brush index to set props to, or -1 for the current toolbrush
// can't change position, size and selection for the brush (would repartition and stuff)
//////////////////////////////////////////////////////////////////////////////////

// browse command for the idx prop (store prop idx in DV_USER)
func DlgProps_Browse( idx )
{
	if( g_brushprop[idx][BRUSHPROP_BROWSE]==USERBROWSE_CUSTOM )
	{
		MOD_BrushPropsBrowser(idx);
	}
}

// for use in custom browser
func DlgProps_GetProp(idx)
{
	dlg = DlgGetSelect();
	item = ItemGetSelect();
	DlgSelect(DlgFind(ID_DLGPROPS));
	ItemSelect(ItemFind(ID_DLGITEM+idx*10+2));
	val = (int)ItemGetTxt(IV_TXT);
	if(dlg!=-1) DlgSelect(dlg);
	if(item!=-1) ItemSelect(item);
	return val;
}

// for use in custom browser
func DlgProps_SetProp(idx,val)
{
	dlg = DlgGetSelect();
	item = ItemGetSelect();
	DlgSelect(DlgFind(ID_DLGPROPS));
	ItemSelect(ItemFind(ID_DLGITEM+idx*10+2));
	ItemSetTxt(IV_TXT,(str)val);
	if(g_dlgprop_brushidx>=0)	MapBrushSet(g_dlgprop_brushidx,idx,val);
	else						ToolBrushSet(idx,val);
	DlgProps_Update();
	if(dlg!=-1) DlgSelect(dlg);
	if(item!=-1) ItemSelect(item);
}

//////////////////////////////////////////////////////////////////////////////////
