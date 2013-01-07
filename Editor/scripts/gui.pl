:- module(gui, [loadResources/0]).
loadResources :-
	gui:imgLoad("Editor\\Graphics\\check1.tga", Img_check1),
	core:dl(img(Img_check1)),
	gui:imgLoad("Editor\\Graphics\\check2.tga", Img_check2),
	core:dl(img(Img_check2)),
	gui:imgLoad("Editor\\Graphics\\check3.tga", Img_check3),
	core:dl(img(Img_check3)),
	gui:imgLoad("Editor\\Graphics\\check4.tga", Img_check4),
	core:dl(img(Img_check4)),
	gui:imgLoad("Editor\\Graphics\\radio1.tga", Img_radio1),
	core:dl(img(Img_radio1)),
	gui:imgLoad("Editor\\Graphics\\radio2.tga", Img_radio2),
	core:dl(img(Img_radio2)),

	gui:imgLoad("Editor\\Graphics\\icon_info.tga", Img_Icon_Info),
	core:dl(img(Img_Icon_Info)),
	gui:imgLoad("Editor\\Graphics\\icon_question.tga", Img_icon_Question),
	core:dl(img(Img_icon_Question)),
	gui:imgLoad("Editor\\Graphics\\icon_warning.tga", Img_Icon_Warning),
	core:dl(img(Img_Icon_Warning)),
	gui:imgLoad("Editor\\Graphics\\icon_error.tga", Img_Icon_Error),
	core:dl(img(Img_Icon_Error)).
