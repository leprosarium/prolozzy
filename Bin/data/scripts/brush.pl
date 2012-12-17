:- module(brush, [var/3,
	       varDef/2]).


var(BrushIdx, Var, Val) :-
	varDef(Var, VarIdx),
	map:brushVar(BrushIdx, VarIdx, Val).


varDef(layer,	0).
varDef(x,	1).
varDef(y,	2).
varDef(w,	3).
varDef(h,	4).
varDef(tile,	5).
varDef(frame,	6).
varDef(map,	7).
varDef(flip,	11).
varDef(color,	12).
varDef(shader,	13).
varDef(scale,	14).
varDef(id,	17).
varDef(material,18).
varDef(draw,	19).
varDef(delay,	21).
varDef(anim,	22).
%varDef('B_MAX',		48).








