:- module(sound, [music/2,
		  fx/2,
		  playFx/1]).

music('MUSIC_A', 1).
music('MUSIC_B', 2).
music('MUSIC_C', 3).


fx(success,	1).
fx(stun,	2).
fx(death,	3).
fx(respawn,	4).
fx(coin,	5).
fx(hurt,	6).
fx(jump,	7).
fx(beep1,	8).
fx(beep2,	9).

playFx(Fx) :-
	fx(Fx, FxIdx),
	sample:play(FxIdx).
