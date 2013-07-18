:- module(gamedef, [gameTitle/1,
		    gameAuthor/1,
		    playerLayer/1,
		    maxInventory/1,
		    supportJumpUp/0,
		    supportWaterPlay/0,
		    loadingTime/1,
		    mainMenuAttract/1,
		    playerMainMenuX/1,
		    playerMainMenuY/1,
		    playerBeginX/1,
		    playerBeginY/1,
		    playerBubbles/1,
		    musicDefault/1,
		    fontDefault/1,
		    maxCredits/1,
		    maxCoins/1,
		    scuba/1,
		    flippers/1,
		    bubbles/0]).

maxCredits(3).		% max number of credits
maxCoins(4).		% max number of coins or diamonds to find (set it to the number of coins you added in your map)
maxInventory(3).	% max number of inventory items ( max 10 items )


gameTitle('DizzyAGE Default Template').
gameAuthor('ELFIMOV ANTON').
mainMenuAttract(1).
playerLayer(5).
playerMainMenuX(400).	%set the player's position x in the mainmenu room (attract mode room)
playerMainMenuY(246).	%set the player's position y in the mainmenu room (attract mode room)
playerBeginX(640).	% set the starting positon x of the player in map (when a new game begins)
playerBeginY(246).	% set the starting positon y of the player in map (when a new game begins)
playerBubbles(16).	% set this to the count of air bubble objects you placed in the map. used in WaterPlay

supportJumpUp.
supportWaterPlay.
loadingTime(1).

musicDefault(music_a).	% id of the default music

fontDefault(4).		% id of the default font

% WaterPlay items: scuba and flippers
scuba(scuba).
flippers(flippers).
bubbles.



