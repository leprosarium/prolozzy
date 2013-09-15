:-module(keys, [key/2,
	       mouse/2]).

% Direct Input Keys

key(escape, 0x01).
key(1, 0x02).
key(2, 0x03).
key(3, 0x04).
key(4, 0x05).
key(5, 0x06).
key(6, 0x07).
key(7, 0x08).
key(8, 0x09).
key(9, 0x0A).
key(0, 0x0B).
key(minus, 0x0C).    % - on main keyboard
key(equals, 0x0D).
key(back, 0x0E).    % backspace
key(tab, 0x0F).
key(q, 0x10).
key(w, 0x11).
key(e, 0x12).
key(r, 0x13).
key(t, 0x14).
key(y, 0x15).
key(u, 0x16).
key(i, 0x17).
key(o, 0x18).
key(p, 0x19).
key(lbracket, 0x1A).
key(rbracket, 0x1B).
key(return, 0x1C).    % Enter on main keyboard
key(lcontrol, 0x1D).
key(a, 0x1E).
key(s, 0x1F).
key(d, 0x20).
key(f, 0x21).
key(g, 0x22).
key(h, 0x23).
key(j, 0x24).
key(k, 0x25).
key(l, 0x26).
key(semicolon, 0x27).
key(apostrophe, 0x28).
key(grave, 0x29).    % accent grave
key(lshift, 0x2A).
key(backslash, 0x2B).
key(z, 0x2C).
key(x, 0x2D).
key(c, 0x2E).
key(v, 0x2F).
key(b, 0x30).
key(n, 0x31).
key(m, 0x32).
key(comma, 0x33).
key(period, 0x34).    % . on main keyboard
key(slash, 0x35).    % / on main keyboard
key(rshift, 0x36).
key(multiply, 0x37).    % * on numeric keypad
key(lmenu, 0x38).    % left Alt
key(space, 0x39).
key(capital, 0x3A).
key(f1, 0x3B).
key(f2, 0x3C).
key(f3, 0x3D).
key(f4, 0x3E).
key(f5, 0x3F).
key(f6, 0x40).
key(f7, 0x41).
key(f8, 0x42).
key(f9, 0x43).
key(f10, 0x44).
key(numlock, 0x45).
key(scroll, 0x46).    % Scroll Lock
key(numpad7, 0x47).
key(numpad8, 0x48).
key(numpad9, 0x49).
key(subtract, 0x4A).    % - on numeric keypad
key(numpad4, 0x4B).
key(numpad5, 0x4C).
key(numpad6, 0x4D).
key(add, 0x4E).    % + on numeric keypad
key(numpad1, 0x4F).
key(numpad2, 0x50).
key(numpad3, 0x51).
key(numpad0, 0x52).
key(decimal, 0x53).    % . on numeric keypad
key(oem_102, 0x56).    % <> or \| on RT 102-key keyboard (Non-U.S.)
key(f11, 0x57).
key(f12, 0x58).
key(f13, 0x64).    %                     (NEC PC98)
key(f14, 0x65).    %                     (NEC PC98)
key(f15, 0x66).    %                     (NEC PC98)
key(kana, 0x70).    % (Japanese keyboard)
key(abnt_c1, 0x73).    % /? on Brazilian keyboard
key(convert, 0x79).    % (Japanese keyboard)
key(noconvert, 0x7B).    % (Japanese keyboard)
key(yen, 0x7D).    % (Japanese keyboard)
key(abnt_c2, 0x7E).    % Numpad . on Brazilian keyboard
key(numpadequals, 0x8D).    % = on numeric keypad (NEC PC98)
key(prevtrack, 0x90).    % Previous Track (KEY_CIRCUMFLEX on Japanese keyboard)
key(at, 0x91).    %                     (NEC PC98)
key(colon, 0x92).    %                     (NEC PC98)
key(underline, 0x93).    %                     (NEC PC98)
key(kanji, 0x94).    % (Japanese keyboard)
key(stop, 0x95).    %                     (NEC PC98)
key(ax, 0x96).    %                     (Japan AX)
key(unlabeled, 0x97).    %                        (J3100)
key(nexttrack, 0x99).    % Next Track
key(numpadenter, 0x9C).    % Enter on numeric keypad
key(rcontrol, 0x9D).
key(mute, 0xA0).    % Mute
key(calculator, 0xA1).    % Calculator
key(playpause, 0xA2).    % Play / Pause
key(mediastop, 0xA4).    % Media Stop
key(volumedown, 0xAE).    % Volume -
key(volumeup, 0xB0).    % Volume +
key(webhome, 0xB2).    % Web home
key(numpadcomma, 0xB3).    % , on numeric keypad (NEC PC98)
key(divide, 0xB5).    % / on numeric keypad
key(sysrq, 0xB7).
key(rmenu, 0xB8).    % right Alt
key(pause, 0xC5).    % Pause
key(home, 0xC7).    % Home on arrow keypad
key(up, 0xC8).    % UpArrow on arrow keypad
key(prior, 0xC9).    % PgUp on arrow keypad
key(left, 0xCB).    % LeftArrow on arrow keypad
key(right, 0xCD).    % RightArrow on arrow keypad
key(end, 0xCF).    % End on arrow keypad
key(down, 0xD0).    % DownArrow on arrow keypad
key(next, 0xD1).    % PgDn on arrow keypad
key(insert, 0xD2).    % Insert on arrow keypad
key(delete, 0xD3).    % Delete on arrow keypad
key(lwin, 0xDB).    % Left Windows key
key(rwin, 0xDC).    % Right Windows key
key(apps, 0xDD).    % AppMenu key
key(power, 0xDE).    % System Power
key(sleep, 0xDF).    % System Sleep
key(wake, 0xE3).    % System Wake
key(websearch, 0xE5).    % Web Search
key(webfavorites, 0xE6).    % Web Favorites
key(webrefresh, 0xE7).    % Web Refresh
key(webstop, 0xE8).    % Web Stop
key(webforward, 0xE9).    % Web Forward
key(webback, 0xEA).    % Web Back
key(mycomputer, 0xEB).    % My Computer
key(mail, 0xEC).    % Mail
key(mediaselect, 0xED).    % Media Select


%  Alternate names for keys, to facilitate transition from DOS.

key(backspace, 0x0E).		     % backspace
key(numpadstar, 0x37).			 % * on numeric keypad
key(lalt, 0x38).		     % left Alt
key(capslock, 0x3A).	         % CapsLock
key(numpadminus, 0x4A).	         % - on numeric keypad
key(numpadplus, 0x4E).            % + on numeric keypad
key(numpadperiod, 0x53).	         % . on numeric keypad
key(numpadslash, 0xB5).            % / on numeric keypad
key(ralt, 0xB8).	         % right Alt
key(uparrow, 0xC8).            % UpArrow on arrow keypad
key(pgup, 0xC9).            % PgUp on arrow keypad
key(leftarrow, 0xCB).            % LeftArrow on arrow keypad
key(rightarrow, 0xCD).            % RightArrow on arrow keypad
key(downarrow, 0xD0).            % DownArrow on arrow keypad
key(pgdn, 0xD1).            % PgDn on arrow keypad


% Mouse related, specific for our input only

mouse(button1, 0).
mouse(button2, 1).
mouse(button3, 2).
mouse(button4, 3).








