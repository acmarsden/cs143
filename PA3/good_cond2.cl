class Silly {
    copy() : SELF_TYPE { self };
};

class Sally inherits Silly { };

class Sully inherits Sally { 
    copy() : SELF_TYPE { self };
};

class C {
	a : Int;
	b : Bool;
    c : SELF_TYPE;
	init() : SELF_TYPE { self };
    condcheck(x : Int, y : Bool) : C { 
        if y then init() else c fi
    };
};

Class Main {
    x : Sally <- (new Sally).copy();

    main() : Sally { x };
};


