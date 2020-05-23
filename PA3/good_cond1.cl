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
	init() : SELF_TYPE { self };
    condcheck(x : Int, y : Bool) : Int { 
        if y then 1 else 3 fi
    };
};

Class Main {
    x : Sally <- (new Sally).copy();

    main() : Sally { x };
};



