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
	init(x : Int, y : Bool) : C {
					 {
		a <- x;
		b <- y;
		self;
					 }
	};
};

class D {
	a : Int;
	b : Bool;
	init() : SELF_TYPE { self };
    condcheck(x : Int, y : Bool) : Int { 
        if y then 1 else 3 fi
    };
};

class E {
	a : Int;
	b : Bool;
    c : SELF_TYPE;
	init() : SELF_TYPE { self };
    condcheck(x : Int, y : Bool) : E { 
        if y then init() else c fi
    };
};


class F {
	a : Int;
	b : Bool;
    init() : SELF_TYPE {self };
	looper(x : Int, y : Bool) : Bool {
         isvoid while b loop { a <- x; b <- false; } pool
	};
};

Class Main {
    x : Sally <- (new Sally).copy();

    main() : Sally { x };
};



