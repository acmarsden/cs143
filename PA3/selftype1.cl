class Silly {
    copy() : SELF_TYPE { self };
};

class Sally inherits Silly {
    a : Silly;
    testfun() : Silly {
        a
    };
};

class Sully inherits Sally { 
    copy() : SELF_TYPE { self };

    testfun() : SELF_TYPE { self };
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

Class Main {
    x : Sally <- (new Sally).copy();

    main() : Sally { x };
};



