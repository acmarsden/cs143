class Silly {
    copy() : SELF_TYPE { self };
};

class Sally inherits Silly { };


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

    main(invalid: String) : Sally { x };
};



