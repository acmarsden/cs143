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
	looper(x : Int, y : Bool) : C {
        s <- while b loop { a <- x; b <- false; } pool
	};
};

Class Main {
    x : Sally <- (new Sally).copy();

    main() : Sally { x };
};



