class A {
    test(x : Int, y : Int) : Bool {
        true
    };

    copy(): SELF_TYPE {self };
};


(* Test assignment to the wrong type *)
class B {
a : A <- (new A).copy();

xcar : String;

testb(hd : Int, t1 : String) : Object { 
    { 
        xcar <- hd;
    }
};

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

(* Test bad attribute assignment *)
class D {
a : A <- (new A).copy();
b : String <-(new A).copy();

testb(c : Object, t1 : String) : Object { c };

};

(* Test conditional with pred not bool *)
class E {
	a : Int;
	b : Bool;
	init() : SELF_TYPE { self };
    condcheck(x : Int, y : Bool) : Int { 
        if x then 1 else 3 fi
    };
};


class F {
	a : Int;
	b : Bool;
    c : SELF_TYPE;
	init() : SELF_TYPE { self };
    condcheck(x : Int, y : Bool) : F { 
        if y then init() else c fi
    };
};

(* Test bad dispatch, formal does not conform *)
class G {
    test(x : Object, y : Int) : Bool {
        true
    };

    copy(): SELF_TYPE {self };
};

class H {
a : G <- (new G).copy();
b : Int;

testb(hd : Object, t1 : String) : Object { 
    { 
        b <- hd;
        a.test(b,3);
    }
};

};

(* Let introduces no identifier *)
class I {
	a : Int;
	b : Bool;
	letter(x : Int, y : Bool) : I {
	    let in x + y
    };
};

(*Case branches do not have distinct types *)
class J {
	a : Int;
	b : Bool;
	caser(x : Int, y : Bool) : I {
	   case x+y of 
            a : Int => y;
            c : Int => x;
       esac
    };
};
(* The return type for a method does not conform to SELF_TYPE for original class which implemented the method*)

class Silly {
    copy() : SELF_TYPE { self };
};

class Sally inherits Silly { };

class Sully inherits Sally { 
    copy() : Sully { self };
};

Class Main {
  main():C {
   {
    (new C).init(1,1);
    (new C).init(1,true,3);
    (new C).iinit(1,true);
    (new C);
   }
  };
};
