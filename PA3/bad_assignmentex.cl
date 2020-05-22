(* Expression does not inherit from a class with the method *)
class A {
    test(x : Int, y : Int) : Bool {
        true
    };

    copy(): SELF_TYPE {self };
};

class B {
a : A <- (new A).copy();

xcar : String;

testb(hd : Int, t1 : String) : Object { 
    { 
        xcar <- hd;
    }
};

};


Class Main {
    main() : Bool {true };
};
