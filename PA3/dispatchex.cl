(* Expression does not inherit from a class with the method *)
class A {
    test(x : Int, y : Int) : Bool {
        true
    };

    copy(): SELF_TYPE {self };
};

class B {
a : A <- (new A).copy();
b : Int;

testb(hd : Int, t1 : String) : Object { 
    { 
        b <- hd;
        a.test(b,3);
    }
};

};


Class Main {
    main() : Bool {true };
};
