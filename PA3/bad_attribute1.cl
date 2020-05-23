(* Expression does not inherit from a class with the method *)
class A {
    test(x : Int, y : Int) : Bool {
        true
    };

    copy(): SELF_TYPE {self };
};

class B {
a : A <- (new A).copy();
b : String <-(new A).copy();

testb(c : Object, t1 : String) : Object { c };

};


Class Main {
    main() : Bool {true };
};
