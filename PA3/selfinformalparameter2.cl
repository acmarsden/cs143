(* Expression does not inherit from a class with the method *)
class A {
    test(x : Int, a : A) : Bool {
        true
    };

    b : Bool <- test(0, self);

};


Class Main {
    main() : Bool {true };
};
