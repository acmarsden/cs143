class A {
    get_self() : SELF_TYPE { self };
};

class Main inherits IO {
    a : A <- new A;
    let_test() : Int {  let b : A <- a.get_self() in 3 };
    main():SELF_TYPE { 
        { out_string("\n Testing let with SELF_TYPE \n"); out_int(let_test());
        }
    };
};

