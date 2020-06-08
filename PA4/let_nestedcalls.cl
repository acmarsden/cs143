class A {
    get_self() : SELF_TYPE { self };
    ret_int() : Int { 3 };
    computenum() : Int {3 + 4};
};

class Main inherits IO {
    a : A <- new A;
    lt_test_true() : Bool { 3 = 3};

    let_test1() : Int {  3 + let b : Int <- 0 in b };
    let_test2() : Int { let b : Int <- a.ret_int() in b + let c : Bool <- lt_test_true() in if c then a.computenum() else 0 fi};
    main():SELF_TYPE { 
        { out_string("\n Testing nested lets \n"); out_int(let_test1());
          out_string("\n Testing nested lets 2 \n"); out_int(let_test2());
        }
    };
};

