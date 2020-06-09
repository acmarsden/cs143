(*  Tests type case on matching superclasses and not exact matches
 *)
class A {
    a_dispatch(x : Int, y : Int) : Int {x + y};
};

class B inherits A{
  a_dispatch(x: Int, y: Int) : Int {x - y};
};

class C inherits B{
  a_dispatch(x: Int, y: Int) : Int {x - y};
};

class D inherits B{
  a_dispatch(x: Int, y: Int) : Int {x - y};
};

class Main inherits IO {
  a : A <- new A;
  b : B <- new B;
  c : C <- new C;
  d : D <- new D;
  case_test_DT(x: Object): Int {case x of
                          a: A => 56;
                          b: B => 67;
                       esac};

  case_test_ST(x: D): Int {case x of
                          a: A => 56;
                          b: B => 67;
                       esac};
  main():SELF_TYPE {
    {
       out_string("\n case_test_ST\n");
       out_int(case_test_ST(d));
       out_string("\n");
       out_string("\n case_test_DT\n");
       out_int(case_test_DT(d));
       out_string("\n");
   }
  };
};
