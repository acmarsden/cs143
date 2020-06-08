(*  Tests type case on matching superclasses and not exact matches
 *)
class A {
  a1 : Int;
  foo(x : Int, y : Int) : Int {x + y};
};

class B inherits A{
  b1 : Bool;
  foo(x: Int, y: Int) : Int {x - y};
};

class C inherits B{
  c1: B;
  foo(x: Int, y: Int) : Int {x * y};
};

class D inherits B{
  d1: A;
  foo(x: Int, y: Int) : Int {x / y};
};

class Main inherits IO {
  a : A <- new A;
  b : B <- new B;
  c : C< - new C;
  d : D <- new D;
  case_test_1(x: Object): Int {case x of
                          a: A => 56;
                          b: B => 67;
                          c: C => 78;
                          d: D => 89;
                       esac};

  main():SELF_TYPE {
    {
       out_string("\n Expects 56 Got: \n");
       out_int(case_test_1(a));

       out_string("\n Expects 67 Got: \n");
       out_int(case_test_1(b));

       out_string("\n Expects 78 Got: \n");
       out_int(case_test_1(c));

       out_string("\n Expects 89 Got: \n");
       out_int(case_test_1(d));
   }
  };
};
