
(*  Example cool program testing as many aspects of the code generator
    as possible.
 *)
class A {
    a_dispatch(x : Int, y : Int) : Int {x + y};
};

class B inherits A{
  a_dispatch(x: Int, y: Int) : Int {x - y};
};

class Main inherits IO {
  a : A;
  b : B;
  loop_test() : Object { while 2<1 loop 3 pool };
  void_case_test(): Int {case loop_test() of
                          x: Int => 56;
                          y: Bool => 67;
                          z: String => 78;
                       esac};
  main():SELF_TYPE { 
    {   out_string("Testing void case \n"); out_int(void_case_test());
   }
  };
};

