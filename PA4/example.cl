
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
  plus_test(): Int { 1+2 };
  minus_test(): Int { 4-3 };
  mult_test(): Int { 5*6 };
  div_test(): Int { 8/2 };
  comp_test(): Int { ~2 };
  neg_test(): Bool { not true };
  lt_test_true(): Bool {2 < 3};
  leq_test_true(): Bool {3 <= 3};
  eq_test_true(): Bool {4=4};
  lt_test_false(): Bool {4 < 3};
  leq_test_false(): Bool {4 <= 3};
  eq_test_false(): Bool {43=4};
  isvoid_test(x:Int): Bool {isvoid x};
  loop_test(): Object { while 1<2 loop 3 pool };
  if_test(): Int {if 1<2 then 3 else 4 fi};
  block_test(): Int {{1; 2; 3; 2+3;}};
  let_test(): Int {let x:Int <- 4, y:Int <-3 in x+y};
  case_test(x: Object): Int {case x of
                          x: Int => 56;
                          y: Bool => 67;
                          z: String => 78;
                       esac};
  new_test(): Object {new Main};
  new_self_test(): Object {new SELF_TYPE};
  dispatch_test1() : Int {a.a_dispatch(0, 1)};
  dispatch_test2() : Int {plus_test()};
  static_dispatch_test() : Int {b@A.a_dispatch(0, 1)};
  main():SELF_TYPE { 
    {  out_string("Testing dispatch1 \n"); out_int(dispatch_test1());
       (*
       out_string("\n Testing dispatch2 \n"); out_int(dispatch_test2());
       out_string("\n minus_test \n"); out_int(minus_test()); 
       out_string("\n mult_test \n"); out_int(mult_test());
       out_string("\n div_test \n"); out_int(div_test());
       out_string("\n comp_test \n"); out_int(comp_test());
       out_string("\n neg_test \n"); out_int(if neg_test() then 0 else 1 fi);
       out_string("\n lt_test_true \n"); out_int( if lt_test_true() then 1 else 0 fi);
       out_string("\n leq_test_true \n"); out_int( if leq_test_true() then 1 else 0 fi);
       out_string("\n eq_test_true \n"); out_int( if eq_test_true() then 1 else 0 fi);
       out_string("\n lt_test_false \n"); out_int( if lt_test_false() then 0 else 1 fi);
       out_string("\n leq_test_false \n"); out_int( if leq_test_false() then 0 else 1 fi);
       out_string("\n eq_test_false \n"); out_int( if eq_test_false() then 0 else 1 fi);
       out_string("\n if_test \n"); out_int(if_test());
       out_string("\n block_test \n"); out_int(block_test());
       out_string("\n let_test \n"); out_int(let_test());
       out_string("\n case_test \n"); out_int(case_test(0));
       *) 
   }
  };
};

