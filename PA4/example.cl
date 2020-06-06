
(*  Example cool program testing as many aspects of the code generator
    as possible.
 *)
class A {
    a_dispatch(x : Int, y : Bool) : Int {if y then x else x + 1 fi};
};

class Main {
  a : A;
  main():Int { 0 };
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
  if_test(): Object {if 1<2 then 3 else 4 fi};
  block_test(): Int {{1; 2; 3; 2+3;}};
  let_test(): Int {let x:Int <- 4, y:Int <-3 in x+y};
  new_test(): Object {new Main};
  new_self_test(): Object {new SELF_TYPE};
  let_test(x: Int): Int {let a : Int <- 3 in x};
  dispatch_test() : Int {a.a_dispatch(0, true)};
};

