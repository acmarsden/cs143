
(*  Example cool program testing as many aspects of the code generator
    as possible.
 *)

class Main {
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
};

