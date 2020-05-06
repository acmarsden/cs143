
(*
 *  execute "coolc bad.cl" to see the error messages that the coolc parser
 *  generates
 *
 *  execute "myparser bad.cl" to see the error messages that your parser
 *  generates
 *)

(* no error *)
class A {
};

(* error:  b is not a type identifier *)
Class b inherits A {
};

(* error:  a is not a type identifier *)
Class C inherits a {
};

(* error:  keyword inherits is misspelled *)
Class D inherts A {
};

(* error:  closing brace is missing *)
Class E inherits A {
;


(* Feature Errors *)

(* no error *)
class A {
featuretest( a : Int) : Int { a } ;
featuretest : Int;
featuretest : Int <- x;
featuretest() : Int { a };
};

(* error: b is not a type identifier *)
class A {
featuretest( a : b) : B { a } ;
featuretest : b;
featuretest : b <- x;
featuretest() : b { a };
};

(* error: Featuretest is not an objectid *)
class A {
Featuretest( a : B) : B { a };
Featuretest : B;
Featuretest : B <- x;
Featuretest() : B { a };
};

(* error: typos with '(' or ':' or ';' *)
class A {
featuretest{a : B) : B { a };
featuretest :: B;
featuretest ; B <- x;
featuretest() : B { a ) ;
};


class a {
};

class a {
};


class a {
};



(* expression block errors *)

(* no error *)
class A {

feature( a : B ) : B {

{
a + 1;
a + 2;
a + 3;
}

};

};


class a {
};

class a {
};

class a {
};

(* middle expression block doesn't terminate properly*)
class A {

feature( a : B) {

{
a + 1;
# + 2;
a + 3;
}

};
};


class a {
};
class a {
};
class a {
};


(* Let Errors *)
(* no error *)
class A {
  main() : M {
    let b : B in let c : C in x + x
  };
};

(* error in types *)
class A {
  main() : M {
    let b : B in let c : c in let d : D in let e : e in x + x
  };
};

(* correct let, shorthand *)
class A {
  main() : M {
    let b : B, c : C, d : D, e : E in x + x
  };
};

class A {
  main() : M {
    let b : B, c : C, d : D <- 5, e : E in x + x
  };
};

(* errors in let bindings , shorthand *)
class A {
  main() : M {
    let b : B, c : c, d : D, e : E in x + x
  };
};

class A {
  main() : M {
    let b : B, c : C, d : D <-, e : E in x + x
  };
};

