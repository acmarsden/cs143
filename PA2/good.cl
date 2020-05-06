class A {
ana(): Int {
(let x:Int <- 1 in 2)+3
};
};

Class BB__ inherits A {
};

(* some fragments of correct cool code from the AFS examples *)

class B inherits A {  -- B is a number squared

   method5(num : Int) : E { -- square
      (let x : Int in
   {
            x <- num * num;
      (new E).set_var(x);
   }
      )
   };

};

class C inherits B {

   method6(num : Int) : A { -- negate
      (let x : Int in
         {
            x <- ~num;
      (new A).set_var(x);
         }
      )
   };

   method5(num : Int) : E {  -- cube
      (let x : Int in
   {
            x <- num * num * num;
      (new E).set_var(x);
   }
      )
   };

};

(*
   c2i   Converts a 1-character string to an integer.  Aborts
         if the string is not "0" through "9"
*)
class A2I {

     c2i(char : String) : Int {
  if char = "0" then 0 else
  if char = "1" then 1 else
  if char = "2" then 2 else
        if char = "3" then 3 else
        if char = "4" then 4 else
        if char = "5" then 5 else
        if char = "6" then 6 else
        if char = "7" then 7 else
        if char = "8" then 8 else
        if char = "9" then 9 else
        { abort(); 0; }  -- the 0 is needed to satisfy the typchecker
        fi fi fi fi fi fi fi fi fi fi
     };

(*
   i2c is the inverse of c2i.
*)
     i2c(i : Int) : String {
  if i = 0 then "0" else
  if i = 1 then "1" else
  if i = 2 then "2" else
  if i = 3 then "3" else
  if i = 4 then "4" else
  if i = 5 then "5" else
  if i = 6 then "6" else
  if i = 7 then "7" else
  if i = 8 then "8" else
  if i = 9 then "9" else
  { abort(); ""; }  -- the "" is needed to satisfy the typchecker
        fi fi fi fi fi fi fi fi fi fi
     };

(*
   a2i converts an ASCII string into an integer.  The empty string
is converted to 0.  Signed and unsigned strings are handled.  The
method aborts if the string does not represent an integer.  Very
long strings of digits produce strange answers because of arithmetic
overflow.

*)
     a2i(s : String) : Int {
        if s.length() = 0 then 0 else
  if s.substr(0,1) = "-" then ~a2i_aux(s.substr(1,s.length()-1)) else
        if s.substr(0,1) = "+" then a2i_aux(s.substr(1,s.length()-1)) else
           a2i_aux(s)
        fi fi fi
     };

(*
  a2i_aux converts the usigned portion of the string.  As a programming
example, this method is written iteratively.
*)
     a2i_aux(s : String) : Int {
  (let int : Int <- 0 in
           {
               (let j : Int <- s.length() in
            (let i : Int <- 0 in
        while i < j loop
      {
          int <- int * 10 + c2i(s.substr(i,1));
          i <- i + 1;
      }
        pool
      )
         );
              int;
      }
        )
     };

(*
    i2a converts an integer to a string.  Positive and negative
numbers are handled correctly.
*)
    i2a(i : Int) : String {
  if i = 0 then "0" else
        if 0 < i then i2a_aux(i) else
          "-".concat(i2a_aux(i * ~1))
        fi fi
    };

(*
    i2a_aux is an example using recursion.
*)
    i2a_aux(i : Int) : String {
        if i = 0 then "" else
      (let next : Int <- i / 10 in
    i2a_aux(next).concat(i2c(i - next * 10))
      )
        fi
    };

};

Class Article inherits Book {
    per_title : String;

    initArticle(title_p : String, author_p : String,
    per_title_p : String) : Article {
        {
            initBook(title_p, author_p);
            per_title <- per_title_p;
            self;
        }
    };

    print() : Book {
        {
      self@Book.print();
            out_string("periodical:  ").out_string(per_title).out_string("\n");
            self;
        }
    };
};

Class BookList inherits IO {
    (* Since abort "returns" type Object, we have to add
       an expression of type Bool here to satisfy the typechecker.
       This code is unreachable, since abort() halts the program.
    *)
    isNil() : Bool { { abort(); true; } };

    cons(hd : Book) : Cons {
        (let new_cell : Cons <- new Cons in
            new_cell.init(hd,self)
        )
    };

    (* Since abort "returns" type Object, we have to add
       an expression of type Book here to satisfy the typechecker.
       This code is unreachable, since abort() halts the program.
    *)
    car() : Book { { abort(); new Book; } };

    (* Since abort "returns" type Object, we have to add
       an expression of type BookList here to satisfy the typechecker.
       This code is unreachable, since abort() halts the program.
    *)
    cdr() : BookList { { abort(); new BookList; } };

    print_list() : Object { abort() };
};

class Complex inherits IO {
    x : Int;
    y : Int;

    init(a : Int, b : Int) : Complex {
  {
      x = a;
      y = b;
      self;
  }
    };

    print() : Object {
  if y = 0
  then out_int(x)
  else out_int(x).out_string("+").out_int(y).out_string("I")
  fi
    };

    reflect_0() : Complex {
  {
      x = ~x;
      y = ~y;
      self;
  }
    };

    reflect_X() : Complex {
  {
      y = ~y;
      self;
  }
    };

    reflect_Y() : Complex {
  {
      x = ~x;
      self;
  }
    };
};

class CellularAutomaton inherits IO {
    population_map : String;

    init(map : String) : SELF_TYPE {
        {
            population_map <- map;
            self;
        }
    };

    print() : SELF_TYPE {
        {
            out_string(population_map.concat("\n"));
            self;
        }
    };

    num_cells() : Int {
        population_map.length()
    };

    cell(position : Int) : String {
        population_map.substr(position, 1)
    };

    cell_left_neighbor(position : Int) : String {
        if position = 0 then
            cell(num_cells() - 1)
        else
            cell(position - 1)
        fi
    };

    cell_right_neighbor(position : Int) : String {
        if position = num_cells() - 1 then
            cell(0)
        else
            cell(position + 1)
        fi
    };

    (* a cell will live if exactly 1 of itself and it's immediate
       neighbors are alive *)
    cell_at_next_evolution(position : Int) : String {
        if (if cell(position) = "X" then 1 else 0 fi
            + if cell_left_neighbor(position) = "X" then 1 else 0 fi
            + if cell_right_neighbor(position) = "X" then 1 else 0 fi
            = 1)
        then
            "X"
        else
            "."
        fi
    };

    evolve() : SELF_TYPE {
        (let position : Int in
        (let num : Int <- num_cells() in
        (let temp : String in
            {
                while position < num loop
                    {
                        temp <- temp.concat(cell_at_next_evolution(position));
                        position <- position + 1;
                    }
                pool;
                population_map <- temp;
                self;
            }
        ) ) )
    };
};

class Graph {

   vertices : VList <- new VList;
   edges    : EList <- new EList;

   add_vertice(v : Vertice) : Object { {
      edges <- v.outgoing().append(edges);
      vertices <- vertices.cons(v);
   } };

   print_E() : Object { edges.print() };
   print_V() : Object { vertices.print() };

};

class Vertice inherits IO {

   num  : Int;
   out  : EList <- new EList;

   outgoing() : EList { out };

   number() : Int { num };

   init(n : Int) : SELF_TYPE {
      {
         num <- n;
         self;
      }
   };


   add_out(s : Edge) : SELF_TYPE {
      {
   out <- out.cons(s);
         self;
      }
   };

   print() : Object {
      {
         out_int(num);
   out.print();
      }
   };

};

class Edge inherits IO {

   from   : Int;
   to     : Int;
   weight : Int;

   init(f : Int, t : Int, w : Int) : SELF_TYPE {
      {
         from <- f;
   to <- t;
   weight <- w;
   self;
      }
   };

   print() : Object {
      {
         out_string(" (");
   out_int(from);
   out_string(",");
   out_int(to);
   out_string(")");
   out_int(weight);
      }
   };

};

class Foo inherits Bazz {
     a : Razz <- case self of
          n : Razz => (new Bar);
          n : Foo => (new Razz);
          n : Bar => n;
             esac;

     b : Int <- a.doh() + g.doh() + doh() + printh();

     doh() : Int { (let i : Int <- h in { h <- h + 2; i; } ) };

};

class Bar inherits Razz {

     c : Int <- doh();

     d : Object <- printh();
};


class Razz inherits Foo {

     e : Bar <- case self of
      n : Razz => (new Bar);
      n : Bar => n;
    esac;

     f : Int <- a@Bazz.doh() + g.doh() + e.doh() + doh() + printh();

};

class Bazz inherits IO {

     h : Int <- 1;

     g : Foo  <- case self of
          n : Bazz => (new Foo);
          n : Razz => (new Bar);
      n : Foo  => (new Razz);
      n : Bar => n;
      esac;

     i : Object <- printh();

     printh() : Int { { out_int(h); 0; } };

     doh() : Int { (let i: Int <- h in { h <- h + 1; i; } ) };
};

class A {

   -- Let's assume that we don't want A to not inherit from IO.

   io : IO <- new IO;

   out_a() : Object { io.out_string("A: Hello world\n") };

};


class B inherits A {

   -- B does not have to an extra attribute, since it inherits io from A.

   out_b() : Object { io.out_string("B: Hello world\n") };

};


class C inherits IO {

   -- Now the IO methods are part of C.

   out_c() : Object { out_string("C: Hello world\n") };

   -- Note that out_string(...) is just a shorthand for self.out_string(...)

};

class VarListNE inherits VarList {
  x : Variable;
  rest : VarList;
  isNil() : Bool { false };
  head()  : Variable { x };
  tail()  : VarList { rest };
  init(y : Variable, r : VarList) : VarListNE { { x <- y; rest <- r; self; } };
  print() : SELF_TYPE { { x.print_self(); out_string(" ");
                    rest.print(); self; } };
};

(*
 * A list of closures we need to build.  We need to number (well, name)
 * the closures uniquely.
 *)
class LambdaList {
  isNil() : Bool { true };
  headE() : VarList { { abort(); new VarList; } };
  headC() : Lambda { { abort(); new Lambda; } };
  headN() : Int { { abort(); 0; } };
  tail()  : LambdaList { { abort(); new LambdaList; } };
  add(e : VarList, x : Lambda, n : Int) : LambdaList {
    (new LambdaListNE).init(e, x, n, self)
  };
};

class LambdaListNE inherits LambdaList {
  lam : Lambda;
  num : Int;
  env : VarList;
  rest : LambdaList;
  isNil() : Bool { false };
  headE() : VarList { env };
  headC() : Lambda { lam };
  headN() : Int { num };
  tail()  : LambdaList { rest };
  init(e : VarList, l : Lambda, n : Int, r : LambdaList) : LambdaListNE {
    {
      env <- e;
      lam <- l;
      num <- n;
      rest <- r;
      self;
    }
  };
};

class Board inherits IO {

 rows : Int;
 columns : Int;
 board_size : Int;

 size_of_board(initial : String) : Int {
   initial.length()
 };

 board_init(start : String) : SELF_TYPE {
   (let size :Int  <- size_of_board(start) in
    {
  if size = 15 then
   {
    rows <- 3;
    columns <- 5;
    board_size <- size;
   }
  else if size = 16 then
    {
    rows <- 4;
    columns <- 4;
    board_size <- size;
   }
  else if size = 20 then
   {
    rows <- 4;
    columns <- 5;
    board_size <- size;
   }
  else if size = 21 then
   {
    rows <- 3;
    columns <- 7;
    board_size <- size;
   }
  else if size = 25 then
   {
    rows <- 5;
    columns <- 5;
    board_size <- size;
   }
  else if size = 28 then
   {
    rows <- 7;
    columns <- 4;
    board_size <- size;
   }
  else  -- If none of the above fit, then just give
   {  -- the configuration of the most common board
    rows <- 5;
    columns <- 5;
    board_size <- size;
   }
  fi fi fi fi fi fi;
  self;
    }
   )
 };

};

class Complex inherits IO {
    x : Int;
    y : Int;

    init(a : Int, b : Int) : Complex {
  {
      x = a;
      y = b;
      self;
  }
    };

    print() : Object {
  if y = 0
  then out_int(x)
  else out_int(x).out_string("+").out_int(y).out_string("I")
  fi
    };

    reflect_0() : Complex {
  {
      x = ~x;
      y = ~y;
      self;
  }
    };

    reflect_X() : Complex {
  {
      y = ~y;
      self;
  }
    };

    reflect_Y() : Complex {
  {
      x = ~x;
      self;
  }
    };

    equal(d : Complex) : Bool {
  if x = d.x_value()
  then
      if y = d.y_value()
      then true
      else false
      fi
  else false
  fi
    };

    x_value() : Int {
  x
    };

    y_value() : Int {
  y
    };
};

class List {
   -- Define operations on empty lists.

   isNil() : Bool { true };

   -- Since abort() has return type Object and head() has return type
   -- Int, we need to have an Int as the result of the method body,
   -- even though abort() never returns.

   head()  : Int { { abort(); 0; } };

   -- As for head(), the self is just to make sure the return type of
   -- tail() is correct.

   tail()  : List { { abort(); self; } };

   -- When we cons and element onto the empty list we get a non-empty
   -- list. The (new Cons) expression creates a new list cell of class
   -- Cons, which is initialized by a dispatch to init().
   -- The result of init() is an element of class Cons, but it
   -- conforms to the return type List, because Cons is a subclass of
   -- List.

   cons(i : Int) : List {
      (new Cons).init(i, self)
   };

};


(*
 *  Cons inherits all operations from List. We can reuse only the cons
 *  method though, because adding an element to the front of an emtpy
 *  list is the same as adding it to the front of a non empty
 *  list. All other methods have to be redefined, since the behaviour
 *  for them is different from the empty list.
 *
 *  Cons needs two attributes to hold the integer of this list
 *  cell and to hold the rest of the list.
 *
 *  The init() method is used by the cons() method to initialize the
 *  cell.
 *)

class Cons inherits List {

   car : Int; -- The element in this list cell

   cdr : List;  -- The rest of the list

   isNil() : Bool { false };

   head()  : Int { car };

   tail()  : List { cdr };

   init(i : Int, rest : List) : List {
      {
   car <- i;
   cdr <- rest;
   self;
      }
   };

};

class Main inherits IO {

  main() : Int {  -- main() is an atrophied method so we can parse.
    0
  };

  out : Int <-    -- out is our 'output'.  It's values are the primes.
    {
      out_string("2 is trivially prime.\n");
      2;
    };

  testee : Int <- out;  -- testee is a number to be tested for primeness.

  divisor : Int;  -- divisor is a number which may factor testee.

  stop : Int <- 500;  -- stop is an arbitrary value limiting testee.

  m : Object <-   -- m supplants the main method.
    while true loop
      {

        testee <- testee + 1;
        divisor <- 2;

        while
          if testee < divisor * divisor
            then false    -- can stop if divisor > sqrt(testee).
    else if testee - divisor*(testee/divisor) = 0
            then false    -- can stop if divisor divides testee.
            else true
          fi fi
        loop
          divisor <- divisor + 1
        pool;

        if testee < divisor * divisor -- which reason did we stop for?
        then  -- testee has no factors less than sqrt(testee).
          {
            out <- testee;  -- we could think of out itself as the output.
            out_int(out);
            out_string(" is prime.\n");
          }
        else  -- the loop halted on testee/divisor = 0, testee isn't prime.
          0 -- testee isn't prime, do nothing.
  fi;

        if stop <= testee then
          "halt".abort()  -- we could think of "halt" as SIGTERM.
        else
          "continue"
        fi;

      }
    pool;

}; (* end of Main *)

Class List inherits IO {
        (* Since abort() returns Object, we need something of
     type Bool at the end of the block to satisfy the typechecker.
           This code is unreachable, since abort() halts the program. *)
  isNil() : Bool { { abort(); true; } };

  cons(hd : Int) : Cons {
    (let new_cell : Cons <- new Cons in
    new_cell.init(hd,self)
    )
  };

  (*
     Since abort "returns" type Object, we have to add
     an expression of type Int here to satisfy the typechecker.
     This code is, of course, unreachable.
        *)
  car() : Int { { abort(); new Int; } };

  cdr() : List { { abort(); new List; } };

  rev() : List { cdr() };

  sort() : List { cdr() };

  insert(i : Int) : List { cdr() };

  rcons(i : Int) : List { cdr() };

  print_list() : Object { abort() };
};

Class Cons inherits List {
  xcar : Int;  -- We keep the car in cdr in attributes.
  xcdr : List; -- Because methods and features must have different names,
         -- we use xcar and xcdr for the attributes and reserve
         -- cons and car for the features.

  isNil() : Bool { false };

  init(hd : Int, tl : List) : Cons {
    {
      xcar <- hd;
      xcdr <- tl;
      self;
    }
  };

  car() : Int { xcar };

  cdr() : List { xcdr };

  rev() : List { (xcdr.rev()).rcons(xcar) };

  sort() : List { (xcdr.sort()).insert(xcar) };

  insert(i : Int) : List {
    if i < xcar then
      (new Cons).init(i,self)
    else
      (new Cons).init(xcar,xcdr.insert(i))
    fi
  };


  rcons(i : Int) : List { (new Cons).init(xcar, xcdr.rcons(i)) };

  print_list() : Object {
    {
         out_int(xcar);
         out_string("\n");
         xcdr.print_list();
    }
  };
};
