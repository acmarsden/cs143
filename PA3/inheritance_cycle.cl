class A inherits B{
  x: Int;
  y: Bool;
};

class B inherits C{
  u: Str;
  v: IO;
};

class C inherits A{
  a : Int;
  b : Bool;
  init(x : Int, y : Bool) : C {
           {
    a <- x;
    b <- y;
    self;
           }
  };
};

