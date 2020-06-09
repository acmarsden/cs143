(*  Tests type case on matching superclasses and not exact matches
 *)
class One {
    a_dispatch(x : Int, y : Int) : Int {x + y};
};

class Two {

    a_dispatch(x : Int, y : Int) : Int {x + y};
};

class Three {

    a_dispatch(x : Int, y : Int) : Int {x + y};
};


class Four inherits One {

    a_dispatch(x : Int, y : Int) : Int {x + y};
};

class Seven inherits Four {

    a_dispatch(x : Int, y : Int) : Int {x + y};
};


class Five inherits Three {

    a_dispatch(x : Int, y : Int) : Int {x + y};
};

class Six inherits Three {

    a_dispatch(x : Int, y : Int) : Int {x + y};
};

Class Eight inherits Five {

    a_dispatch(x : Int, y : Int) : Int {x + y};
};

Class Nine inherits Six {

    a_dispatch(x : Int, y : Int) : Int {x + y};
};

class Main inherits IO {
  one : One <- new One;
  two : Two <- new Two;
  three : Three <- new Three;
  four : Four <- new Four;
  five : Five <- new Five;
  six : Six <- new Six;
  seven : Seven <- new Seven;
  eight : Eight <- new Eight;
  nine : Nine <- new Nine;


  case_test_DT(x: Object): String {case x of
                                    four : Four => "\n Matched with Four \n";
                                    five : Five => "\n Matched with Five \n";
                                    six : Six => "\n Matched with Six \n";
                                   esac};
  main():SELF_TYPE {
    {
       out_string("\n case_test_DT\n");
       out_string(case_test_DT(seven));
       out_string("\n");
   }
  };
};
