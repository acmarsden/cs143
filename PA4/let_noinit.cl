
(*  Example cool program testing as many aspects of the code generator
    as possible.
 *)
class Main inherits IO {
  -- use default values 
  let_test(): Int {let x:Int in let y : Bool  in if x = 0 then (if y = false then 7 else 999 fi) else 888 fi };   
  main():SELF_TYPE { 
    { out_string("\n Testing let with no initialization \n"); out_int(let_test());
   }
  };
};

