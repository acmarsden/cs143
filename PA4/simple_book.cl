Class A inherits IO {
    
    a : String; 
    initA( a_init : String) : A {
        { a <- a_init;
        self; }
    };
    g() : Int { 5};

    print() : Object { out_string("\n Class A \n ") };
};


Class B inherits IO { 
    ab : A;
    init(ab_init : A) : B {
        { ab<- ab_init; self;}
    };

    f() : Int { ab.g() };
    
    printB() : Object {
        ab.print()
    };
 
};

Class Main inherits IO{
    b_main : B;

    get_int() : Int { 
        (let a_main : A <- (new A ) in {a_main.initA("Initial A ");  b_main <- (new B).init(a_main); b_main.f(); } )
          };

    main() : Object {
        {out_string("\n The number is: \n "); out_int(get_int());}
        --(let a_main : A <- (new A ).initA("Initial A ") in { b_main <- (new B).init(a_main); b_main.f(); } )
        --(let a_main : A <- (new A) in { a_main.initA("Initial A "); b_main <- (new B).init(a_main); b_main.printB(); } )
    };

};
