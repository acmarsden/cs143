Class A inherits IO {
    
    a : String; 
    initA( a_init : String) : A {
        { a <- a_init;
        self; }
    };
    print() : Object { out_string("\n Class A \n ") };
};


Class B inherits IO { 
    ab : A;
    init(ab_init : A) : B {
        { ab<- ab_init; self;}
    };
    
    printB() : Object {
        ab.print()
    };
 
};

Class Main {
    b_main : B;
    main() : Object {
        
         -- (let a_main : A <- (new A ).initA("Initial A ") in { b_main <- new B.init(a_main); b_main.printB(); } )
        (let a_main : A <- (new A) in { a_main.initA("Initial A "); b_main <- new B.init(a_main); b_main.printB(); } )
    };

};
