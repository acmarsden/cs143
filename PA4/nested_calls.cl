Class A  {
    a_fun() : Int {5};
    get_self(a : String) : A { { self; } };
};


Class Main inherits IO{
    main() : Int {
        (let a_main : A <- (new A).get_self("a") in a_main.a_fun() )
    };

};
