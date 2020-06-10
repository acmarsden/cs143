Class A  {
    a_fun() : Int {5};
    get_self(s : String) : SELF_TYPE { {s; self; } };
};


Class Main inherits IO{
    main() : Int {
        (let a_main : A <- (new A).get_self("a") in a_main.a_fun() )
    };

};
