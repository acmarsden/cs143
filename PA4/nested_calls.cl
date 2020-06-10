Class A  {
    a_fun() : Int {5};
    get_self() : A { { self; } };
};


Class Main inherits IO{
    main() : Int {
        (let a_main : A <- (new A).get_self() in a_main.a_fun() )
    };

};
