class I {
    b : SELF_TYPE;
    
	letter(x : Int) : Int {
	    let self : I <- b in x
    };
};

Class Main {
    main() : Int { 0 };
};



