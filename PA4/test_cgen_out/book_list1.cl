-- example of static and dynamic type differing for a dispatch

Class Book inherits IO {
    title : String <- "Dummy Title";

    initBook(title_p : String) : Book {
        {
            title <- title_p;
            self;
        }
    };

    print() : Object {
        {
            out_string("title:      ").out_string(title).out_string("\n");
        }
    };
};


Class Cons inherits IO {
    xcar : Book;  -- We keep the car and cdr in attributes.
    init(hd : Book) : Cons {
        {
            xcar <- hd;  -- Error in attr assignment here? 
            self;
        }
    };

    
    print_list() : Object {
            xcar.print() 
    };
};


Class Main {
    books : Cons;

    main() : Object {
        (let a_book : Book <-
            (new Book).initBook("Compilers, Principles, Techniques, and Tools")
        in
                {
                    --books <- (new Nil).cons(a_book).cons(an_article);
                    --books <- new BookList.cons(a_book);
                    books <- new Cons.init(a_book);
                    books.print_list();
                }
        )  -- end let a_book
    };
};
