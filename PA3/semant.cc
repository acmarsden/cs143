

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include "semant.h"
#include "utilities.h"
#include <map>
#include <vector>
#include <iostream>
#include <set>
#include <queue>
#include <stack>
extern int semant_debug;
extern char *curr_filename;

//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
static Symbol
    arg,
    arg2,
    Bool,
    concat,
    cool_abort,
    copy,
    Int,
    in_int,
    in_string,
    IO,
    length,
    Main,
    main_meth,
    No_class,
    No_type,
    Object,
    out_int,
    out_string,
    prim_slot,
    self,
    SELF_TYPE,
    Str,
    str_field,
    substr,
    type_name,
    val;
//
// Initializing the predefined symbols.
//
static void initialize_constants(void)
{
    arg         = idtable.add_string("arg");
    arg2        = idtable.add_string("arg2");
    Bool        = idtable.add_string("Bool");
    concat      = idtable.add_string("concat");
    cool_abort  = idtable.add_string("abort");
    copy        = idtable.add_string("copy");
    Int         = idtable.add_string("Int");
    in_int      = idtable.add_string("in_int");
    in_string   = idtable.add_string("in_string");
    IO          = idtable.add_string("IO");
    length      = idtable.add_string("length");
    Main        = idtable.add_string("Main");
    main_meth   = idtable.add_string("main");
    //   _no_class is a symbol that can't be the name of any
    //   user-defined class.
    No_class    = idtable.add_string("_no_class");
    No_type     = idtable.add_string("_no_type");
    Object      = idtable.add_string("Object");
    out_int     = idtable.add_string("out_int");
    out_string  = idtable.add_string("out_string");
    prim_slot   = idtable.add_string("_prim_slot");
    self        = idtable.add_string("self");
    SELF_TYPE   = idtable.add_string("SELF_TYPE");
    Str         = idtable.add_string("String");
    str_field   = idtable.add_string("_str_field");
    substr      = idtable.add_string("substr");
    type_name   = idtable.add_string("type_name");
    val         = idtable.add_string("_val");
}



ClassTable::ClassTable(Classes classes) : semant_errors(0) , error_stream(cerr) {

    /* Fill this in */

    install_basic_classes();

    for(int i=classes->first(); classes->more(i); i=classes->next(i)) {
        Class_ curr_class = classes->nth(i);
        printf("%s: %s\n", curr_class->getName()->get_string(), 
			   curr_class->getParent()->get_string());
        Features feature_list = classes->nth(i)->getFeatures();
        Symbol node = curr_class->getName();
        Symbol parent = curr_class->getParent();
        if(children.find(node) == children.end())
	    children[node] = std::vector<Symbol>();
        children[parent].push_back(node);
	symb_class_map[node] = curr_class;
    }

    _has_cycle = has_cycle_bfs();
}

bool ClassTable::has_cycle_bfs() {
    std::set<Symbol> discovered;
    std::queue<Symbol> Q;
    printf("Number of children: %lu\n", children.size());
    discovered.insert(Object);
    Q.push(Object);
    while(!Q.empty() || discovered.size() < children.size()) {
	if(Q.empty()){
	    for (auto it=children.begin(); it!=children.end(); ++it) {
                if(discovered.find(it->first) == discovered.end()) {
	            Q.push(it->first);
		    break;
		}
	    }
	}
        Symbol current_vertex = Q.front();
	printf("Visiting: %s\n", current_vertex->get_string());
        Q.pop();
        std::vector<Symbol> current_children = children[current_vertex];
        //for(uint iter = 0; iter < current_children.size(); iter++) {
	for (auto it=current_children.begin(); it!=current_children.end(); ++it) {
	    //Symbol current_child = current_children[iter];
	    Symbol current_child = *it;
	    printf("bfs visiting: %s\n", current_child->get_string());
            printf("%d\n", discovered.find(current_child) == discovered.end());
            if(discovered.find(current_child) == discovered.end()) {
                // The node hasn't already been discovered
                Q.push(current_child);
		printf("capacity: %lu\n", Q.size());
                discovered.insert(current_child);
            }
            else {
                // The node had already been discovered so we know there is a cycle
		ostream& err_stream = semant_error(symb_class_map[current_child]);
                err_stream << current_child->get_string() <<" is part of a cycle in the inheritance graph."<<endl;
                return true;
            }
        }

    }
    return false;
}

void ClassTable::install_basic_classes() {

    // The tree package uses these globals to annotate the classes built below.
   // curr_lineno  = 0;
    Symbol filename = stringtable.add_string("<basic class>");

    // The following demonstrates how to create dummy parse trees to
    // refer to basic Cool classes.  There's no need for method
    // bodies -- these are already built into the runtime system.

    // IMPORTANT: The results of the following expressions are
    // stored in local variables.  You will want to do something
    // with those variables at the end of this method to make this
    // code meaningful.

    //
    // The Object class has no parent class. Its methods are
    //        abort() : Object    aborts the program
    //        type_name() : Str   returns a string representation of class name
    //        copy() : SELF_TYPE  returns a copy of the object
    //
    // There is no need for method bodies in the basic classes---these
    // are already built in to the runtime system.

    Class_ Object_class =
    class_(Object,
           No_class,
           append_Features(
                   append_Features(
                           single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
                           single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
                   single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
           filename);

    //
    // The IO class inherits from Object. Its methods are
    //        out_string(Str) : SELF_TYPE       writes a string to the output
    //        out_int(Int) : SELF_TYPE            "    an int    "  "     "
    //        in_string() : Str                 reads a string from the input
    //        in_int() : Int                      "   an int     "  "     "
    //
    Class_ IO_class =
    class_(IO,
           Object,
           append_Features(
                   append_Features(
                           append_Features(
                                   single_Features(method(out_string, single_Formals(formal(arg, Str)),
                                              SELF_TYPE, no_expr())),
                                   single_Features(method(out_int, single_Formals(formal(arg, Int)),
                                              SELF_TYPE, no_expr()))),
                           single_Features(method(in_string, nil_Formals(), Str, no_expr()))),
                   single_Features(method(in_int, nil_Formals(), Int, no_expr()))),
           filename);

    //
    // The Int class has no methods and only a single attribute, the
    // "val" for the integer.
    //
    Class_ Int_class =
    class_(Int,
           Object,
           single_Features(attr(val, prim_slot, no_expr())),
           filename);

    //
    // Bool also has only the "val" slot.
    //
    Class_ Bool_class =
    class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())),filename);

    //
    // The class Str has a number of slots and operations:
    //       val                                  the length of the string
    //       str_field                            the string itself
    //       length() : Int                       returns length of the string
    //       concat(arg: Str) : Str               performs string concatenation
    //       substr(arg: Int, arg2: Int): Str     substring selection
    //
    Class_ Str_class =
    class_(Str,
           Object,
           append_Features(
                   append_Features(
                           append_Features(
                                   append_Features(
                                           single_Features(attr(val, Int, no_expr())),
                                           single_Features(attr(str_field, prim_slot, no_expr()))),
                                   single_Features(method(length, nil_Formals(), Int, no_expr()))),
                           single_Features(method(concat,
                                      single_Formals(formal(arg, Str)),
                                      Str,
                                      no_expr()))),
                   single_Features(method(substr,
                              append_Formals(single_Formals(formal(arg, Int)),
                                     single_Formals(formal(arg2, Int))),
                              Str,
                              no_expr()))),
           filename);

    children[Object].push_back(IO);
    children[Object].push_back(Int);
    children[Object].push_back(Bool);
    children[Object].push_back(Str);
    children[IO] = std::vector<Symbol>();
    children[Int] = std::vector<Symbol>();
    children[Bool] = std::vector<Symbol>();
    children[Str] = std::vector<Symbol>();

    symb_class_map[Object] = Object_class;
    symb_class_map[IO] = IO_class;
    symb_class_map[Int] = Int_class;
    symb_class_map[Bool] = Bool_class;
    symb_class_map[Str] = Str_class;

}

void ClassTable::type_checks()
{
	// Traverse the inheritance tree in DFS order
	std::stack<Symbol> S;
	std::set<Symbol> visited;
	S.push(Object);
	while(S.size()>0){
		Symbol current_class_ = S.top();
		Class_ current_class = symb_class_map.find(current_class_)->second;
		visited.insert(current_class_);
		printf("DFS visiting: %s\n", current_class_->get_string());
		S.pop();
		std::vector<Symbol> curr_children = children[current_class_];
		for(auto it=curr_children.begin(); it!=curr_children.end(); ++it){
			if(visited.find(*it) == visited.end()){
				S.push(*it);
			}
		}
		// Now do stuff with the other nodes
	}
}

////////////////////////////////////////////////////////////////////
//
// semant_error is an overloaded function for reporting errors
// during semantic analysis.  There are three versions:
//
//    ostream& ClassTable::semant_error()
//
//    ostream& ClassTable::semant_error(Class_ c)
//       print line number and filename for `c'
//
//    ostream& ClassTable::semant_error(Symbol filename, tree_node *t)
//       print a line number and filename
//
///////////////////////////////////////////////////////////////////

ostream& ClassTable::semant_error(Class_ c)
{
    return semant_error(c->get_filename(),c);
}

ostream& ClassTable::semant_error(Symbol filename, tree_node *t)
{
    error_stream << filename << ":" << t->get_line_number() << ": ";
    return semant_error();
}

ostream& ClassTable::semant_error()
{
    semant_errors++;
    return error_stream;
}


/*   This is the entry point to the semantic checker.

     Your checker should do the following two things:

     1) Check that the program is semantically correct
     2) Decorate the abstract syntax tree with type information
        by setting the `type' field in each Expression node.
        (see `tree.h')

     You are free to first do 1), make sure you catch all semantic
     errors. Part 2) can be done in a second stage, when you want
     to build mycoolc.
 */
void program_class::semant()
{
    initialize_constants();

    /* ClassTable constructor may do some semantic analysis */
    ClassTable *classtable = new ClassTable(classes);

    /* some semantic analysis code may go here */
    // Stop semantic analysis if inheritance graph is not well formed
    if(!classtable->has_cycle()){
        // continue semantic analysis
        printf("No inheritance cycles\n");
	classtable->type_checks();
    }
    printf("========= END Constructor output =========\n");

    if (classtable->errors()) {
    cerr << "Compilation halted due to static semantic errors." << endl;
    exit(1);
    }
}


