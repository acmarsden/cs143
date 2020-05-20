

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
    install_basic_classes();

    for(int i=classes->first(); classes->more(i); i=classes->next(i)) {
        Class_ curr_class = classes->nth(i);
        if(_DEBUG) printf("%s: %s\n", curr_class->getName()->get_string(),
               curr_class->getParent()->get_string());
        Features feature_list = classes->nth(i)->getFeatures();
        Symbol node = curr_class->getName();
        Symbol parent = curr_class->getParent();
        if(children.find(node) == children.end()){
            children[node] = std::vector<Symbol>();
        }
        children[parent].push_back(node);
        symb_class_map[node] = curr_class;
    }

    _has_cycle = has_cycle_bfs();
}

bool ClassTable::has_cycle_bfs() {
    std::set<Symbol> discovered;
    std::queue<Symbol> Q;
    if(_DEBUG) printf("Number of children: %lu\n", children.size());
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
        if(_DEBUG) printf("Visiting: %s\n", current_vertex->get_string());
        Q.pop();
        std::vector<Symbol> current_children = children[current_vertex];
        for (auto it=current_children.begin(); it!=current_children.end(); ++it) {
            Symbol current_child = *it;
            if(_DEBUG) printf("bfs visiting: %s\n", current_child->get_string());
            if(_DEBUG) printf("%d\n", discovered.find(current_child) == discovered.end());
            if(discovered.find(current_child) == discovered.end()) {
                // The node hasn't already been discovered
                Q.push(current_child);
                if(_DEBUG) printf("capacity: %lu\n", Q.size());
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

void ClassTable::run_type_checks()
{
    // Traverse the inheritance tree in DFS order
    std::set<Symbol> visited;
    // enter scope
    objectST.enterscope();
    methodST.enterscope();
    run_type_checks_r(Object, &visited);
    objectST.exitscope();
    methodST.enterscope();
}

void ClassTable::run_type_checks_r(Symbol curr_class, std::set<Symbol>* visited)
{
    // Do whatever you need to do
    // check_features?
    visited->insert(curr_class);
    if(_DEBUG) printf("DFS visiting: %s\n", curr_class->get_string());
    check_features(curr_class);
    if(children[curr_class].size() != 0){
        std::vector<Symbol> curr_children = children[curr_class];
        for(auto it=curr_children.begin(); it!=curr_children.end(); ++it){
            if(visited->find(*it) == visited->end()){
                // enter scope
                objectST.enterscope();
                methodST.enterscope();
                run_type_checks_r(*it, visited);
                if(_DEBUG) objectST.dump();
                objectST.exitscope();
                methodST.enterscope();
            }
        }
    }
}

void ClassTable::check_features(Symbol curr_class) {
    Features feature_list = symb_class_map[curr_class]->getFeatures();
    // First, let's gather all the identifier information for this class
    for(int i=feature_list->first(); feature_list->more(i); i=feature_list->next(i)) {
        Feature curr_feature = feature_list->nth(i);
        curr_feature->addToScope(this);
    }
    // Now that we have all the object and methid ids, let's type check them
    for(int i=feature_list->first(); feature_list->more(i); i=feature_list->next(i)) {
        Feature curr_feature = feature_list->nth(i);
        bool is_attr = curr_feature->isAttribute();
        Symbol inferred_type = curr_feature->typeCheck(this);
        Symbol declared_type;
        Symbol feat_name = curr_feature->getName();
        if(is_attr){
            Symbol* lookup = objectST.lookup(feat_name);
            if(lookup != NULL){
                declared_type = *lookup;
            }else{
                printf("Error\n");
            }
            // assert this is equal to curr_feature->getType()?
        }else{
            std::vector<Symbol>* lookup = methodST.lookup(feat_name);
            if(lookup != NULL){
                declared_type = lookup->front();
            }else{
                printf("Error\n");
            }
            // assert this is equal to curr_feature->getType()?
        }
        // TODO: compare declared type and inferred type
    }
}

void attr_class::addToScope(ClassTable* classtable) {
    classtable->objectST.addid(name, &type_decl);
}

void method_class::addToScope(ClassTable* classtable) {
    //Check whether this method is a redefinition and if so make sure it adheres
    std::vector<Symbol> data;
    data.push_back(return_type);
    for(int i=formals->first(); formals->more(i); i=formals->next(i)) {
        Symbol formal_type = formals->nth(i)->getType();
        data.push_back(formal_type);
    }

    //Check whether this method is a redefinition and if so make sure it adheres
    std::vector<Symbol>* lookup = methodST.lookup(name);
    if(lookup!=NULL){
        if(data != *lookup){
            if(DEBUG_){
                printf("Error: method %s is a redefinition which does not follow inheritance rules.\n",
                name->get_string());
            }
        }
    }
    classtable->methodST.addid(name, &data);
    //call addToScope on the expression of the method
    expr->addToScope(classtable);
}

void formal_class::addToScope(ClassTable* classtable) {
    classtable->objectST.addid(name, &type_decl);
}

void branch_class::addToScope(ClassTable* classtable) {
    classtable->objectST.addid(name, &type_decl);
}

void let_class::addToScope(ClassTable* classtable){
    classtable->objectST.addid(identifier, &type_decl);
}

Symbol attr_class::typeCheck(ClassTable* classtable) {
    // Check that the attribute type has been defined.
    if(classtable->children.find(type_decl) == classtable->children.end()) {
        if(_DEBUG) printf("Attribute type error: %s is not defined\n", type_decl->get_string());
    }
    if(classtable->objectST.lookup(name)!=NULL){
        if(_DEBUG) printf("Error: Attribute %s has already been defined.\n", name->get_string()); }
    else {
        classtable->objectST.addid(name, &type_decl);
    }
    return type_decl;
}

Symbol method_class::typeCheck(ClassTable* classtable){
    std::set<Symbol> formal_names;
    std::vector<Symbol> formal_types;
    // Check the formals list for previous definition in a parent class
    for(int i=formals->first(); formals->more(i); i=formals->next(i)) {
        Symbol formal_name = formals->nth(i)->getName();
        Symbol formal_type = formals->nth(i)->getType();
        //Check that the identifiers in the formal params are distinct
        if(formal_names.find(formal_name) != formal_names.end()){
            if(_DEBUG) {
                printf("Formal error: %s is not a distinct formal identifier for method %s",
                       formal_name->get_string(),
                       name->get_string() );
            }
        }
        formal_names.insert(formal_name);
        formal_types.push_back(formal_type);
    }
    // Check for a previous definition of this method in the class hierarchy
    std::vector<Symbol>* lookup = classtable->methodST.lookup(name);
    if(lookup != NULL){
        // If it did find a match, the defintions must conform
        bool matches = true;
        if(return_type != (*lookup)[1]){
            matches = false;
        }
        for(uint i=0; i<formal_types.size(); ++i){
            if(formal_types[i] != (*lookup)[i+1]){
                matches = false;
            }
        }
        if(!matches){
            printf("Method formals list or return type does not conform to parent definition\n");
        }
    }
    // If it did not find a match, we are OK, since it is the first time it is
    // defined in the class hierarchy

    // Now enter a new scope, add formals to it, recurse on the body and exit scope
    classtable->objectST.enterscope();
    for(int i=formals->first(); formals->more(i); i=formals->next(i)) {
        Symbol formal_name = formals->nth(i)->getName();
        Symbol formal_type = formals->nth(i)->getType();
        classtable->objectST.addid(formal_name, &formal_type);
    }
    Symbol inferred_return_type = expr.typeCheck(classtable);
    classtable->objectST.exitscope();

    // Check the inferred type against the declared return type
    if(inferred_return_type == return_type){
        return inferred_return_type;
    }else{
        // TODO: maybe an error here?
        return Object;
    }
}

Symbol formal_class::typeCheck(ClassTable* classtable){
    // TODO
    return Object;
}

Symbol branch_class::typeCheck(ClassTable* classtable) {
// TODO
    return Object;
}

Symbol assign_class::typeCheck(ClassTable* classtable) {
// TODO
    return Object;
}

Symbol static_dispatch_class::typeCheck(ClassTable* classtable) {
// TODO
    return Object;
}

Symbol dispatch_class::typeCheck(ClassTable* classtable) {
// TODO
    return Object;
}

Symbol cond_class::typeCheck(ClassTable* classtable) {
// TODO
    return Object;
}

Symbol loop_class::typeCheck(ClassTable* classtable) {
// TODO
    return Object;
}

Symbol typcase_class::typeCheck(ClassTable* classtable) {
// TODO
    return Object;
}

Symbol block_class::typeCheck(ClassTable* classtable) {
// TODO
    return Object;
}

Symbol let_class::typeCheck(ClassTable* classtable) {
// TODO
    return Object;
}

Symbol plus_class::typeCheck(ClassTable* classtable) {
// TODO
    return Object;
}

Symbol sub_class::typeCheck(ClassTable* classtable) {
// TODO
    return Object;
}

Symbol mul_class::typeCheck(ClassTable* classtable) {
// TODO
    return Object;
}

Symbol divide_class::typeCheck(ClassTable* classtable) {
// TODO
    return Object;
}

Symbol neg_class::typeCheck(ClassTable* classtable) {
// TODO
    return Object;
}

Symbol lt_class::typeCheck(ClassTable* classtable) {
// TODO
    return Object;
}

Symbol eq_class::typeCheck(ClassTable* classtable) {
// TODO
    return Object;
}

Symbol leq_class::typeCheck(ClassTable* classtable) {
// TODO
    return Object;
}

Symbol comp_class::typeCheck(ClassTable* classtable) {
// TODO
    return Object;
}


Symbol int_const_class::typeCheck(ClassTable* classtable) {
// TODO
    return Object;
}

Symbol bool_const_class::typeCheck(ClassTable* classtable) {
// TODO
    return Object;
}

Symbol string_const_class::typeCheck(ClassTable* classtable) {
// TODO
    return Object;
}

Symbol new__class::typeCheck(ClassTable* classtable) {
// TODO
    return Object;
}

Symbol isvoid_class::typeCheck(ClassTable* classtable) {
// TODO
    return Object;
}

Symbol no_expr_class::typeCheck(ClassTable* classtable) {
// TODO
    return Object;
}

Symbol object_class::typeCheck(ClassTable* classtable) {
// TODO
    return Object;
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
    _DEBUG = true;
    initialize_constants();

    /* ClassTable constructor may do some semantic analysis */
    ClassTable *classtable = new ClassTable(classes);

    /* some semantic analysis code may go here */
    // Stop semantic analysis if inheritance graph is not well formed
    if(!classtable->has_cycle()){
        // continue semantic analysis
        if(_DEBUG) printf("No inheritance cycles\n");
        classtable->run_type_checks();
    }
    if(_DEBUG) printf("========= END Constructor output =========\n");

    if (classtable->errors()) {
        cerr << "Compilation halted due to static semantic errors." << endl;
        exit(1);
    }
}

