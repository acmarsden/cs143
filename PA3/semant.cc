

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

        // This data structure will be useful to traverse the classes in
        // DFS order of inheritance
        if(children.find(node) == children.end()){
            children[node] = std::vector<Symbol>();
        }
        children[parent].push_back(node);

        // Take this opportunity to check no class is not defined multiple times
        if(symb_class_map.find(node) != symb_class_map.end()){
            // this class was already defined before!
            ostream& err_stream = semant_error(curr_class);
            err_stream << "Class '" << node->get_string() << "' is multiply defined."<<endl;
        }else{
            symb_class_map[node] = curr_class;
        }
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
    // This guarantees that methodST at any given point only has
    // members of the present class or its ancestors
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

Symbol ClassTable::compute_join(std::vector<Symbol> symbol_vec) {
//Sequentially compute the least type C such that C_old \leq C and Type_i \leq C
//Set C_old to be C
//TODO: We need to implement SELF_TYPE in here.
    std::set<Symbol> seen_symbols;
    Symbol join = Object;
    join = symbol_vec[0];
    seen_symbols.insert(symbol_vec[0]);
    for(uint i=1; i<symbol_vec.size(); ++i){
        if(seen_symbols.find(symbol_vec[i])==seen_symbols.end()){
            //This symbol has NOT already been included in join so we need to update join.
            join = compute_join_pair(join, symbol_vec[i]);
            seen_symbols.insert(symbol_vec[i]);
        }
    }
    return join;
}

Symbol ClassTable::compute_join_pair(Symbol symbolA, Symbol symbolB) {
    if(symbolA == symbolB) return symbolA;
    //Increment A's ancestors one by one and check against all of B's ancestors
    //First get B's ancestors
    std::set<Symbol> b_ancestors;
    Symbol curr_symbolB = symbolB;
    bool found_all_ancestors = false;
    while(!found_all_ancestors){
        if(curr_symbolB == Object) found_all_ancestors = true;
        b_ancestors.insert(symbolB);
        curr_symbolB = symb_class_map[curr_symbolB]->getParent();
    }

    Symbol curr_symbolA = symbolA;
    bool found_join = false;
    Symbol join_pair = Object;
    while(!found_join){
        if(b_ancestors.find(curr_symbolA)!=b_ancestors.end()){
            //Found join
            join_pair = curr_symbolA;
            found_join = true;
        }
        curr_symbolA =  symb_class_map[curr_symbolA]->getParent();
    }
    return join_pair;
}

void ClassTable::check_features(Symbol curr_class) {
    this->current_class = curr_class;
    Features feature_list = symb_class_map[curr_class]->getFeatures();
    // First, let's gather all the identifier information for this class
    for(int i=feature_list->first(); feature_list->more(i); i=feature_list->next(i)) {
        Feature curr_feature = feature_list->nth(i);
        curr_feature->addToScope(this);
    }
    // Now that we have all the object and methid ids, let's type check them,
    // but not the base classes
    if(curr_class != Object && curr_class != Str && curr_class != Int &&
       curr_class != IO && curr_class != Bool){
        for(int i=feature_list->first(); feature_list->more(i); i=feature_list->next(i)) {
            Feature curr_feature = feature_list->nth(i);
            curr_feature->typeCheck(this);
        }
    }
}

bool ClassTable::isDescendantOf(Symbol parent, Symbol query_type) {
    // Starting at parent, recursively checks its children to see if they
    // contain query_type
    std::vector<Symbol> curr_children = children[parent];
    for(auto it=curr_children.begin(); it!=curr_children.end(); ++it){
        Symbol child_type = *it;
        if(child_type==query_type){
            //query_type does indeed inherit from parent
            return true;
        }
        else{
            return isDescendantOf(child_type, query_type);
        }
    }
    //Should return false if you have no children since we already checked that you weren't query_type
    return false;
}

void attr_class::addToScope(ClassTable* classtable) {
    // Don´t do this for base classes
    Symbol curr_class = classtable->getCurrentClass();
    if(curr_class != Object && curr_class != Str && curr_class != Int &&
           curr_class != IO && curr_class != Bool){
        // Check that the attribute type has been defined.
        if(classtable->children.find(type_decl) == classtable->children.end()) {
            if(_DEBUG) printf("Attribute type error: '%s' is not defined\n", type_decl->get_string());
            ostream& err_stream = classtable->semant_error(classtable->symb_class_map[curr_class]);
            err_stream << "Attribute type error: '" << type_decl->get_string() << "'' is not defined" << endl;
        }
        // Check if an attribute with the same name already exists (since attributes are global)
        if(classtable->objectST.probe(name)!=NULL){
            if(_DEBUG) printf("Error: Attribute '%s' has already been defined.\n", name->get_string());
            ostream& err_stream = classtable->semant_error(classtable->symb_class_map[curr_class]);
            err_stream << "Error: Attribute '"<< name->get_string() << "'' has already been defined." << endl;
        }
        else {
            if(type_decl == SELF_TYPE) {
                classtable->objectST.addid(name, &(classtable->getCurrentClass()));
            }else{
                classtable->objectST.addid(name, &type_decl);
            }

        }
    }
}

void method_class::addToScope(ClassTable* classtable) {
    Symbol curr_class = classtable->getCurrentClass();
    std::vector<Symbol> data;
    if(return_type == SELF_TYPE) {
        data.push_back(classtable->getCurrentClass());
    }else{
        data.push_back(return_type);
    }

    for(int i=formals->first(); formals->more(i); i=formals->next(i)) {
        Symbol formal_type = formals->nth(i)->getType();
        if(formal_type == SELF_TYPE) {
            ostream& err_stream = classtable->semant_error(classtable->symb_class_map[curr_class]);
            err_stream << "Formal '" << formals->nth(i)->getName()->get_string();
             err_stream << "' was declared to be SELF_TYPE which is not allowed" << endl;
        }
        data.push_back(formal_type);
    }

    // Check for a previous definition of this method in the class hierarchy
    std::vector<Symbol>* lookup = classtable->methodST.lookup(name);
    if(lookup != NULL){
        // If it did find a match, the defintions must conform
        bool matches = true;
        for(uint i=0; i<data.size(); ++i){
            if(data[i] != (*lookup)[i]){
                matches = false;
            }
        }
        if(!matches){
            ostream& err_stream = classtable->semant_error(classtable->symb_class_map[curr_class]);
            err_stream << "Method formals list or return type does not conform to parent definition" << endl;
        }
    }
    // If it did not find a match, we are OK, since it is the first time it is
    // defined in the class hierarchy

    // Now add it to the present scope
    classtable->methodST.addid(name, &data);
    // and add the method signature to this scope-independent data structure
    classtable->classMethods[curr_class][name] = data;
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
    // Quite similar to assign_class
    Symbol curr_class = classtable->getCurrentClass();
    Symbol declared_type;
    Symbol* lookup = classtable->objectST.probe(name);
    if(lookup != NULL){
        // will already have SELF_TYPE resolved
        declared_type = *lookup;
    }else{
        ostream& err_stream = classtable->semant_error(classtable->symb_class_map[curr_class]);
        err_stream << "Error: Attribute '" << name->get_string() << "'' was not declared in the current scope." << endl;
        return Object;
    }

    // Now get the type of the assign expression
    Symbol inferred_init_type = init->typeCheck(classtable);
    if(inferred_init_type != No_type){
        // Check that the type of the expression conforms to declared_type
        // i.e. that inferred_assign_type inherits from declared_type
        if(declared_type!=inferred_init_type){
            bool inheritance_found = classtable->isDescendantOf(declared_type, inferred_init_type);
            if(!inheritance_found){
                if(_DEBUG) printf("Attribute init error: Assignment expression type does not conform to declared Id type.\n");
                ostream& err_stream = classtable->semant_error(classtable->symb_class_map[curr_class]);
                err_stream << "Attribute init error: Assignment expression type does not conform to declared Id type." << endl;
                return Object;
            }
        }
        return inferred_init_type;
    }else{
        return declared_type;
    }
}

Symbol method_class::typeCheck(ClassTable* classtable){
    Symbol curr_class = classtable->getCurrentClass();
    std::set<Symbol> formal_names;
    Symbol declared_return_type;

    // Check the method exists
    std::vector<Symbol>* lookup = classtable->methodST.lookup(name);
    if(lookup != NULL){
        // will already have SELF_TYPE resolved
        declared_return_type = lookup->front();
    }else{
        ostream& err_stream = classtable->semant_error(classtable->symb_class_map[curr_class]);
        err_stream << "Error: Did not find method '"<< name->get_string();
        err_stream << "'' in the current scope." << endl;
        return Object;
    }

    // Enter a new scope, add formals to it, recurse on the body and exit scope
    classtable->objectST.enterscope();
    for(int i=formals->first(); formals->more(i); i=formals->next(i)) {
        Symbol formal_name = formals->nth(i)->getName();
        Symbol formal_declared_type = formals->nth(i)->getType();

        // Check that the types of the formals have been prevously declared
        Symbol formal_inferred_type = formals->nth(i)->typeCheck(classtable);
        if(!classtable->isDescendantOf(formal_declared_type, formal_inferred_type)){
            // The type of the formal was not declared before
            //(already caught by formal_class::typeCheck)
            ostream& err_stream = classtable->semant_error(classtable->symb_class_map[curr_class]);
            err_stream << "Error: In method '" << name->get_string();
            err_stream << "', the type '" << formal_declared_type->get_string();
            err_stream << "' of formal '" << formal_name->get_string();
            err_stream << "'' was not defined." << endl;
            return Object;
        }

        // Check that the identifiers in the formal params are distinct
        if(formal_names.find(formal_name) != formal_names.end()){
            if(_DEBUG) {
                printf("Formal error: %s is not a distinct formal identifier for method %s",
                       formal_name->get_string(),
                       name->get_string() );
            }
            ostream& err_stream = classtable->semant_error(classtable->symb_class_map[curr_class]);
            err_stream << "Formal error: " << formal_name->get_string();
            err_stream << " is not a distinct formal identifier for method ";
            err_stream << name->get_string() << endl;
        }
        formal_names.insert(formal_name);

        classtable->objectST.addid(formal_name, &formal_declared_type);
    }

    expr->addToScope(classtable);
    Symbol inferred_return_type = expr->typeCheck(classtable);
    classtable->objectST.exitscope();

    // Check the inferred type conforms to the declared return type
    if(classtable->isDescendantOf(declared_return_type, inferred_return_type)){
        return declared_return_type;
    }else{
        ostream& err_stream = classtable->semant_error(classtable->symb_class_map[curr_class]);
        err_stream << "Method error: '" << name->get_string();
        err_stream << "' returns type '" << inferred_return_type->get_string();
        err_stream << "' but type '" << declared_return_type->get_string();
        err_stream << "' was declared."<< endl;
        return Object;
    }
}

Symbol formal_class::typeCheck(ClassTable* classtable){
    Symbol curr_class = classtable->getCurrentClass();
    // Only checks if the type of the formal was previously defined
    if(classtable->symb_class_map.find(type_decl) != classtable->symb_class_map.end()){
        return type_decl;
    }else{
        ostream& err_stream = classtable->semant_error(classtable->symb_class_map[curr_class]);
        err_stream << "Error: The type '" << type_decl->get_string() << "'' was not defined." << endl;
        return Object;
    }
}

Symbol branch_class::typeCheck(ClassTable* classtable) {
    Symbol curr_class = classtable->getCurrentClass();
    classtable->objectST.enterscope();
    this->addToScope(classtable);
    Symbol expr_inferred_type = expr->typeCheck(classtable);
    classtable->objectST.exitscope();

    // Check if type_decl is defined
    if(classtable->symb_class_map.find(type_decl) == classtable->symb_class_map.end()){
        ostream& err_stream = classtable->semant_error(classtable->symb_class_map[curr_class]);
        err_stream << "Error: The type '" << type_decl->get_string() << "'' was not defined." << endl;
    }
    return expr_inferred_type;
}

Symbol assign_class::typeCheck(ClassTable* classtable) {
    Symbol curr_class = classtable->getCurrentClass();
    // First get O(Id), i.e. the type the environment gives to the id
    Symbol* declared_type = classtable->objectST.probe(name);
    // Now get the type of the expression
    Symbol inferred_assign_type = expr->typeCheck(classtable);
    // Check that the type of the expression conforms to declared_type
    // i.e. that inferred_assign_type inherits from declared_type
    if(*declared_type!=inferred_assign_type){
        bool inheritance_found = classtable->isDescendantOf(*declared_type, inferred_assign_type);
        if(!inheritance_found){
            if(_DEBUG) printf("Assign error: Expression type does not conform to Id type.\n");
            ostream& err_stream = classtable->semant_error(classtable->symb_class_map[curr_class]);
            err_stream << "Assign error: Expression type does not conform to Id type." << endl;
            return Object;
        }
        else return inferred_assign_type;
    }
    else{
         // return the type as the type of the expression
        return inferred_assign_type;
    }
}

Symbol static_dispatch_class::typeCheck(ClassTable* classtable) {
    Symbol curr_class = classtable->getCurrentClass();
    // First get the type for the base expression e_0
    Symbol inferred_expr_type = expr->typeCheck(classtable);
    // Check that this type conforms with what is declared
    // This part is what makes it STATIC DISPATCH
    if(!classtable->isDescendantOf(type_name, inferred_expr_type)){
        ostream& err_stream = classtable->semant_error(classtable->symb_class_map[curr_class]);
        err_stream << "Static Dispatch Error: Expression type '" << inferred_expr_type->get_string()
            << "' does not conform to type name '" << type_name->get_string() << "' for method '" << name->get_string() << "'" << endl;
    }
    // Check that method with "name" is implemented as a method of some parent of the declared type_name
    Symbol curr_type = type_name;
    std::vector<Symbol> curr_signature = classtable->classMethods[curr_type][name];
    bool still_searching_for_method = true;
    while(still_searching_for_method){
        if(curr_signature.size()>0 || curr_type == Object){
            still_searching_for_method = false;
        }
        else{
            curr_type = classtable->symb_class_map[curr_type]->getParent();
            curr_signature = classtable->classMethods[curr_type][name];
        }
    }
    if(curr_signature.size()<1){
        ostream& err_stream = classtable->semant_error(classtable->symb_class_map[curr_class]);
        err_stream << "Static Dispatch Error: Declared type '" << type_name->get_string()
            << "' does not have method '" << name->get_string() << "'" <<  endl;
    }
    // Loop through the expressions and get their inferred return types
    int j = 1;
    for(int i=actual->first(); actual->more(i); i=actual->next(i)) {
        Expression curr_expr = actual->nth(i);
        Symbol inferred_curr_expr_type = curr_expr->typeCheck(classtable);
        // Check that this type inherits from the type given in the method declaration
        // Otherwise return an error
        if(!classtable->isDescendantOf(curr_signature[j], inferred_curr_expr_type)){
            if(_DEBUG) printf("Static Dispatch Error: Declared type's method implementation does not have associated formal type for method.\n");
            ostream& err_stream = classtable->semant_error(classtable->symb_class_map[curr_class]);
            err_stream << "Static Dispatch Error: Declared type '" << type_name->get_string()
                << "' method implementation does not match formals type list. " << endl;
        }
        ++j;
    }
    // Return the return type of the method. This is key part where we need to implement SELF_TYPE
    // If curr_signature[0] == SELF_TYPE then we return inferred_expr_type
    if(curr_signature[0] == SELF_TYPE) {
        return curr_class;
    }
    else{
        return curr_signature[0];
    }
}


Symbol dispatch_class::typeCheck(ClassTable* classtable) {

    Symbol curr_class = classtable->getCurrentClass();
    // First get the type for the base expression e_0
    Symbol inferred_expr_type = expr->typeCheck(classtable);
    if(inferred_expr_type == SELF_TYPE){
        inferred_expr_type = classtable->getCurrentClass();
    }
    // Check that method with "name" is implemented as a method of some parent of the expression type
    Symbol curr_type = inferred_expr_type;
    std::vector<Symbol> curr_signature = classtable->classMethods[curr_type][name];
    bool still_searching_for_method = true;
    while(still_searching_for_method){
        if(curr_signature.size()>0 || curr_type == Object){
            still_searching_for_method = false;
        }
        else{
            printf("LOOK FOR METHOD IN INHERITED CLASS.\n");
            curr_type = classtable->symb_class_map[curr_type]->getParent();
            curr_signature = classtable->classMethods[curr_type][name];
        }
    }
    if(curr_signature.size()<1){
        ostream& err_stream = classtable->semant_error(classtable->symb_class_map[curr_class]);
        err_stream << "Dispatch Error: Expression type '" << inferred_expr_type->get_string()
            << "' does not have method '" << name->get_string() << "'." << endl;
    }
    // Loop through the expressions and get their inferred return types
    int j = 1;
    for(int i=actual->first(); actual->more(i); i=actual->next(i)) {
        Expression curr_expr = actual->nth(i);
        Symbol inferred_curr_expr_type = curr_expr->typeCheck(classtable);
        // Check that this type inherits from the type given in the method declaration
        // Otherwise return an error
        if(!classtable->isDescendantOf(curr_signature[j], inferred_curr_expr_type)){
            ostream& err_stream = classtable->semant_error(classtable->symb_class_map[curr_class]);
            err_stream << "Static Dispatch Error: The formal types listed in dispatch call for expression '"
                << inferred_expr_type->get_string() << "' do not match the formal types declared for method implementation." << endl;
        }
        ++j;
    }
    // Return the return type of the method. This is key part where we need to implement SELF_TYPE
    // If curr_signature[0] == SELF_TYPE then we return inferred_expr_type
    if(curr_signature[0] == SELF_TYPE) {
        return curr_class;
    }
    else{
        return curr_signature[0];
    }
}

Symbol cond_class::typeCheck(ClassTable* classtable) {

    Symbol pred_type = pred->typeCheck(classtable);
    Symbol then_exp_type = then_exp->typeCheck(classtable);
    Symbol else_exp_type = else_exp->typeCheck(classtable);
    Symbol return_type = Object;
    if(pred_type!=Bool){
        if(_DEBUG) printf("If-Statement Error: the predicate is not of type Bool.\n");
    }
    else {
        //Compute join of then_exp_type and else_exp_type
        std::vector<Symbol> symbol_vec;
        symbol_vec.push_back(then_exp_type);
        symbol_vec.push_back(else_exp_type);
        return_type = classtable->compute_join(symbol_vec);
    }

    return return_type;
}

Symbol loop_class::typeCheck(ClassTable* classtable) {
    Symbol pred_type = pred->typeCheck(classtable);
    if(pred_type!=Bool){
        if(_DEBUG) printf("Loop Error: the predicate is not of type Bool. \n");
        Symbol curr_class = classtable->getCurrentClass();
        ostream& err_stream = classtable->semant_error(classtable->symb_class_map[curr_class]);
        err_stream << "Loop Error: the predicate type '" << pred_type->get_string() <<  "' is not of type Bool." << endl;
    }
    return Object;
}

Symbol typcase_class::typeCheck(ClassTable* classtable) {
    Symbol curr_class = classtable->getCurrentClass();
    //Iterate over the cases
    std::vector<Symbol> expr_types_vec;
    std::set<Symbol> declared_types_set;
    for(int i=cases->first(); cases->more(i); i=cases->next(i)) {
        //Compute the type of the expression e_i when x_i has type T_i i.e. O[T/x_i],M,C |- e_i:T_i'
        Case curr_branch = cases->nth(i);
        Symbol branch_expr_inferred_type = curr_branch->typeCheck(classtable);
        expr_types_vec.push_back(branch_expr_inferred_type);

        // check the declared types are all distinct
        Symbol branch_declared_type = curr_branch->getType();
        if(declared_types_set.find(branch_declared_type)==declared_types_set.end()) {
            declared_types_set.insert(branch_declared_type);
        }
        else {
            if(_DEBUG) printf("Case Error: The branches in each case must have distinct types.\n");
            ostream& err_stream = classtable->semant_error(classtable->symb_class_map[curr_class]);
            err_stream << "Case Error: The branches in each case must have distinct types." << endl;
        }
    }
    Symbol return_type = classtable->compute_join(expr_types_vec);
    return return_type;
}

Symbol block_class::typeCheck(ClassTable* classtable) {
    Expression curr_expr;
    for(int i=body->first(); body->more(i); i=body->next(i)) {
        curr_expr = body->nth(i);
    }
    return curr_expr->typeCheck(classtable);
}

Symbol let_class::typeCheck(ClassTable* classtable) {

    Symbol curr_class = classtable->getCurrentClass();
    //TODO: Compute T_0' using SELFTYPE on type_decl
    // Check that type_decl is defined.
    Symbol var_type = type_decl;
    if(var_type == SELF_TYPE){
        var_type = curr_class;
    }
    if(classtable->children.find(var_type) == classtable->children.end()) {
        ostream& err_stream = classtable->semant_error(classtable->symb_class_map[curr_class]);
        err_stream << "Let error: Declared type of variable: '" << type_decl->get_string()
            << "' is not defined." << endl;
        }

    // Check if there is initialization
    Symbol inferred_init_type = init->typeCheck(classtable);
    if(inferred_init_type != No_type){
        // Ensure initialization type conforms to the declared type of variable
        if(!classtable->isDescendantOf(var_type, inferred_init_type)){
            ostream& err_stream = classtable->semant_error(classtable->symb_class_map[curr_class]);
            err_stream << "Let Error: Initialization expression type: '" << inferred_init_type->get_string()
                << "' does not conform to declared type of variable: ' " << var_type->get_string() << "'" << endl;
        }
    }

    // Infer type of the body expression when the variable type is set to var_type
    classtable->objectST.enterscope();
    classtable->objectST.addid(identifier, &var_type);
    Symbol body_inferred_type = body->typeCheck(classtable);
    classtable->objectST.exitscope();

    return body_inferred_type;
}

Symbol plus_class::typeCheck(ClassTable* classtable) {
    Symbol curr_class = classtable->getCurrentClass();
    e1->addToScope(classtable);
    e2->addToScope(classtable);
    Symbol inferred_e1_type = e1->typeCheck(classtable);
    Symbol inferred_e2_type = e2->typeCheck(classtable);

    if(inferred_e1_type == Int && inferred_e2_type == Int) {
        return Int;
    }
    else{
        if(_DEBUG) printf("Expression plus_class error: Cannot add non-integer expressions \n");
        ostream& err_stream = classtable->semant_error(classtable->symb_class_map[curr_class]);
        err_stream << "Expression plus_class error: Cannot add non-integer expressions" << endl;
        return Object;
    }
}

Symbol sub_class::typeCheck(ClassTable* classtable) {
    Symbol curr_class = classtable->getCurrentClass();
    e1->addToScope(classtable);
    e2->addToScope(classtable);
    Symbol inferred_e1_type = e1->typeCheck(classtable);
    Symbol inferred_e2_type = e2->typeCheck(classtable);

    if(inferred_e1_type == Int && inferred_e2_type == Int) {
        return Int;
    }
    else{
        if(_DEBUG) printf("Expression sub_class error: Cannot subtract non-integer expressions \n");
        ostream& err_stream = classtable->semant_error(classtable->symb_class_map[curr_class]);
        err_stream << "Expression sub_class error: Cannot subtract non-integer expressions" << endl;
        return Object;
    }
}

Symbol mul_class::typeCheck(ClassTable* classtable) {
    Symbol curr_class = classtable->getCurrentClass();
    e1->addToScope(classtable);
    e2->addToScope(classtable);
    Symbol inferred_e1_type = e1->typeCheck(classtable);
    Symbol inferred_e2_type = e2->typeCheck(classtable);

    if(inferred_e1_type == Int && inferred_e2_type == Int) {
        return Int;
    }
    else{
        if(_DEBUG)printf("Expression mul_class error: Cannot multiply non-integer expressions \n");
        ostream& err_stream = classtable->semant_error(classtable->symb_class_map[curr_class]);
        err_stream << "Expression mul_class error: Cannot multiply non-integer expressions" << endl;
        return Object;
    }
}

Symbol divide_class::typeCheck(ClassTable* classtable) {
    Symbol curr_class = classtable->getCurrentClass();
    e1->addToScope(classtable);
    e2->addToScope(classtable);
    Symbol inferred_e1_type = e1->typeCheck(classtable);
    Symbol inferred_e2_type = e2->typeCheck(classtable);

    if(inferred_e1_type == Int && inferred_e2_type == Int) {
        return Int;
    }
    else{
        if(_DEBUG) printf("Expression divide_class error: Cannot divide non-integer expressions \n");
        ostream& err_stream = classtable->semant_error(classtable->symb_class_map[curr_class]);
        err_stream << "Expression divide_class error: Cannot divide non-integer expressions" << endl;
        return Object;
    }
}

Symbol neg_class::typeCheck(ClassTable* classtable) {
    Symbol curr_class = classtable->getCurrentClass();
    e1->addToScope(classtable);
    Symbol inferred_e1_type = e1->typeCheck(classtable);

    if(inferred_e1_type == Int) {
        return Int;
    }
    else{
        if(_DEBUG) printf("Expression neg_class error: Cannot negate a non-integer expression \n");
        ostream& err_stream = classtable->semant_error(classtable->symb_class_map[curr_class]);
        err_stream << "Expression neg_class error: Cannot negate a non-integer expression" << endl;
        return Object;
    }
}

Symbol lt_class::typeCheck(ClassTable* classtable) {
    Symbol curr_class = classtable->getCurrentClass();
    e1->addToScope(classtable);
    e2->addToScope(classtable);
    Symbol inferred_e1_type = e1->typeCheck(classtable);
    Symbol inferred_e2_type = e2->typeCheck(classtable);

    if(inferred_e1_type == Int && inferred_e2_type == Int) {
        return Bool;
    }
    else{
        if(_DEBUG) printf("Expression lt_class error: Cannot compare non-integer expressions \n");
        ostream& err_stream = classtable->semant_error(classtable->symb_class_map[curr_class]);
        err_stream << "Expression lt_class error: Cannot compare non-integer expressions" << endl;
        return Object;
    }
}

Symbol eq_class::typeCheck(ClassTable* classtable) {
    Symbol curr_class = classtable->getCurrentClass();
    e1->addToScope(classtable);
    e2->addToScope(classtable);
    Symbol inferred_e1_type = e1->typeCheck(classtable);
    Symbol inferred_e2_type = e2->typeCheck(classtable);

    // Implement "wrinkle" that if any expr is of type Int, String, or Bool then both must be.
    if(inferred_e1_type == Int || inferred_e1_type == Str || inferred_e1_type == Bool
            || inferred_e2_type == Int || inferred_e2_type == Str || inferred_e2_type == Bool){
        if(_DEBUG) printf("Expression eq_class error: if any expression is of type Int, String, or Bool then both must be.\n");
        ostream& err_stream = classtable->semant_error(classtable->symb_class_map[curr_class]);
        err_stream << "Expression eq_class error: if any expression is of type Int, String, or Bool then both must be." << endl;
    }
    return Bool;
}

Symbol leq_class::typeCheck(ClassTable* classtable) {
    Symbol curr_class = classtable->getCurrentClass();
    e1->addToScope(classtable);
    e2->addToScope(classtable);
    Symbol inferred_e1_type = e1->typeCheck(classtable);
    Symbol inferred_e2_type = e2->typeCheck(classtable);

    if(inferred_e1_type == Int && inferred_e2_type == Int) {
        return Bool;
    }
    else{
        if(_DEBUG) printf("Expression eq_class error: Cannot compare non-integer expressions \n");
        ostream& err_stream = classtable->semant_error(classtable->symb_class_map[curr_class]);
        err_stream << "Expression eq_class error: Cannot compare non-integer expressions" << endl;
        return Object;
    }
}

Symbol comp_class::typeCheck(ClassTable* classtable) {
    return e1->typeCheck(classtable);
}


Symbol int_const_class::typeCheck(ClassTable* classtable) {
    return Int;
}

Symbol bool_const_class::typeCheck(ClassTable* classtable) {
    return Bool;
}

Symbol string_const_class::typeCheck(ClassTable* classtable) {
    return Str;
}

Symbol new__class::typeCheck(ClassTable* classtable) {
    if(type_name==SELF_TYPE){
        return classtable->getCurrentClass();
    }
    else{
        return type_name;
    }
}

Symbol isvoid_class::typeCheck(ClassTable* classtable) {
    return Bool;
}

Symbol no_expr_class::typeCheck(ClassTable* classtable) {
    return No_type;
}

Symbol object_class::typeCheck(ClassTable* classtable) {
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
    _DEBUG = false;
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

