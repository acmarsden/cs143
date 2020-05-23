

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
    Class_ curr_class;
    for(int i=classes->first(); classes->more(i); i=classes->next(i)) {
        curr_class = classes->nth(i);
        if(_DEBUG) printf("%s: %s\n", curr_class->getName()->get_string(),
               curr_class->getParent()->get_string());
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

        // Now loop over features and collect method signatures
        if(_DEBUG) printf("Adding class %s \n", curr_class->getName()->get_string());
        addSignature(curr_class);
    }

    _has_cycle = has_cycle_bfs();

    // Each program must have a Main class defined
    if(_DEBUG){
        for(auto it = classMethods.begin(); it!=classMethods.end(); ++it){
            printf("key: %s\n", it->first->get_string());
        }
    }
    if(classMethods.find(Main) == classMethods.end()){
        ostream& err_stream = semant_error(curr_class);
        err_stream << "Any valid COOL program must have a 'Main' class."<<endl;
        return;
    }
    // The Main class must have a main method defined
    curr_class = symb_class_map[Main];
    if(classMethods[curr_class->getName()].find(main_meth) ==
       classMethods[curr_class->getName()].end()) {
        ostream& err_stream = semant_error(curr_class);
        err_stream << "The class 'Main' must have a 'main' method."<<endl;
        return;
    }
    // The main method takes no formal parameters
    std::vector<Symbol> main_signature = classMethods[Main][main_meth];
    if(main_signature.size() != 1) {
        ostream& err_stream = semant_error(curr_class);
        err_stream << "The 'Main.main' method must take no formal parameters."<<endl;
        return;
    }
}

void ClassTable::addSignature(Class_ class_){
    Features feature_list = class_->getFeatures();
    this->current_class = class_->getName();
    for(int i=feature_list->first(); feature_list->more(i); i=feature_list->next(i)) {
        Feature curr_feature = feature_list->nth(i);
        if(!curr_feature->isAttribute()){
            curr_feature->collectSignature(this);
        }
    }
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

    addSignature(Object_class);
    addSignature(IO_class);
    addSignature(Int_class);
    addSignature(Bool_class);
    addSignature(Str_class);
}

// Helper functions
bool ClassTable::isDescendantOf(Symbol parent, Symbol query_type) {
    // Starting at parent, recursively checks its children to see if they
    // contain query_type
    if(parent == SELF_TYPE){
        parent = getCurrentClass();
    }
    if(query_type == SELF_TYPE){
        query_type = getCurrentClass();
    }
    if(parent == query_type){
        return true;
    }else{
        std::vector<Symbol> curr_children = children[parent];
        bool is_descendant;
        for(auto it=curr_children.begin(); it!=curr_children.end(); ++it){
            Symbol child_type = *it;
            if(child_type==query_type){
                //query_type does indeed inherit from parent
                return true;
            }
            else{
                if(isDescendantOf(child_type, query_type)){
                    return true;
                }
            }
        }
        //Should return false if you have no children since we already checked that you weren't query_type
        return false;
    }
}

std::vector<Symbol> ClassTable::getSignature(Symbol class_name, Symbol method_name) {
    if(_DEBUG) printf("Called getSignature on class '%s'\n", class_name->get_string());
    std::vector<Symbol> curr_signature = classMethods[class_name][method_name];
    bool still_searching_for_method = true;
    while(still_searching_for_method){
        if(_DEBUG) printf("Searching for method '%s' in class %s\n", method_name->get_string(), class_name->get_string());
        if(_DEBUG) printf("Signature size found:  %lu\n", curr_signature.size());
        if(curr_signature.size()>0 || class_name == Object){
            still_searching_for_method = false;
        }
        else{
            class_name = symb_class_map[class_name]->getParent();
            curr_signature = classMethods[class_name][method_name];
        }
    }
    if(curr_signature.size()<1){
        ostream& err_stream = semant_error(symb_class_map[class_name]);
        err_stream << "getSignature Error: Class '" << class_name->get_string();
        err_stream << "' does not have method '" << method_name->get_string() << "'" <<  endl;
    }

    return curr_signature;
}

std::vector<Symbol> ClassTable::getSignature(Symbol method_name) {
    if(_DEBUG) printf("Called getSignature on current class\n");
    return getSignature(getCurrentClass(), method_name);
}

Symbol ClassTable::compute_join(std::vector<Symbol> symbol_vec) {
//Sequentially compute the least type C such that C_old \leq C and Type_i \leq C
//Set C_old to be C
    if(_DEBUG) printf("Computing a join on several Symbols\n");
    Symbol curr_class = getCurrentClass();
    std::set<Symbol> seen_symbols;
    Symbol join = symbol_vec[0];

    seen_symbols.insert(join);
    for(uint i=1; i<symbol_vec.size(); ++i){
        if(seen_symbols.find(symbol_vec[i])==seen_symbols.end()){
            // Make sure symbol_vec[i] is known to us
            if(symbol_vec[i] != SELF_TYPE && children.find(symbol_vec[i]) == children.end()) {
                if(_DEBUG) printf("'compute_join' error: '%s' is not defined\n", symbol_vec[i]->get_string());
                ostream& err_stream = semant_error(symb_class_map[curr_class]);
                err_stream << "'compute_join' error: '" << symbol_vec[i]->get_string() << "'' is not defined" << endl;
            }
            //This symbol has NOT already been included in join so we need to update join.
            join = compute_join_pair(join, symbol_vec[i]);
            seen_symbols.insert(symbol_vec[i]);
        }
    }
    return join;
}

Symbol ClassTable::compute_join_pair(Symbol symbolA, Symbol symbolB) {
    if(_DEBUG) printf("Computing a join on pair %s, %s\n", symbolA->get_string(), symbolB->get_string());
    if(symbolA == symbolB) return symbolA; // also applies to when they are both self type
    if(symbolA == SELF_TYPE) symbolA = current_class;
    if(symbolB == SELF_TYPE) symbolB = current_class; //As per the manual

    //Increment A's ancestors one by one and check against all of B's ancestors
    //First get B's ancestors
    std::set<Symbol> b_ancestors;
    Symbol curr_symbolB = symbolB;
    bool found_all_ancestors = false;
    while(!found_all_ancestors){
        if(curr_symbolB == Object) found_all_ancestors = true;
        b_ancestors.insert(curr_symbolB);
        curr_symbolB = symb_class_map[curr_symbolB]->getParent();
        if(_DEBUG) printf("JOIN: adding %s to ancestors list\n", curr_symbolB->get_string());
    }
    if(_DEBUG) printf("JOIN: Finished gathering all ancestors of type %s \n", symbolB->get_string());
    Symbol curr_symbolA = symbolA;
    bool found_join = false;
    Symbol join_pair = Object;
    while(!found_join){
        if(_DEBUG) printf("JOIN: checking %s against ancestors list\n", curr_symbolA->get_string());
        if(b_ancestors.find(curr_symbolA)!=b_ancestors.end()){
            //Found join
            join_pair = curr_symbolA;
            found_join = true;
        }
        curr_symbolA =  symb_class_map[curr_symbolA]->getParent();
    }
    if(_DEBUG) printf("Join resolved to type %s\n", join_pair->get_string());
    return join_pair;
}

// Semantic analysis entry point
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
    methodST.exitscope();
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
                if(_DEBUG) printf("Dumping objectST\n");
                if(_DEBUG) objectST.dump();
                objectST.exitscope();
                methodST.exitscope();
            }
        }
    }
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

void method_class::collectSignature(ClassTable* classtable) {
    Symbol curr_class = classtable->getCurrentClass();
    std::vector<Symbol> data;

    //if(return_type == SELF_TYPE) {
    //     data.push_back(classtable->getCurrentClass());
    //}else{
    //    data.push_back(return_type);
    //}
    data.push_back(return_type);
    for(int i=formals->first(); formals->more(i); i=formals->next(i)) {
        Symbol formal_type = formals->nth(i)->getType();
        if(formal_type == SELF_TYPE) {
            ostream& err_stream = classtable->semant_error(classtable->symb_class_map[curr_class]);
            err_stream << "Formal '" << formals->nth(i)->getName()->get_string();
             err_stream << "' was declared to be SELF_TYPE which is not allowed" << endl;
        }
        data.push_back(formal_type);
    }

    // and add the method signature to this scope-independent data structure
    if(_DEBUG) printf("Added a signature of size %lu to %s.%s\n", data.size(), curr_class->get_string(), name->get_string());
    classtable->classMethods[curr_class][name] = data;
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
            //OLD
            //if(type_decl == SELF_TYPE) {
            //    classtable->objectST.addid(name, &curr_class);
            //}else{
            //    classtable->objectST.addid(name, &type_decl);
            classtable->objectST.addid(name, &type_decl);
        }

    }
}

void method_class::addToScope(ClassTable* classtable) {
    Symbol curr_class = classtable->getCurrentClass();
    std::vector<Symbol> data;

    //if(return_type == SELF_TYPE) {
   //     data.push_back(classtable->getCurrentClass());
    //}else{
    //    data.push_back(return_type);
    //}
    data.push_back(return_type);
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
    Symbol* lookup = classtable->methodST.lookup(name);
    if(lookup != NULL){
        // If it did find a match, the defintions must conform
        bool matches = true;
        //Now we need to get the rest of the signature
        std::vector<Symbol> old_signature = classtable->getSignature(name);
        for(uint i=0; i<data.size(); ++i){
            if(data[i] != old_signature[i]){
                matches = false;
            }
        }
        if(!matches){
            ostream& err_stream = classtable->semant_error(classtable->symb_class_map[curr_class]);
            err_stream << "Method formals list or return type does not conform to parent definition" << endl;
            err_stream << "Method is '" << name->get_string() << "'. Class calling it is '" << curr_class->get_string();
            err_stream << "'. Old signature starts with '" << old_signature[0]->get_string();
            err_stream << "'. Return type is '" << return_type->get_string() << endl;
        }
    }
    // If it did not find a match, we are OK, since it is the first time it is
    // defined in the class hierarchy

    // Now add it to the present scope
    classtable->methodST.addid(name, &(data[0]));
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
        if(declared_type == SELF_TYPE) declared_type = curr_class;
        if(declared_type!=inferred_init_type){
            bool inheritance_found = classtable->isDescendantOf(declared_type, inferred_init_type);
            if(!inheritance_found){
                if(_DEBUG) printf("Attribute init error: Assignment expression type does not conform to declared Id type.\n");
                ostream& err_stream = classtable->semant_error(classtable->symb_class_map[curr_class]);
                err_stream << "Attribute init error: Assignment expression type '" << inferred_init_type;
                err_stream << " does not conform to declared Id type '" << declared_type <<"'." << endl;
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
    std::vector<Symbol> signature;

    // Check the method exists
    Symbol* lookup = classtable->methodST.lookup(name);
    if(lookup != NULL){
        signature = classtable->getSignature(name);
        declared_return_type = signature[0];
    }else{
        if(_DEBUG) printf("Dump of Method Symbol Table: \n");
        if(_DEBUG) classtable->methodST.dump();
        ostream& err_stream = classtable->semant_error(classtable->symb_class_map[curr_class]);
        err_stream << "Error: Did not find method '"<< name->get_string();
        err_stream << "' in the current scope." << endl;
        err_stream << "The class calling is '" << curr_class->get_string() << endl;
        return Object;
    }

    // Enter a new scope, add formals to it, recurse on the body and exit scope
    classtable->objectST.enterscope();
    int j = 1;
    for(int i=formals->first(); formals->more(i); i=formals->next(i)) {
        Symbol formal_name = formals->nth(i)->getName();
        Symbol formal_declared_type = signature[j];

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
        if(_DEBUG) printf("Adding to scope name %s of type %s \n", formal_name->get_string(), formal_declared_type->get_string());
        classtable->objectST.addid(formal_name, &(signature[j]));
        ++j;
    }

    Symbol inferred_return_type = expr->typeCheck(classtable);
    classtable->objectST.exitscope();

    // New idea for SELF_TYPE:
    //if(declared_return_type == SELF_TYPE){
    //    declared_return_type = curr_class;
    //}
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
        return type_decl;
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
    if(_DEBUG) printf("Type checking an assignment \n");
    Symbol curr_class = classtable->getCurrentClass();
    // First get O(Id), i.e. the type the environment gives to the id
    if(_DEBUG) printf("looking for %s in scope \n", name->get_string());
    if(_DEBUG) classtable->objectST.dump();
    Symbol* declared_type = classtable->objectST.lookup(name);
    Symbol declared_type_deref = *declared_type;
    if(_DEBUG) printf("The declared type is '%s' \n", declared_type_deref->get_string());
    // Now get the type of the expression
    Symbol inferred_assign_type = expr->typeCheck(classtable);
    // Check that the type of the expression conforms to declared_type
    // i.e. that inferred_assign_type inherits from declared_type

    if(!classtable->isDescendantOf(*declared_type, inferred_assign_type)) {
            if(_DEBUG) printf("Assign error: Expression type does not conform to Id type.\n");
            ostream& err_stream = classtable->semant_error(classtable->symb_class_map[curr_class]);
            err_stream << "Assign error: Type '" << inferred_assign_type->get_string();
            err_stream << "' of assigned expression  does not conform to declared type '";
            err_stream << (*declared_type)->get_string() << "' of identifier '" << name->get_string();
            err_stream << "'." << endl;
            set_type(Object);
            return get_type();
    }
    else{
        set_type(inferred_assign_type);
        return get_type();
    }
}

Symbol static_dispatch_class::typeCheck(ClassTable* classtable) {
    Symbol curr_class = classtable->getCurrentClass();

    // First get the type for the base expression e_0
    Symbol inferred_calling_expr_type = expr->typeCheck(classtable);

    // Before getting signature resolve the type of the inferred_calling_expr_type
    if(inferred_calling_expr_type == SELF_TYPE) inferred_calling_expr_type = curr_class;

    // Check that this type conforms with what is declared
    // This part is what makes it STATIC DISPATCH
    if(!classtable->isDescendantOf(type_name, inferred_calling_expr_type)){
        ostream& err_stream = classtable->semant_error(classtable->symb_class_map[curr_class]);
        err_stream << "Static Dispatch Error: Expression type '" << inferred_calling_expr_type->get_string();
        err_stream << "' does not conform to type name '" << type_name->get_string() << "' for method '";
        err_stream << name->get_string() << "'" << endl;
    }

    // check the inferred_calling_expr_type exists
    if(classtable->children.find(inferred_calling_expr_type) == classtable->children.end()){
        ostream& err_stream = classtable->semant_error(classtable->symb_class_map[curr_class]);
        err_stream << "Statis cispatch Error: When trying to dispatch method '" << name->get_string();
        err_stream << "' from an expression of type '";
        err_stream << inferred_calling_expr_type->get_string();
        err_stream << "' found the type does not exist." << endl;
        set_type(Object);
        return Object;
    }else{
        // Check that method with "name" is implemented as a method of some parent of the declared type_name
        std::vector<Symbol> curr_signature = classtable->getSignature(type_name, name);
        // Loop through the expressions and get their inferred return types
        int j = 1;
        for(int i=actual->first(); actual->more(i); i=actual->next(i)) {
            Expression curr_expr = actual->nth(i);
            Symbol inferred_body_expr_type = curr_expr->typeCheck(classtable);
            // Check that this type inherits from the type given in the method declaration
            // Otherwise return an error
            //SELF_TYPE with dispatch: If the body of the method has SELF_TYPE its type should be the class calling the method, not the current class
            if(inferred_body_expr_type == SELF_TYPE) inferred_body_expr_type = inferred_calling_expr_type;
            if(!classtable->isDescendantOf(curr_signature[j], inferred_body_expr_type)){
                if(_DEBUG) printf("Static Dispatch Error: Declared type's method implementation does not have associated formal type for method.\n");
                ostream& err_stream = classtable->semant_error(classtable->symb_class_map[curr_class]);
                err_stream << "Static Dispatch Error: Declared type '" << type_name->get_string();
                err_stream << "' method implementation does not match formals type list. " << endl;
            }
            ++j;
        }
        // Return the return type of the method. This is key part where we need to implement SELF_TYPE
        // If curr_signature[0] == SELF_TYPE then we return inferred_calling_expr_type
        if(curr_signature[0] == SELF_TYPE) {
            if(_DEBUG) printf("Dispatch type SELF_TYPE resolved to: '%s'\n", inferred_calling_expr_type->get_string());
            set_type(inferred_calling_expr_type);
        }else{
            set_type(curr_signature[0]);
        }
        return get_type();
    }
}


Symbol dispatch_class::typeCheck(ClassTable* classtable) {
    Symbol curr_class = classtable->getCurrentClass();
    // First get the type for the base expression e_0
    Symbol inferred_calling_expr_type = expr->typeCheck(classtable);
    if(_DEBUG) printf("Method Dispatch: In class '%s' an id that resolved to type '%s' is trying to call a method\n",
            curr_class->get_string(), inferred_calling_expr_type->get_string());

    // Check that method with "name" is implemented as a method of some parent of the expression type
    Symbol curr_type = inferred_calling_expr_type;

    // Before getting signature resolve the type of the inferred_calling_expr_type
    if(inferred_calling_expr_type == SELF_TYPE) inferred_calling_expr_type = curr_class;

    // check the inferred_calling_expr_type exists
    if(classtable->children.find(inferred_calling_expr_type) == classtable->children.end()){
        ostream& err_stream = classtable->semant_error(classtable->symb_class_map[curr_class]);
        err_stream << "Dispatch Error: When trying to dispatch method '" << name->get_string();
        err_stream << "' from an expression of type '";
        err_stream << inferred_calling_expr_type->get_string();
        err_stream << "' found the type does not exist." << endl;
        set_type(Object);
        return Object;
    }else{
        std::vector<Symbol> curr_signature = classtable->getSignature(inferred_calling_expr_type, name);
        if(_DEBUG){
            for(uint i = 0; i<curr_signature.size(); i++){
                printf("Curr Sign: '%s'\n", curr_signature[i]->get_string());
            }
        }
        // Loop through the expressions and get their inferred return types
        int j = 1;
        for(int i=actual->first(); actual->more(i); i=actual->next(i)) {
            Expression curr_expr = actual->nth(i);
            Symbol inferred_body_expr_type = curr_expr->typeCheck(classtable);
            //if(curr_expr == SELF_TYPE){
            //    printf("FOUND SELF!!!!");
            //    inferred_boyd_expr_type = curr_class;
            //}
            //SELF_TYPE with dispatch: If the body of the method has SELF_TYPE its type should be the class calling the method, not the current class
            if(inferred_body_expr_type == SELF_TYPE) {
                if(_DEBUG) printf("The method has an expression of type SELF_TYPE, which is being replaced with type %s\n", inferred_body_expr_type->get_string());
                //inferred_body_expr_type = curr_class; }
                //inferred_body_expr_type = inferred_calling_expr_type;}
                inferred_body_expr_type = curr_class;}
            // Check that the number of actuals is not more than the number of parameters
            if(j == curr_signature.size()){
                ostream& err_stream = classtable->semant_error(classtable->symb_class_map[curr_class]);
                err_stream << "Dispatch Error: The actual number of parameters supplied exceeds ";
                err_stream << "those the method was defined with" << endl;
                break;
            }
            // Check that this type inherits from the type given in the method declaration
            // Otherwise return an error
            if(!classtable->isDescendantOf(curr_signature[j], inferred_body_expr_type)){
                if(_DEBUG) printf("curr_signature: %s \n inferred_curr_expr_type: %s \n", curr_signature[j]->get_string(), inferred_body_expr_type->get_string());
                ostream& err_stream = classtable->semant_error(classtable->symb_class_map[curr_class]);
                err_stream << "Dispatch Error: The formal types listed in dispatch call for expression of type '";
                err_stream << inferred_calling_expr_type->get_string();
                err_stream << "' do not match the formal types declared for method implementation." << endl;
            }
            ++j;
        }
        if(j < curr_signature.size()){
            ostream& err_stream = classtable->semant_error(classtable->symb_class_map[curr_class]);
            err_stream << "Dispatch Error: The actual number of parameters supplied is less than ";
            err_stream << "those the method was defined with" << endl;
        }
        // Return the return type of the method. This is key part where we need to implement SELF_TYPE
        // If curr_signature[0] == SELF_TYPE then we return inferred_calling_expr_type
        if(curr_signature[0] == SELF_TYPE && inferred_calling_expr_type!=curr_class) {
            if(_DEBUG) printf("Dispatch type SELF_TYPE resolved to: '%s'\n", inferred_calling_expr_type->get_string());
            set_type(inferred_calling_expr_type);
        }else{
            set_type(curr_signature[0]);
        }

        return get_type();
    }
}

Symbol cond_class::typeCheck(ClassTable* classtable) {
    Symbol curr_class = classtable->getCurrentClass();
    if(_DEBUG) printf("Type chcecking an if statement\n");
    Symbol pred_type = pred->typeCheck(classtable);
    if(_DEBUG) printf("IF: if resolved to type %s \n", pred_type->get_string());
    Symbol then_exp_type = then_exp->typeCheck(classtable);
    if(_DEBUG) printf("IF: then resolved to type %s \n", then_exp_type->get_string());
    Symbol else_exp_type = else_exp->typeCheck(classtable);
    if(_DEBUG) printf("IF: else resolved to type %s \n", else_exp_type->get_string());
    Symbol return_type = Object;
    if(pred_type!=Bool){
        if(_DEBUG) printf("If-Statement Error: the predicate is not of type Bool.\n");
    }
    else {
        //Compute join of then_exp_type and else_exp_type
        std::vector<Symbol> symbol_vec;
        //if(then_exp_type == SELF_TYPE) then_exp_type = curr_class;
        symbol_vec.push_back(then_exp_type);
        //if(else_exp_type == SELF_TYPE) else_exp_type = curr_class;
        symbol_vec.push_back(else_exp_type);
        if(_DEBUG) printf("IF: computing join of %s and %s\n", then_exp_type->get_string(), else_exp_type->get_string());
        return_type = classtable->compute_join(symbol_vec);
        if(_DEBUG) printf("IF: join resolved to type %s.\n", return_type->get_string());
    }
    set_type(return_type);
    return get_type();
}

Symbol loop_class::typeCheck(ClassTable* classtable) {
    Symbol pred_type = pred->typeCheck(classtable);
    if(pred_type!=Bool){
        if(_DEBUG) printf("Loop Error: the predicate is not of type Bool. \n");
        Symbol curr_class = classtable->getCurrentClass();
        ostream& err_stream = classtable->semant_error(classtable->symb_class_map[curr_class]);
        err_stream << "Loop Error: the predicate type '" << pred_type->get_string() <<  "' is not of type Bool." << endl;
    }
    body->typeCheck(classtable);
    set_type(Object);
    return get_type();
}

Symbol typcase_class::typeCheck(ClassTable* classtable) {
    Symbol curr_class = classtable->getCurrentClass();
    expr->typeCheck(classtable);
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
    set_type(return_type);
    return get_type();
}

Symbol block_class::typeCheck(ClassTable* classtable) {
    if(_DEBUG) printf("Type checking a block\n");
    Expression curr_expr;
    for(int i=body->first(); body->more(i); i=body->next(i)) {
        curr_expr = body->nth(i);
        //Though we don't use we do need to typecheck
        if(_DEBUG) printf("Type checking first expression in block\n");
        Symbol curr_type = curr_expr->typeCheck(classtable);
    }
    if(_DEBUG) printf("Type checking the last expr in  block\n");
    Symbol inferred_type = curr_expr->typeCheck(classtable);
    if(_DEBUG) printf("Block resolved to type %s\n", inferred_type->get_string());
    set_type(inferred_type);
    if(_DEBUG) printf("Type was set to %s\n", get_type()->get_string());
    return get_type();
}

Symbol let_class::typeCheck(ClassTable* classtable) {

    Symbol curr_class = classtable->getCurrentClass();
    // Check that type_decl is defined.
    Symbol var_type = type_decl;
    if(var_type == SELF_TYPE){
        var_type = curr_class;
    }
    if(classtable->children.find(var_type) == classtable->children.end()) {
        ostream& err_stream = classtable->semant_error(classtable->symb_class_map[curr_class]);
        err_stream << "Let error: Declared type of variable: '" << type_decl->get_string();
        err_stream << "' is not defined." << endl;
    }

    // Check if there is initialization
    Symbol inferred_init_type = init->typeCheck(classtable);
    if(inferred_init_type != No_type){
        // Ensure initialization type conforms to the declared type of variable
        if(!classtable->isDescendantOf(var_type, inferred_init_type)){
            ostream& err_stream = classtable->semant_error(classtable->symb_class_map[curr_class]);
            err_stream << "Let Error: Initialization expression type: '" << inferred_init_type->get_string();
            err_stream << "' does not conform to declared type of variable: ' " << var_type->get_string() << "'" << endl;
        }
    }

    // Infer type of the body expression when the variable type is set to var_type
    classtable->objectST.enterscope();
    classtable->objectST.addid(identifier, &var_type);
    Symbol body_inferred_type = body->typeCheck(classtable);
    classtable->objectST.exitscope();

    set_type(body_inferred_type);
    return get_type();
}

Symbol plus_class::typeCheck(ClassTable* classtable) {
    Symbol curr_class = classtable->getCurrentClass();
    e1->addToScope(classtable);
    e2->addToScope(classtable);
    Symbol inferred_e1_type = e1->typeCheck(classtable);
    Symbol inferred_e2_type = e2->typeCheck(classtable);

    if(inferred_e1_type == Int && inferred_e2_type == Int) {
        set_type(Int);
        return get_type();
    }
    else{
        if(_DEBUG) printf("Expression plus_class error: Cannot add non-integer expressions \n");
        ostream& err_stream = classtable->semant_error(classtable->symb_class_map[curr_class]);
        err_stream << "Expression plus_class error: Cannot add non-integer expressions" << endl;
        set_type(Object);
        return get_type();
    }
}

Symbol sub_class::typeCheck(ClassTable* classtable) {
    Symbol curr_class = classtable->getCurrentClass();
    e1->addToScope(classtable);
    e2->addToScope(classtable);
    Symbol inferred_e1_type = e1->typeCheck(classtable);
    Symbol inferred_e2_type = e2->typeCheck(classtable);

    if(inferred_e1_type == Int && inferred_e2_type == Int) {
        set_type(Int);
        return get_type();
    }
    else{
        if(_DEBUG) printf("Expression sub_class error: Cannot subtract non-integer expressions \n");
        ostream& err_stream = classtable->semant_error(classtable->symb_class_map[curr_class]);
        err_stream << "Expression sub_class error: Cannot subtract non-integer expressions" << endl;
        set_type(Object);
        return get_type();
    }
}

Symbol mul_class::typeCheck(ClassTable* classtable) {
    Symbol curr_class = classtable->getCurrentClass();
    e1->addToScope(classtable);
    e2->addToScope(classtable);
    Symbol inferred_e1_type = e1->typeCheck(classtable);
    Symbol inferred_e2_type = e2->typeCheck(classtable);

    if(inferred_e1_type == Int && inferred_e2_type == Int) {
        set_type(Int);
        return get_type();
    }
    else{
        if(_DEBUG)printf("Expression mul_class error: Cannot multiply non-integer expressions \n");
        ostream& err_stream = classtable->semant_error(classtable->symb_class_map[curr_class]);
        err_stream << "Expression mul_class error: Cannot multiply non-integer expressions" << endl;
        set_type(Object);
        return get_type();
    }
}

Symbol divide_class::typeCheck(ClassTable* classtable) {
    Symbol curr_class = classtable->getCurrentClass();
    e1->addToScope(classtable);
    e2->addToScope(classtable);
    Symbol inferred_e1_type = e1->typeCheck(classtable);
    Symbol inferred_e2_type = e2->typeCheck(classtable);

    if(inferred_e1_type == Int && inferred_e2_type == Int) {
        set_type(Int);
        return get_type();
    }
    else{
        if(_DEBUG) printf("Expression divide_class error: Cannot divide non-integer expressions \n");
        ostream& err_stream = classtable->semant_error(classtable->symb_class_map[curr_class]);
        err_stream << "Expression divide_class error: Cannot divide non-integer expressions" << endl;
        set_type(Object);
        return get_type();
    }
}

Symbol neg_class::typeCheck(ClassTable* classtable) {
    Symbol curr_class = classtable->getCurrentClass();
    e1->addToScope(classtable);
    Symbol inferred_e1_type = e1->typeCheck(classtable);

    if(inferred_e1_type == Int) {
        set_type(Int);
    }
    else{
        if(_DEBUG) printf("Expression neg_class error: Cannot negate a non-integer expression \n");
        ostream& err_stream = classtable->semant_error(classtable->symb_class_map[curr_class]);
        err_stream << "Expression neg_class error: Cannot negate a non-integer expression" << endl;
        set_type(Object);
        return get_type();
    }
    return get_type();
}

Symbol lt_class::typeCheck(ClassTable* classtable) {
    Symbol curr_class = classtable->getCurrentClass();
    e1->addToScope(classtable);
    e2->addToScope(classtable);
    Symbol inferred_e1_type = e1->typeCheck(classtable);
    Symbol inferred_e2_type = e2->typeCheck(classtable);

    if(inferred_e1_type == Int && inferred_e2_type == Int) {
        set_type(Bool);
    }
    else{
        if(_DEBUG) printf("Expression lt_class error: Cannot compare non-integer expressions \n");
        ostream& err_stream = classtable->semant_error(classtable->symb_class_map[curr_class]);
        err_stream << "Expression lt_class error: Cannot compare non-integer expressions" << endl;
        set_type(Object);
        return get_type();
    }
    return get_type();
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
        if(inferred_e1_type != inferred_e2_type){
            if(_DEBUG) printf("Expression eq_class error: if any expression is of type Int, String, or Bool then both must be.\n");
            if(_DEBUG) printf("The type of e1 is '%s' and the type of e2 is '%s'\n", inferred_e1_type->get_string(), inferred_e2_type->get_string());
            ostream& err_stream = classtable->semant_error(classtable->symb_class_map[curr_class]);
            err_stream << "Expression eq_class error: if any expression is of type Int, String, or Bool then both must be." << endl;
        }
    }
    set_type(Bool);
    return get_type();
}

Symbol leq_class::typeCheck(ClassTable* classtable) {
    Symbol curr_class = classtable->getCurrentClass();
    e1->addToScope(classtable);
    e2->addToScope(classtable);
    Symbol inferred_e1_type = e1->typeCheck(classtable);
    Symbol inferred_e2_type = e2->typeCheck(classtable);

    if(inferred_e1_type == Int && inferred_e2_type == Int) {
        set_type(Bool);
    }
    else{
        if(_DEBUG) printf("Expression leq_class error: Cannot compare non-integer expressions \n");
        ostream& err_stream = classtable->semant_error(classtable->symb_class_map[curr_class]);
        err_stream << "Expression leq_class error: Cannot compare non-integer expressions" << endl;
        set_type(Object);
        return get_type();
    }
    return get_type();
}

Symbol comp_class::typeCheck(ClassTable* classtable) {
    set_type(e1->typeCheck(classtable));
    return get_type();
}


Symbol int_const_class::typeCheck(ClassTable* classtable) {
    set_type(Int);
    return get_type();
}

Symbol bool_const_class::typeCheck(ClassTable* classtable) {
    set_type(Bool);
    return get_type();
}

Symbol string_const_class::typeCheck(ClassTable* classtable) {
    set_type(Str);
    return get_type();
}

Symbol new__class::typeCheck(ClassTable* classtable) {
    if(type_name==SELF_TYPE){
        set_type(classtable->getCurrentClass());
    }
    else{
        set_type(type_name);
    }
    return get_type();
}

Symbol isvoid_class::typeCheck(ClassTable* classtable) {
    e1->typeCheck(classtable);
    set_type(Bool);
    return get_type();
}

Symbol no_expr_class::typeCheck(ClassTable* classtable) {
    set_type(No_type);
    return get_type();
}

Symbol object_class::typeCheck(ClassTable* classtable) {
    if(_DEBUG) printf("Type checking a single object id: %s\n", name->get_string());
    Symbol curr_class = classtable->getCurrentClass();
    if(name == self){
        if(_DEBUG) printf("Resolving self to be: %s\n", curr_class->get_string());
        set_type(SELF_TYPE);
        return get_type();
    }
    if(_DEBUG) printf("Looking for %s in all the symbol table\n", name->get_string());
    if(_DEBUG) classtable->objectST.dump();
    Symbol* lookup = classtable->objectST.lookup(name);
    if(lookup != NULL){
        if(_DEBUG) printf("The type of %s in the ST is %s\n", name->get_string(), (*lookup)->get_string());
        set_type(*lookup);
        return get_type();
    }else{
        set_type(Object);
        return get_type();
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

