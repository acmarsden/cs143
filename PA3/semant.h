#ifndef SEMANT_H_
#define SEMANT_H_

#include <assert.h>
#include <iostream>
#include "cool-tree.h"
#include "stringtab.h"
#include "symtab.h"
#include "list.h"
#include <map>
#include <vector>
#include <set>

#define TRUE 1
#define FALSE 0

//class ClassTable;
//typedef ClassTable *ClassTableP;

// This is a structure that may be used to contain the semantic
// information such as the inheritance graph.  You may use it or not as
// you like: it is only here to provide a container for the supplied
// methods.

class ClassTable {
private:
    int semant_errors;
    bool _has_cycle;
    Symbol current_class;
    ostream& error_stream;
    void install_basic_classes();
    bool has_cycle_bfs();
    void run_type_checks_r(Symbol, std::set<Symbol>*);
    void check_features(Symbol);
public:
    ClassTable(Classes);
    SymbolTable<Symbol, Symbol> objectST;
    SymbolTable<Symbol, Symbol > methodST;
    std::map<Symbol, std::vector<Symbol> > children;
    std::map<Symbol, std::map<Symbol, std::vector<Symbol> > > classMethods;
    std::vector<Symbol> getSignature(Symbol);
    std::vector<Symbol> getSignature(Symbol, Symbol);
    std::map<Symbol, Class_> symb_class_map;
    void run_type_checks();
    bool has_cycle() {return _has_cycle;}
    int errors() { return semant_errors; }
    Symbol getCurrentClass(){return current_class;}
    Symbol compute_join(std::vector<Symbol>);
    Symbol compute_join_pair(Symbol, Symbol);
    bool isDescendantOf(Symbol, Symbol);
    ostream& semant_error();
    ostream& semant_error(Class_ c);
    ostream& semant_error(Symbol filename, tree_node *t);
};

bool _DEBUG;

#endif
