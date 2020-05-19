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
  ostream& error_stream;
  void install_basic_classes();
  bool has_cycle_bfs();
  void run_type_checks_r(Symbol, std::set<Symbol>*, ClassTable*);
  void check_features(Symbol, ClassTable*);
public:
  ClassTable(Classes);
  void run_type_checks(ClassTable*);
  std::map<Symbol, std::vector<Symbol> > children;
  std::map<Symbol, Class_> symb_class_map;
  bool has_cycle() {return _has_cycle;}
  SymbolTable<Symbol, Symbol> class_symbol_table;
  int errors() { return semant_errors; }
  ostream& semant_error();
  ostream& semant_error(Class_ c);
  ostream& semant_error(Symbol filename, tree_node *t);
};


#endif

