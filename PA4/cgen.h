#include <assert.h>
#include <stdio.h>
#include "emit.h"
#include "cool-tree.h"
#include "cool-tree.handcode.h"
#include "symtab.h"
#include <set>
#include <map>
#include <vector>
#include <utility> // for std::pair

enum Basicness     {Basic, NotBasic};
#define TRUE 1
#define FALSE 0

//class CgenClassTable;
//typedef CgenClassTable *CgenClassTableP;

class CgenNode;
typedef CgenNode *CgenNodeP;
typedef SymbolTable<Symbol, std::pair<char*, int > > Scopetable;

class CgenClassTable : public SymbolTable<Symbol,CgenNode> {
private:
   List<CgenNode> *nds;
   ostream& str;
   int stringclasstag;
   int intclasstag;
   int boolclasstag;
   CgenNodeP current_node;

   // The following methods emit code for
   // constants and global declarations.
   void code_global_data();
   void code_global_text();
   void code_bools(int);
   void code_select_gc();
   void code_constants();

   // There are the student-implemented methods to emit code for the
   // rest of the compiler
   void code_class_nameTab(CgenNodeP);
   void code_class_objTab(CgenNodeP);
   void code_class_parentTab(CgenNodeP);
   void code_dispatch_tables(CgenNodeP,
                             std::map<Symbol, std::vector<Symbol> >*,
                             std::vector<Symbol>*);
   void code_prototypes(CgenNodeP, std::vector<std::pair<Symbol, Symbol> >*);
   void code_prototype(CgenNodeP, std::vector<std::pair<Symbol, Symbol> >*);
   void code_object_initializers(CgenNodeP, int*);
   void code_object_initializer(CgenNodeP, int*);
   void code_all_class_methods(CgenNodeP, int*);
   void code_class_methods(CgenNodeP, int*);

   // The following creates an inheritance graph from
   // a list of classes.  The graph is implemented as
   // a tree of `CgenNode', and class names are placed
   // in the base class symbol table.
   void build_classtag_map(CgenNodeP, int*, std::vector<int>*);
   void install_basic_classes();
   void install_class(CgenNodeP nd);
   void install_classes(Classes cs);
   void build_inheritance_tree();
   void set_relations(CgenNodeP nd);
public:
   CgenClassTable(Classes, ostream& str);
   void code();
   CgenNodeP root();
   CgenNodeP getCurrentNode(){return current_node;}
   std::map<Symbol, std::vector<Symbol> > dispatch_table;
   std::map<Symbol, int> classtag_map;
   std::map<Symbol, std::set<int> > classtag_ancestor_map;
   Scopetable objectST;
};


class CgenNode : public class__class {
private:
   CgenNodeP parentnd;                        // Parent of class
   List<CgenNode> *children;                  // Children of class
   Basicness basic_status;                    // `Basic' if class is basic
                                              // `NotBasic' otherwise

public:
   CgenNode(Class_ c,
            Basicness bstatus,
            CgenClassTableP class_table);

   void add_child(CgenNodeP child);
   List<CgenNode> *get_children() { return children; }
   void set_parentnd(CgenNodeP p);
   CgenNodeP get_parentnd() { return parentnd; }
   int basic() { return (basic_status == Basic); }
};

class BoolConst
{
 private:
  int val;
 public:
  BoolConst(int);
  void code_def(ostream&, int boolclasstag);
  void code_ref(ostream&) const;
};
