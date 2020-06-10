
//**************************************************************
//
// Code generator SKELETON
//
// Read the comments carefully. Make sure to
//    initialize the base class tags in
//       `CgenClassTable::CgenClassTable'
//
//    Add the label for the dispatch tables to
//       `IntEntry::code_def'
//       `StringEntry::code_def'
//       `BoolConst::code_def'
//
//    Add code to emit everyting else that is needed
//       in `CgenClassTable::code'
//
//
// The files as provided will produce code to begin the code
// segments, declare globals, and emit constants.  You must
// fill in the rest.
//
//**************************************************************

#include "cgen.h"
#include "cgen_gc.h"
#include <memory>

extern void emit_string_constant(ostream& str, char *s);
extern int cgen_debug;

//
// Three symbols from the semantic analyzer (semant.cc) are used.
// If e : No_type, then no code is generated for e.
// Special code is generated for new SELF_TYPE.
// The name "self" also generates code different from other references.
//
//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
Symbol
             arg,
             arg2,
             Bool,
             concat,
             cool_abort,
             copy,
             _copy,
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

typedef std::unique_ptr<std::pair<char*, int > > PairPointer ;
std::vector<PairPointer> pair_container;

static void dump_pair_container(){
    cout << "# pair_container: [";
    for(auto it=pair_container.cbegin(); it!=pair_container.cend(); ++it){
        cout << "(" << (**it).first << ", " << (**it).second;
        cout << ")";
    }
    cout << "]\n";
}
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
    _copy       = idtable.add_string("copy");
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

static char *gc_init_names[] =
    { "_NoGC_Init", "_GenGC_Init", "_ScnGC_Init" };
static char *gc_collect_names[] =
    { "_NoGC_Collect", "_GenGC_Collect", "_ScnGC_Collect" };

// A global label counter to generate labels on the fly for branching code
static int GLOBAL_LABEL_CTR = 0;
static int GLOBAL_FP_OFF = 0;
static std::vector<int> FP_OFF_SCOPE;

//  BoolConst is a class that implements code generation for operations
//  on the two booleans, which are given global names here.
BoolConst falsebool(FALSE);
BoolConst truebool(TRUE);

//*********************************************************
//
// Define method for code generation
//
// This is the method called by the compiler driver
// `cgtest.cc'. cgen takes an `ostream' to which the assembly will be
// emmitted, and it passes this and the class list of the
// code generator tree to the constructor for `CgenClassTable'.
// That constructor performs all of the work of the code
// generator.
//
//*********************************************************

void program_class::cgen(ostream &os)
{
    // spim wants comments to start with '#'
    os << "# start of generated code\n";

    initialize_constants();
    CgenClassTable *codegen_classtable = new CgenClassTable(classes,os);

    os << "\n# end of generated code\n";
}


//////////////////////////////////////////////////////////////////////////////
//
//  emit_* procedures
//
//  emit_X  writes code for operation "X" to the output stream.
//  There is an emit_X for each opcode X, as well as emit_ functions
//  for generating names according to the naming conventions (see emit.h)
//  and calls to support functions defined in the trap handler.
//
//  Register names and addresses are passed as strings.  See `emit.h'
//  for symbolic names you can use to refer to the strings.
//
//////////////////////////////////////////////////////////////////////////////

static void emit_load(char *dest_reg, int offset, char *source_reg, ostream& s)
{
    s << LW << dest_reg << " " << offset * WORD_SIZE << "(" << source_reg << ")"
        << endl;
}

static void emit_store(char *source_reg, int offset, char *dest_reg, ostream& s)
{
    s << SW << source_reg << " " << offset * WORD_SIZE << "(" << dest_reg << ")"
            << endl;
}

static void emit_load_imm(char *dest_reg, int val, ostream& s)
{ s << LI << dest_reg << " " << val << endl; }

static void emit_load_address(char *dest_reg, char *address, ostream& s)
{ s << LA << dest_reg << " " << address << endl; }

static void emit_partial_load_address(char *dest_reg, ostream& s)
{ s << LA << dest_reg << " "; }

static void emit_load_bool(char *dest, const BoolConst& b, ostream& s)
{
    emit_partial_load_address(dest,s);
    b.code_ref(s);
    s << endl;
}

static void emit_load_string(char *dest, StringEntry *str, ostream& s)
{
    emit_partial_load_address(dest,s);
    str->code_ref(s);
    s << endl;
}

static void emit_load_int(char *dest, IntEntry *i, ostream& s)
{
    emit_partial_load_address(dest,s);
    i->code_ref(s);
    s << endl;
}

static void emit_move(char *dest_reg, char *source_reg, ostream& s)
{ s << MOVE << dest_reg << " " << source_reg << endl; }

static void emit_neg(char *dest, char *src1, ostream& s)
{ s << NEG << dest << " " << src1 << endl; }

static void emit_add(char *dest, char *src1, char *src2, ostream& s)
{ s << ADD << dest << " " << src1 << " " << src2 << endl; }

static void emit_addu(char *dest, char *src1, char *src2, ostream& s)
{ s << ADDU << dest << " " << src1 << " " << src2 << endl; }

static void emit_addiu(char *dest, char *src1, int imm, ostream& s)
{ s << ADDIU << dest << " " << src1 << " " << imm << endl; }

static void emit_div(char *dest, char *src1, char *src2, ostream& s)
{ s << DIV << dest << " " << src1 << " " << src2 << endl; }

static void emit_mul(char *dest, char *src1, char *src2, ostream& s)
{ s << MUL << dest << " " << src1 << " " << src2 << endl; }

static void emit_sub(char *dest, char *src1, char *src2, ostream& s)
{ s << SUB << dest << " " << src1 << " " << src2 << endl; }

static void emit_sll(char *dest, char *src1, int num, ostream& s)
{ s << SLL << dest << " " << src1 << " " << num << endl; }

static void emit_jalr(char *dest, ostream& s)
{ s << JALR << "\t" << dest << endl; }

static void emit_jal(char *address,ostream &s)
{ s << JAL << address << endl; }

static void emit_return(ostream& s)
{ s << RET << endl; }

static void emit_gc_assign(ostream& s)
{ s << JAL << "_GenGC_Assign" << endl; }

static void emit_disptable_ref(Symbol sym, ostream& s)
{  s << sym << DISPTAB_SUFFIX; }

static void emit_init_ref(Symbol sym, ostream& s)
{ s << sym << CLASSINIT_SUFFIX; }

static void emit_label_ref(int l, ostream &s)
{ s << "label" << l; }

static void emit_protobj_ref(Symbol sym, ostream& s)
{ s << sym << PROTOBJ_SUFFIX; }

static void emit_method_ref(Symbol classname, Symbol methodname, ostream& s)
{ s << classname << METHOD_SEP << methodname; }

static void emit_label_def(int l, ostream &s)
{
    emit_label_ref(l,s);
    s << ":" << endl;
}

static void emit_beqz(char *source, int label, ostream &s)
{
    s << BEQZ << source << " ";
    emit_label_ref(label,s);
    s << endl;
}

static void emit_beq(char *src1, char *src2, int label, ostream &s)
{
    s << BEQ << src1 << " " << src2 << " ";
    emit_label_ref(label,s);
    s << endl;
}

static void emit_bne(char *src1, char *src2, int label, ostream &s)
{
    s << BNE << src1 << " " << src2 << " ";
    emit_label_ref(label,s);
    s << endl;
}

static void emit_bleq(char *src1, char *src2, int label, ostream &s)
{
    s << BLEQ << src1 << " " << src2 << " ";
    emit_label_ref(label,s);
    s << endl;
}

static void emit_blt(char *src1, char *src2, int label, ostream &s)
{
    s << BLT << src1 << " " << src2 << " ";
    emit_label_ref(label,s);
    s << endl;
}

static void emit_blti(char *src1, int imm, int label, ostream &s)
{
    s << BLT << src1 << " " << imm << " ";
    emit_label_ref(label,s);
    s << endl;
}

static void emit_bgti(char *src1, int imm, int label, ostream &s)
{
    s << BGT << src1 << " " << imm << " ";
    emit_label_ref(label,s);
    s << endl;
}

static void emit_branch(int l, ostream& s)
{
    s << BRANCH;
    emit_label_ref(l,s);
    s << endl;
}

//
// Push a register on the stack. The stack grows towards smaller addresses.
//
static void emit_push(char *reg, ostream& str)
{
    GLOBAL_FP_OFF -= 1; // in words
    emit_store(reg,0,SP,str);
    emit_addiu(SP,SP,-4,str);
}

static void emit_pop(char *reg, ostream& str)
{
    GLOBAL_FP_OFF += 1; // in words
    emit_load(reg,1,SP,str);
    emit_addiu(SP,SP,4,str);
}

static void emit_pop_null(int num_words, ostream& str)
{
    GLOBAL_FP_OFF += num_words;
    emit_addiu(SP, SP, num_words*WORD_SIZE, str);
}

//
// Fetch the integer value in an Int object.
// Emits code to fetch the integer value of the Integer object pointed
// to by register source into the register dest
//
static void emit_fetch_int(char *dest, char *source, ostream& s)
{ emit_load(dest, DEFAULT_OBJFIELDS, source, s); }

//
// Emits code to store the integer value contained in register source
// into the Integer object pointed to by dest.
//
static void emit_store_int(char *source, char *dest, ostream& s)
{ emit_store(source, DEFAULT_OBJFIELDS, dest, s); }


static void emit_test_collector(ostream &s)
{
    emit_push(ACC, s);
    emit_move(ACC, SP, s); // stack end
    emit_move(A1, ZERO, s); // allocate nothing
    s << JAL << gc_collect_names[cgen_Memmgr] << endl;
    emit_addiu(SP,SP,4,s); // TODO: might this offset the GLOBALFPOFF? probably
    emit_load(ACC,0,SP,s);
}

static void emit_gc_check(char *source, ostream &s)
{
    if (source != (char*)A1) emit_move(A1, source, s);
    s << JAL << "_gc_check" << endl;
}


///////////////////////////////////////////////////////////////////////////////
//
// coding strings, ints, and booleans
//
// Cool has three kinds of constants: strings, ints, and booleans.
// This section defines code generation for each type.
//
// All string constants are listed in the global "stringtable" and have
// type StringEntry.  StringEntry methods are defined both for String
// constant definitions and references.
//
// All integer constants are listed in the global "inttable" and have
// type IntEntry.  IntEntry methods are defined for Int
// constant definitions and references.
//
// Since there are only two Bool values, there is no need for a table.
// The two booleans are represented by instances of the class BoolConst,
// which defines the definition and reference methods for Bools.
//
///////////////////////////////////////////////////////////////////////////////

//
// Strings
//
void StringEntry::code_ref(ostream& s)
{
    s << STRCONST_PREFIX << index;
}

//
// Emit code for a constant String.
// You should fill in the code naming the dispatch table.
//

void StringEntry::code_def(ostream& s, int stringclasstag)
{
    IntEntryP lensym = inttable.add_int(len);

    // Add -1 eye catcher
    s << WORD << "-1" << endl;

    code_ref(s);  s  << LABEL                               // label
            << WORD << stringclasstag << endl               // tag
            << WORD << (DEFAULT_OBJFIELDS + STRING_SLOTS + (len+4)/4) << endl // size
            << WORD;


 /***** Add dispatch information for class String ******/
            emit_disptable_ref(Str, s);

            s << endl;                                      // dispatch table
            s << WORD;  lensym->code_ref(s);  s << endl;    // string length
    emit_string_constant(s,str);                            // ascii string
    s << ALIGN;                                             // align to word
}

//
// StrTable::code_string
// Generate a string object definition for every string constant in the
// stringtable.
//
void StrTable::code_string_table(ostream& s, int stringclasstag)
{
    for (List<StringEntry> *l = tbl; l; l = l->tl())
        l->hd()->code_def(s,stringclasstag);
}

//
// Ints
//
void IntEntry::code_ref(ostream &s)
{
    s << INTCONST_PREFIX << index;
}

//
// Emit code for a constant Integer.
// You should fill in the code naming the dispatch table.
//

void IntEntry::code_def(ostream &s, int intclasstag)
{
    // Add -1 eye catcher
    s << WORD << "-1" << endl;

    code_ref(s);  s << LABEL                                    // label
            << WORD << intclasstag << endl                      // class tag
            << WORD << (DEFAULT_OBJFIELDS + INT_SLOTS) << endl  // object size
            << WORD;

 /***** Add dispatch information for class Int ******/
            emit_disptable_ref(Int, s);

            s << endl;                                          // dispatch table
            s << WORD << str << endl;                           // integer value
}


//
// IntTable::code_string_table
// Generate an Int object definition for every Int constant in the
// inttable.
//
void IntTable::code_string_table(ostream &s, int intclasstag)
{
    for (List<IntEntry> *l = tbl; l; l = l->tl())
        l->hd()->code_def(s,intclasstag);
}


//
// Bools
//
BoolConst::BoolConst(int i) : val(i) { assert(i == 0 || i == 1); }

void BoolConst::code_ref(ostream& s) const
{
    s << BOOLCONST_PREFIX << val;
}

//
// Emit code for a constant Bool.
// You should fill in the code naming the dispatch table.
//

void BoolConst::code_def(ostream& s, int boolclasstag)
{
    // Add -1 eye catcher
    s << WORD << "-1" << endl;

    code_ref(s);  s << LABEL                                      // label
            << WORD << boolclasstag << endl                       // class tag
            << WORD << (DEFAULT_OBJFIELDS + BOOL_SLOTS) << endl   // object size
            << WORD;

 /***** Add dispatch information for class Bool ******/
            emit_disptable_ref(Bool, s);

            s << endl;                                            // dispatch table
            s << WORD << val << endl;                             // value (0 or 1)
}

//////////////////////////////////////////////////////////////////////////////
//
//  CgenClassTable methods
//
//////////////////////////////////////////////////////////////////////////////

//***************************************************
//
//  Emit code to start the .data segment and to
//  declare the global names.
//
//***************************************************

void CgenClassTable::code_global_data()
{
    Symbol main    = idtable.lookup_string(MAINNAME);
    Symbol string  = idtable.lookup_string(STRINGNAME);
    Symbol integer = idtable.lookup_string(INTNAME);
    Symbol boolc   = idtable.lookup_string(BOOLNAME);

    str << "\t.data\n" << ALIGN;
    //
    // The following global names must be defined first.
    //
    str << GLOBAL; falsebool.code_ref(str);  str << endl;
    str << GLOBAL; truebool.code_ref(str);   str << endl;
    str << GLOBAL << INTTAG << endl;
    str << GLOBAL << BOOLTAG << endl;
    str << GLOBAL << STRINGTAG << endl;
    str << GLOBAL << MAXTAG << endl;
    str << GLOBAL << CLASSNAMETAB << endl;
    str << GLOBAL << CLASSOBJTAB << endl;
    str << GLOBAL << CLASSPARENTTAB << endl;

    // Now define the global labels to proto and inits
    for(auto it=classtag_map.begin(); it!=classtag_map.end(); ++it){
        str << GLOBAL; emit_protobj_ref(it->first,str);   str << endl;
        str << GLOBAL; emit_init_ref(it->first, str); str << endl;

    }
    //
    // We also need to know the tag of the Int, String, and Bool classes
    // during code generation.
    ///String
    str << INTTAG << LABEL
            << WORD << intclasstag << endl;
    str << BOOLTAG << LABEL
            << WORD << boolclasstag << endl;
    str << STRINGTAG << LABEL
            << WORD << stringclasstag << endl;
}


//***************************************************
//
//  Emit code to start the .text segment and to
//  declare the global names.
//
//***************************************************

void CgenClassTable::code_global_text()
{
    str << GLOBAL << HEAP_START << endl
            << HEAP_START << LABEL
            << WORD << 0 << endl
            << "\t.text" << endl;

    // Emit global labels for all methods
    for(auto it=classtag_map.cbegin(); it!=classtag_map.cend(); ++it){
        Symbol curr_class = it->first;
        CgenNode* curr_node = probe(it->first);
        assert(curr_node != NULL);
        if(!curr_node->basic()){
            for(int i=curr_node->features->first(); curr_node->features->more(i); i=curr_node->features->next(i)){
                if(!curr_node->features->nth(i)->is_attr()){
                    str << GLOBAL;
                    emit_method_ref(curr_node->name, curr_node->features->nth(i)->get_name(), str);
                    str << endl;
                }
            }
        }
    }
}

void CgenClassTable::code_bools(int boolclasstag)
{
    falsebool.code_def(str,boolclasstag);
    truebool.code_def(str,boolclasstag);
}

void CgenClassTable::code_select_gc()
{
    //
    // Generate GC choice constants (pointers to GC functions)
    //
    str << GLOBAL << "_MemMgr_INITIALIZER" << endl;
    str << "_MemMgr_INITIALIZER:" << endl;
    str << WORD << gc_init_names[cgen_Memmgr] << endl;
    str << GLOBAL << "_MemMgr_COLLECTOR" << endl;
    str << "_MemMgr_COLLECTOR:" << endl;
    str << WORD << gc_collect_names[cgen_Memmgr] << endl;
    str << GLOBAL << "_MemMgr_TEST" << endl;
    str << "_MemMgr_TEST:" << endl;
    str << WORD << (cgen_Memmgr_Test == GC_TEST) << endl;
}


//********************************************************
//
// Emit code to reserve space for and initialize all of
// the constants.  Class names should have been added to
// the string table (in the supplied code, is is done
// during the construction of the inheritance graph), and
// code for emitting string constants as a side effect adds
// the string's length to the integer table.  The constants
// are emmitted by running through the stringtable and inttable
// and producing code for each entry.
//
//********************************************************

void CgenClassTable::code_constants()
{
    //
    // Add constants that are required by the code generator.
    //
    stringtable.add_string("");
    inttable.add_string("0");

    stringtable.code_string_table(str,stringclasstag);
    inttable.code_string_table(str,intclasstag);
    code_bools(boolclasstag);
}


CgenClassTable::CgenClassTable(Classes classes, ostream& s) : nds(NULL) , str(s)
{
     enterscope();
     objectST.enterscope();
     if (cgen_debug) objectST.addid(No_class, NULL);
     if (cgen_debug) cout << "## Building CgenClassTable" << endl;
     install_basic_classes();
     install_classes(classes);
     build_inheritance_tree();
     //  build classtag_map
     int curr_classtag = 0;
     std::vector<int> curr_ancestors;
     build_classtag_map(root(), &curr_classtag, &curr_ancestors);

     stringclasstag = classtag_map[Str];
     intclasstag =    classtag_map[Int];
     boolclasstag =   classtag_map[Bool];

     code();
     objectST.exitscope();
     exitscope();
}

void CgenClassTable::install_basic_classes()
{

// The tree package uses these globals to annotate the classes built below.
    //curr_lineno  = 0;
    Symbol filename = stringtable.add_string("<basic class>");

//
// A few special class names are installed in the lookup table but not
// the class list.  Thus, these classes exist, but are not part of the
// inheritance hierarchy.
// No_class serves as the parent of Object and the other special classes.
// SELF_TYPE is the self class; it cannot be redefined or inherited.
// prim_slot is a class known to the code generator.
//
    addid(No_class,
    new CgenNode(class_(No_class,No_class,nil_Features(),filename),
                    Basic,this));
    addid(SELF_TYPE,
    new CgenNode(class_(SELF_TYPE,No_class,nil_Features(),filename),
                    Basic,this));
    addid(prim_slot,
    new CgenNode(class_(prim_slot,No_class,nil_Features(),filename),
                    Basic,this));

//
// The Object class has no parent class. Its methods are
//        cool_abort() : Object    aborts the program
//        type_name() : Str        returns a string representation of class name
//        copy() : SELF_TYPE       returns a copy of the object
//
// There is no need for method bodies in the basic classes---these
// are already built in to the runtime system.
//
    install_class(
     new CgenNode(
        class_(Object,
         No_class,
         append_Features(
                     append_Features(
                     single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
                     single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
                     single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
         filename),
        Basic,this));

//
// The IO class inherits from Object. Its methods are
//        out_string(Str) : SELF_TYPE          writes a string to the output
//        out_int(Int) : SELF_TYPE               "    an int    "  "     "
//        in_string() : Str                    reads a string from the input
//        in_int() : Int                         "   an int     "  "     "
//
     install_class(
        new CgenNode(
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
         filename),
        Basic,this));

//
// The Int class has no methods and only a single attribute, the
// "val" for the integer.
//
     install_class(
        new CgenNode(
         class_(Int,
            Object,
                        single_Features(attr(val, prim_slot, no_expr())),
            filename),
         Basic,this));

//
// Bool also has only the "val" slot.
//
        install_class(
         new CgenNode(
            class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())),filename),
            Basic,this));

//
// The class Str has a number of slots and operations:
//       val                                  ???
//       str_field                            the string itself
//       length() : Int                       length of the string
//       concat(arg: Str) : Str               string concatenation
//       substr(arg: Int, arg2: Int): Str     substring
//
     install_class(
        new CgenNode(
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
             filename),
                Basic,this));

}

// CgenClassTable::install_class
// CgenClassTable::install_classes
//
// install_classes enters a list of classes in the symbol table.
//
void CgenClassTable::install_class(CgenNodeP nd)
{
    Symbol name = nd->get_name();

    if (probe(name))
        {
            return;
        }

    // The class name is legal, so add it to the list of classes
    // and the symbol table.
    nds = new List<CgenNode>(nd,nds);
    addid(name,nd);
}

void CgenClassTable::install_classes(Classes cs)
{
    for(int i = cs->first(); cs->more(i); i = cs->next(i))
        install_class(new CgenNode(cs->nth(i),NotBasic,this));
}

//
// CgenClassTable::build_inheritance_tree
//
void CgenClassTable::build_inheritance_tree()
{
    for(List<CgenNode> *l = nds; l; l = l->tl())
            set_relations(l->hd());
}

//
// CgenClassTable::set_relations
//
// Takes a CgenNode and locates its, and its parent's, inheritance nodes
// via the class table.  Parent and child pointers are added as appropriate.
//
void CgenClassTable::set_relations(CgenNodeP nd)
{
    CgenNode *parent_node = probe(nd->get_parent());
    nd->set_parentnd(parent_node);
    parent_node->add_child(nd);
}

void CgenNode::add_child(CgenNodeP n)
{
    children = new List<CgenNode>(n,children);
}

void CgenNode::set_parentnd(CgenNodeP p)
{
    assert(parentnd == NULL);
    assert(p != NULL);
    parentnd = p;
}

void CgenClassTable::code()
{
    if (cgen_debug) cout << "## coding global data" << endl;
    code_global_data();

    if (cgen_debug) cout << "## choosing gc" << endl;
    code_select_gc();

    if (cgen_debug) cout << "## coding constants" << endl;
    code_constants();

    // Add your code to emit
    // - class_nameTab
    str << CLASSNAMETAB << LABEL;
    code_class_nameTab(root());
    // - class_objTab
    str << CLASSOBJTAB << LABEL;
    code_class_objTab(root());
    // - class_parentTab
    str << CLASSPARENTTAB << LABEL;
    code_class_parentTab(root());

    str << MAXTAG << LABEL;
    str << WORD << (classtag_map.size()-1) << endl;

    // - dispatch tables
    std::map<Symbol, std::vector<Symbol> > methods;
    std::vector<Symbol> method_order;
    code_dispatch_tables(root(), &methods, &method_order);
    // - prototype objects
    std::vector<std::pair<Symbol, Symbol> > parent_attr;
    code_prototypes(root(), &parent_attr);

    if (cgen_debug) cout << "## coding global text" << endl;
    code_global_text();

    // Add your code to emit
    // - object initializer
    // This handles class attributes
    int num_parent_attr = 0;
    code_object_initializers(root(), &num_parent_attr);

    // - the class methods
    // This handles class methods
    num_parent_attr = 0;
    code_all_class_methods(root(), &num_parent_attr);

    // - etc...

}

CgenNodeP CgenClassTable::root()
{
     return probe(Object);
}

void CgenClassTable::build_classtag_map(CgenNode* curr_node, int* curr_classtag,
    std::vector<int>* curr_ancestors)
{
    classtag_map[curr_node->name] = *curr_classtag;

    classtag_ancestor_map[curr_node->name].insert(*curr_classtag);

    // Remember myself as my ancestor
    curr_ancestors->push_back(*curr_classtag);

    ++ *curr_classtag;
    for(List<CgenNode> *l = curr_node->get_children(); l; l=l->tl()){
        CgenNode* curr_child = l->hd();
        // insert my ancestors (includes myself) into my child's
        for(auto it=curr_ancestors->cbegin(); it!=curr_ancestors->cend(); ++it){
            classtag_ancestor_map[curr_child->name].insert(*it);
        }
        build_classtag_map(curr_child, curr_classtag, curr_ancestors);

    }

    // Forget myself as my ancestor
    curr_ancestors->pop_back();

}

void CgenClassTable::code_class_nameTab(CgenNode* curr_node)
{
    StringEntry* entry = stringtable.add_string(curr_node->name->get_string());
    str << WORD; entry->code_ref(str); str << endl;
    for(List<CgenNode> *l = curr_node->get_children(); l; l=l->tl()){
        CgenNode* curr_child = l->hd();
        code_class_nameTab(curr_child);
    }
}

void CgenClassTable::code_class_objTab(CgenNode* curr_node)
{
    str << WORD; emit_protobj_ref(curr_node->name, str); str << endl;
    str << WORD; emit_init_ref(curr_node->name, str); str << endl;

    for(List<CgenNode> *l = curr_node->get_children(); l; l=l->tl()){
        CgenNode* curr_child = l->hd();
        code_class_objTab(curr_child);
    }
}

void CgenClassTable::code_class_parentTab(CgenNode* curr_node){
    str << WORD << classtag_map[curr_node->get_parentnd()->name] << endl;

    for(List<CgenNode> *l = curr_node->get_children(); l; l=l->tl()){
        CgenNode* curr_child = l->hd();
        code_class_parentTab(curr_child);
    }
}

void CgenClassTable::code_dispatch_tables(CgenNode* curr_node,
        std::map<Symbol, std::vector<Symbol> >* methods,
        std::vector<Symbol>* method_order)
{
    // Add this node's methods
    int num_new_methods = 0;
    for(int i=curr_node->features->first(); curr_node->features->more(i); i=curr_node->features->next(i)){
        if(!(curr_node->features->nth(i)->is_attr())){
            // NOTE: this will override methods witho most recent class's implementation
            Symbol method_name = curr_node->features->nth(i)->get_name();

            if(methods->find(method_name) == methods->end()){
                method_order->push_back(method_name);
                ++num_new_methods;
            }
            (*methods)[method_name].push_back(curr_node->name);
        }
    }

    // Save method order for this class in dispatch_table
    //dispatch_table[curr_node->name].insert(method_order);
    // Emit the references to the methods
    emit_disptable_ref(curr_node->name, str); str << LABEL;
    for(auto it=method_order->cbegin(); it!=method_order->cend(); ++it){
        str << WORD; emit_method_ref((*methods)[*it].back(), *it, str); str << endl;
        dispatch_table[curr_node->name].push_back(*it);

    }
    // Recurse on children
    for(List<CgenNode> *l = curr_node->get_children(); l; l=l->tl()){
        CgenNode* curr_child = l->hd();
        code_dispatch_tables(curr_child, methods, method_order);
    }
    // Remove this node's methods
    for(int i=curr_node->features->first(); curr_node->features->more(i); i=curr_node->features->next(i)){
        if(!(curr_node->features->nth(i)->is_attr())){
            Symbol method_name = curr_node->features->nth(i)->get_name();
            (*methods)[method_name].pop_back();
            if((*methods)[method_name].size()==0){
                methods->erase(method_name);
            }
        }
    }
    for(int i=0; i<num_new_methods; ++i)
        method_order->pop_back();
}

void CgenClassTable::code_prototypes(CgenNode* curr_node,
        std::vector<std::pair<Symbol, Symbol> >* parent_attr)
{
    code_prototype(curr_node, parent_attr);
    // Add my attr as parent attr
    for(int i=curr_node->features->first(); curr_node->features->more(i); i=curr_node->features->next(i)){
        if(curr_node->features->nth(i)->is_attr()){
            parent_attr->push_back(std::pair<Symbol, Symbol>(curr_node->features->nth(i)->get_type(),
                                                             curr_node->features->nth(i)->get_name()));
        }
    }
    // Recurse into children
    for(List<CgenNode> *l = curr_node->get_children(); l; l=l->tl()){
        CgenNode* curr_child = l->hd();
        code_prototypes(curr_child, parent_attr);
    }
    // Pop this parent's attributes
    for(int i=curr_node->features->first(); curr_node->features->more(i); i=curr_node->features->next(i)){
        if(curr_node->features->nth(i)->is_attr()){
            parent_attr->pop_back();
        }
    }
}

static void emit_default_for_class(ostream& str, Symbol curr_class){
    if(curr_class == prim_slot)
        str << 0;
    else if (curr_class == Int)
        inttable.add_int(0)->code_ref(str);
    else if (curr_class == Bool)
        falsebool.code_ref(str);
    else if (curr_class == Str)
        stringtable.add_string("")->code_ref(str);
    else
        str << 0;
    // Cool Runtime Page 2: The value void is a null pointer and is represented
    // by the 32-bit value 0,  All uninitialized variables(except variables of
    // type Int, Bool, and String; see the Cool Reference Manual)
    // should be set to void by default.
}

void CgenClassTable::code_prototype(CgenNode* curr_node,
        std::vector<std::pair<Symbol, Symbol> >* parent_attr)
{
    int num_slots = 0;
    for(int i=curr_node->features->first(); curr_node->features->more(i); i=curr_node->features->next(i)){
        if(curr_node->features->nth(i)->is_attr()){
            num_slots += 1;
        }
    }
    // Add -1 eye catcher for the GC
    str << WORD << "-1" << endl;
    // label
    emit_protobj_ref(curr_node->name, str);  str << LABEL;
    // tag
    str << WORD << classtag_map[curr_node->name] << endl;
    // size
    int num_parent_attr = parent_attr->size();
    str << WORD << (DEFAULT_OBJFIELDS + num_slots + num_parent_attr) << endl;
    // dispatch table
    str << WORD; emit_disptable_ref(curr_node->name, str); str << endl;

    // Parent attributes
    for(auto it=parent_attr->cbegin(); it!=parent_attr->cend(); ++it){
        str << WORD;
        // Default attr values
        emit_default_for_class(str, it->first);
        str << endl;
    }
    // Own Attributes
    for(int i = curr_node->features->first(); curr_node->features->more(i); i=curr_node->features->next(i)){
        if(curr_node->features->nth(i)->is_attr()){
            str << WORD;
            // Default attr values
            emit_default_for_class(str, curr_node->features->nth(i)->get_type());
            str << endl;
        }
    }
}

// This is to make FP, S0 and RA caller saved
static void emit_remember_regs(ostream& str){
    //emit_push(FP, str);
    //emit_push(SELF, str);
    //emit_push(RA, str);
    emit_addiu(SP, SP, -3*WORD_SIZE, str);
    emit_store(FP, 3, SP, str);
    emit_store(SELF, 2, SP, str);
    emit_store(RA, 1, SP, str);
    // Put frame pointer 4 words above: at the top of the AR
    emit_addiu(FP, SP, 4*WORD_SIZE, str);
    emit_move(SELF, ACC, str);
    FP_OFF_SCOPE.push_back(GLOBAL_FP_OFF);
    GLOBAL_FP_OFF = -3; // Reset this: in words
}

// Aaaaand to restore them
static void emit_restore_remember_regs(ostream& str){
    //emit_pop(RA, str);
    //emit_pop(SELF, str);
    //emit_pop(FP, str);
    emit_load(FP, 3, SP, str);
    emit_load(SELF, 2, SP, str);
    emit_load(RA, 1, SP, str);
    emit_addiu(SP, SP, 3*WORD_SIZE, str);
    GLOBAL_FP_OFF = FP_OFF_SCOPE.back(); // Reset it to the last one you remember
    FP_OFF_SCOPE.pop_back();
}

void CgenClassTable::code_object_initializers(CgenNodeP curr_node, int* num_parent_attr)
{
    // Doing this in DFS order to account for attribute offsetting
    code_object_initializer(curr_node, num_parent_attr);
    // Count my attributes as parent attr
    for(int i=curr_node->features->first(); curr_node->features->more(i); i=curr_node->features->next(i)){
        if(curr_node->features->nth(i)->is_attr()){
            ++(*num_parent_attr);
        }
    }
    // Recurse into children
    for(List<CgenNode> *l = curr_node->get_children(); l; l=l->tl()){
        CgenNode* curr_child = l->hd();
        code_object_initializers(curr_child, num_parent_attr);
    }
    // Un-count my attributes as parent attr
    for(int i=curr_node->features->first(); curr_node->features->more(i); i=curr_node->features->next(i)){
        if(curr_node->features->nth(i)->is_attr()){
            --(*num_parent_attr);
        }
    }
}

void CgenClassTable::code_object_initializer(CgenNodeP curr_node, int* num_parent_attr)
{
    Symbol curr_class = curr_node->name;
    // Label to the class init methdd
    emit_init_ref(curr_class, str); str << LABEL;
    // Store the AR header
    emit_remember_regs(str);

    // Call init of parent class
    assert(curr_node != NULL);
    if(curr_node->get_parent() != No_class){
        str << JAL;
        emit_init_ref(curr_node->get_parent(), str);
        str << endl;
    }

    // initialize class attributes here
    // Precond: SELF has addr to self object
    //          ACC will be used to load obj addresses and store them
    //              to the SELF address

    if(!curr_node->basic()){
        // skipping the default object fields and the number of parent attributes
        int offset = DEFAULT_OBJFIELDS + *num_parent_attr;
        for(int i=curr_node->features->first(); curr_node->features->more(i); i=curr_node->features->next(i)){
            if(curr_node->features->nth(i)->is_attr()){
                // Precond: SELF has self object, ACC can be discarded
                curr_node->features->nth(i)->code(str, offset, this);
                ++offset;
            }
        }
    }

    // After using return values, restore SELF to ACC, before it is
    // overwritten by the restore of the AR
    emit_move(ACC, SELF, str);
    // Restore AR header
    emit_restore_remember_regs(str);
    emit_return(str);
}

static void addToScope(Symbol name, char* register_name, int offset, Scopetable* objectST){
    pair_container.push_back(std::make_unique<std::pair<char*, int> >(register_name, offset));
    if(cgen_debug) printf("# SCOPE: adding register %s\n", register_name);
    if(cgen_debug) printf("# SCOPE: at offset: %d\n", offset);
    objectST->addid(name, (pair_container.back().get()));
}

void CgenClassTable::code_all_class_methods(CgenNodeP curr_node, int* num_parent_attr){
    // Doing this in DFS order to account for correct scoping
    objectST.enterscope();

    if(!curr_node->basic()){
        // Set the current class so you can get it at compile time during AST traversal
        this->current_node = curr_node;
        code_class_methods(curr_node, num_parent_attr);
    }
    // Count my attributes as parent attr
    for(int i=curr_node->features->first(); curr_node->features->more(i); i=curr_node->features->next(i)){
        if(curr_node->features->nth(i)->is_attr()){
            ++(*num_parent_attr);
        }
    }
    // Recurse into children
    for(List<CgenNode> *l = curr_node->get_children(); l; l=l->tl()){
        CgenNode* curr_child = l->hd();
        code_all_class_methods(curr_child, num_parent_attr);
    }
    // Un-count my attributes as parent attr
    for(int i=curr_node->features->first(); curr_node->features->more(i); i=curr_node->features->next(i)){
        if(curr_node->features->nth(i)->is_attr()){
            --(*num_parent_attr);
        }
    }
    objectST.exitscope();
}

void CgenClassTable::code_class_methods(CgenNodeP curr_node, int* num_parent_attr){
    int offset = DEFAULT_OBJFIELDS + *num_parent_attr;
    for(int i=curr_node->features->first(); curr_node->features->more(i); i=curr_node->features->next(i)){
        if(!curr_node->features->nth(i)->is_attr()){
            // BEGIN method def
            emit_method_ref(curr_node->name, curr_node->features->nth(i)->get_name(), str);
            str << LABEL;
            emit_remember_regs(str);

            int num_formals = curr_node->features->nth(i)->code(str, 0, this);
            // Postcond: result is in ACC

            // Restore AR
            emit_restore_remember_regs(str);
            // Pop the method parameters
            emit_pop_null(num_formals, str);
            emit_return(str);
            // END method def
        }else{
            // For attributes, just add them to the scope. This means they
            // will be loaded from the class object, not from the AR
            // TODO: what regiser are we offsetting from? Self?
            addToScope(curr_node->features->nth(i)->get_name(), SELF, offset, &objectST);
            ++offset;
        }
    }
}


///////////////////////////////////////////////////////////////////////
//
// CgenNode methods
//
///////////////////////////////////////////////////////////////////////

CgenNode::CgenNode(Class_ nd, Basicness bstatus, CgenClassTableP ct) :
     class__class((const class__class &) *nd),
     parentnd(NULL),
     children(NULL),
     basic_status(bstatus)
{
     stringtable.add_string(name->get_string());          // Add class name to string table
}

//******************************************************************
//
//   Fill in the following methods to produce code for the
//   appropriate expression.  You may add or remove parameters
//   as you wish, but if you do, remember to change the parameters
//   of the declarations in `cool-tree.h'  Sample code for
//   constant integers, strings, and booleans are provided.
//
//*****************************************************************

int method_class::code(ostream &s, int offset, CgenClassTable* cgentable){
    cgentable->objectST.enterscope();
    // Handle arguments (formals) passed: make space in the AR for them
    int j = 0;
    for(int i=formals->first(); formals->more(i); i=formals->next(i)){
        // TODO: check what happens with nested calls
        addToScope(formals->nth(i)->get_name(), FP, j, &(cgentable->objectST));
        ++j;
    }

    expr->code(s, cgentable);
    cgentable->objectST.exitscope();
    return j;
}

int attr_class::code(ostream &s, int offset, CgenClassTable* cgentable){
    // Precond: ACC can be discarded, SELF has reference to self object
    init->code(s, cgentable);
    // Postcond: ACC has the reference to the value the thing is initialized to
    //if(init->get_type() != No_type){ // our semant implementation
    if(init->get_type() != NULL){ // their semant implementation
        // If there was an initilization, then store it.
        // Otherwise, leave default value from protobj
        // Precond: SELF has self object, ACC has value to store
        emit_store(ACC, offset, SELF, s);
    }
    return 0;
}

void assign_class::code(ostream &s, CgenClassTable* cgentable) {

    // Evaluate expression and save in ACC
    expr->code(s, cgentable);

    // Get adress of Id, store in S1
    auto* lookup = cgentable->objectST.lookup(name);
    assert(lookup != NULL);
    if(cgen_debug) printf("# Assign: BEGIN resolved address (assign) \n");
    if(cgen_debug) dump_pair_container();
    emit_store(ACC, lookup->second, lookup->first, s);
    if(cgen_debug) printf("# Assign: END resolved address (assign) \n");

}

void static_dispatch_class::code(ostream &s, CgenClassTable* cgentable) {
    int dispatch_label = GLOBAL_LABEL_CTR++;
    // Compute which method is being used based on the name to get the right offset from T1
    Symbol dispatch_class_type = type_name;
    if(dispatch_class_type == SELF_TYPE){
        dispatch_class_type = cgentable->getCurrentNode()->name;
        if(cgen_debug) printf("# Dispatch Resolving SELF TYPE to %s\n ", dispatch_class_type->get_string());
    }

    // Special handling of basic class dispatches
    if((dispatch_class_type == Str) ||
       //(isDescendantOf(IO, dispatch_class_type) && (name == out_string || name == out_int))){
       ((name == out_string || name == out_int))){
        // For some reason, the runtime expects these arguments the other way
        // around in the stack :/
        for(int i = actual->first(); actual ->more(i); i = actual->next(i)) {
            actual->nth(i)->code(s, cgentable);
            emit_push(ACC, s);
            // Also, let's correct the GLOBAL_FP_OFFSET, since that is not maintained
            // by the runtime-defined methods for String
            GLOBAL_FP_OFF += 1;
        }
    }else{
        // Add the arguments in reverse order
        std::vector<Expression> reverse_helper;
        for(int i = actual->first(); actual ->more(i); i = actual->next(i)) {
            reverse_helper.insert(reverse_helper.begin(),actual->nth(i));
        }
        for(uint i=0; i<reverse_helper.size(); ++i){
            reverse_helper[i]->code(s, cgentable);
            //s << JAL;
            //emit_method_ref(Object, _copy, s);
            //s << endl;
            emit_push(ACC, s);
        }
    }

    // Don't push self emit_push(SELF, s);

    // Cgen expression calling method dispatch
    expr->code(s, cgentable);

    // If expression is void then we call dispatch_abort as in cool runtime manual
    // Dispatch abort requires line number in T1 and filename in ACC
    emit_bne(ACC, ZERO, dispatch_label, s);
    emit_partial_load_address(ACC,s);
    stringtable.add_string(cgentable->getCurrentNode()->get_filename()->get_string())->code_ref(s); s << endl;
    emit_load_imm(T1, get_line_number(), s);
    emit_jal("_dispatch_abort", s);

    // Otherwise we run the dispatch
    emit_label_def(dispatch_label, s);
    // The ACC now holds the address to the resulting object in memory after evaluationg expr

    std::vector<Symbol> methods = cgentable->dispatch_table[dispatch_class_type];
    int method_offset=0;
    for(uint i=0; i<methods.size(); ++i){
        if(methods[i] == name) {
            break;
        }
        ++method_offset;
    }
    if(cgen_debug) printf("# Dispatch: method size: %lu\n", methods.size());
    if(cgen_debug) printf("# Dispatch: method offset: %i\n", method_offset);



    // For static dispatch we load the dispatch table of the declared type
    emit_partial_load_address(T1, s);
    s << dispatch_class_type->get_string() << DISPTAB_SUFFIX << endl;
    emit_load(T1, method_offset, T1, s);
    emit_jalr(T1,s);

}

void dispatch_class::code(ostream &s, CgenClassTable* cgentable) {
    int dispatch_label = GLOBAL_LABEL_CTR++;
    Symbol dispatch_class_type = expr->get_type();
    // Compute which method is being used based on the name to get the right offset from T1
    if(dispatch_class_type == SELF_TYPE){
        dispatch_class_type = cgentable->getCurrentNode()->name;
        if(cgen_debug) printf("# Dispatch Resolving SELF TYPE to %s\n ", dispatch_class_type->get_string());
    }

    // Special handling of basic class dispatches
    if((dispatch_class_type == Str) ||
       //(isDescendantOf(IO, dispatch_class_type) && (name == out_string || name == out_int))){
       ((name == out_string || name == out_int))){
        // For some reason, the runtime expects these arguments the other way
        // around in the stack :/
        for(int i = actual->first(); actual ->more(i); i = actual->next(i)) {
            actual->nth(i)->code(s, cgentable);
            emit_push(ACC, s);
            // Also, let's correct the GLOBAL_FP_OFFSET, since that is not maintained
            // by the runtime-defined methods for String
            GLOBAL_FP_OFF += 1;
        }
    }else{
        // Normal dispatch case
        // Add the arguments in reverse order
        std::vector<Expression> reverse_helper;
        for(int i = actual->first(); actual ->more(i); i = actual->next(i)) {
            reverse_helper.insert(reverse_helper.begin(),actual->nth(i));
        }
        for(uint i=0; i<reverse_helper.size(); ++i){
            reverse_helper[i]->code(s, cgentable);
            //s << JAL;
            //emit_method_ref(Object, _copy, s);
            //s << endl;
            emit_push(ACC, s);
        }
    }

    // Don't add self emit_push(SELF, s);

    // Cgen expression calling method dispatch
    expr->code(s, cgentable);

    // If expression is void then we call dispatch_abort as in cool runtime manual
    // Dispatch abort requires line number in T1 and filename in ACC
    emit_bne(ACC, ZERO, dispatch_label, s);
    emit_partial_load_address(ACC,s);
    stringtable.add_string(cgentable->getCurrentNode()->get_filename()->get_string())->code_ref(s); s << endl;
    emit_load_imm(T1, get_line_number(), s);
    emit_jal("_dispatch_abort", s);

    // Otherwise we run the dispatch
    emit_label_def(dispatch_label, s);
    // The ACC now holds the address to the resulting object in memory after evaluating expr
    // Load the address of the dispatch table into T1
    emit_load(T1, 2, ACC, s);

    std::vector<Symbol> methods = cgentable->dispatch_table[dispatch_class_type];
    int method_offset=0;
    for(uint i=0; i<methods.size(); ++i){
        if(methods[i] == name) {
            break;
        }
        ++method_offset;
    }
    if(cgen_debug) printf("# Dispatch: method size: %lu\n", methods.size());
    if(cgen_debug) printf("# Dispatch: method offset: %i\n", method_offset);
    emit_load(T1, method_offset, T1, s);
    emit_jalr(T1,s);
}

void cond_class::code(ostream &s, CgenClassTable* cgentable) {
    int end_if_label = GLOBAL_LABEL_CTR++;
    int else_label = GLOBAL_LABEL_CTR++;
    pred->code(s, cgentable);
    // ACC contains a bool object

    // Get the actual value of the expr
    emit_fetch_int(T1, ACC, s);
    // If predicate is false, jump to else
    emit_beqz(T1, else_label, s);
    then_exp->code(s, cgentable);
    // Unconditional branch to bottom: end if
    emit_branch(end_if_label, s);
    emit_label_def(else_label, s);
    else_exp->code(s, cgentable);
    // End if
    emit_label_def(end_if_label, s);
}

void loop_class::code(ostream &s, CgenClassTable* cgentable) {
    int loop_label = GLOBAL_LABEL_CTR++;
    int pool_label = GLOBAL_LABEL_CTR++;
    emit_label_def(loop_label, s);
    pred->code(s, cgentable);
    // ACC contains a bool object

    // Get the actual value of the expr
    emit_fetch_int(T1, ACC, s);
    // If predicate is false, jump to bottom: end loop
    emit_beqz(T1, pool_label, s);
    // loop body
    body->code(s, cgentable);
    // Unconditional branch to top: evaluate predicate
    emit_branch(loop_label, s);

    // End of loop
    emit_label_def(pool_label, s);
    // Loop returns void
    emit_move(ACC, ZERO, s);
}

void typcase_class::code(ostream &s, CgenClassTable* cgentable) {
    int begin_case_label = GLOBAL_LABEL_CTR++;
    int end_case_label = GLOBAL_LABEL_CTR++;
    int next_case_label = GLOBAL_LABEL_CTR++;
    int recurse_label, recurse_loop_label;

    emit_push(S1, s); // PUSH 1
        // check object for void
        expr->code(s, cgentable);
        // Object from the expr is in ACC

        emit_bne(ACC, ZERO, begin_case_label, s);
        // Case abort requires line number in T1 and filename in ACC
        emit_partial_load_address(ACC,s);
        stringtable.add_string(cgentable->getCurrentNode()->get_filename()->get_string())->code_ref(s); s << endl;
        emit_load_imm(T1, get_line_number(), s);
        emit_jal("_case_abort2", s);

        // Begn the case
        emit_label_def(begin_case_label, s);
        // Get the classtag into T2: this is the dynamic type
        emit_load(T2, 0, ACC, s);
        emit_move(T3, T2, s);

        // This is to take care of the closest ancestor thing
        // with the dynamic type of the expr which is in T2
        // Sort the cases from largest to smallest according to their classtags
        std::vector<int> sorted_branch_tags;
        std::vector<branch_class*> sorted_branches;
        for(int i = cases->first(); cases->more(i); i = cases->next(i)) {
            Symbol case_type = ((branch_class*)(cases->nth(i)))->type_decl;
            branch_class* branch = ((branch_class*)(cases->nth(i)));

            int case_tag = cgentable->classtag_map[case_type];
            if(cgen_debug) printf("Case_tag: %d\n", case_tag);
            // insert case_tag so that vector remains in largest->smallest order

            if(sorted_branch_tags.size() == 0){
                if(cgen_debug) printf("Performed intial insert into sorted_branch_tags \n");
                sorted_branch_tags.push_back(case_tag);
                sorted_branches.push_back(branch);
            }else{
                int j = 0;
                for(auto it=sorted_branch_tags.cbegin(); it!=sorted_branch_tags.cend(); ++it) {
                    if(case_tag>*it){
                        if(cgen_debug) printf("Inserted into sorted_branch_tags at index %d\n", j);
                        sorted_branch_tags.insert(it, case_tag);
                        break;
                    }
                    ++j;
                }
                int k = 0;
                for(auto it=sorted_branches.cbegin(); it!=sorted_branches.cend(); ++it) {
                    if(k==j){
                        if(cgen_debug) printf("Inserted into sorted_branches at index %d\n", k);
                        sorted_branches.insert(it, branch);
                        break;
                    }
                    ++k;
                }
                // We might want to insert at the end
                if(j == (int) sorted_branch_tags.size()){
                    sorted_branch_tags.push_back(case_tag);
                    sorted_branches.push_back(branch);
                }
            }
        }
        if(cgen_debug) printf("Size of sorted_branches list: %lu\n", sorted_branches.size());

        // Now emit the branches, in desc order
        for(auto it=sorted_branches.cbegin(); it!=sorted_branches.cend(); ++it){
            Symbol case_type = (*it)->type_decl;
            Symbol case_name = (*it)->name;
            Expression case_expr = (*it)->expr;
            // TODO: can the type of a branch be SELF_TYPE?
            cgentable->objectST.enterscope();

            recurse_loop_label = GLOBAL_LABEL_CTR++;
            recurse_label = GLOBAL_LABEL_CTR++;
            emit_label_def(next_case_label, s);
            next_case_label = GLOBAL_LABEL_CTR++;
            emit_move(T2, T3, s);

            emit_label_def(recurse_loop_label, s);
            emit_blti(T2, cgentable->classtag_map[case_type], next_case_label, s);
            emit_bgti(T2, cgentable->classtag_map[case_type], recurse_label, s);
            // BEGIN: code for correct branch
                // Once you matched on a case,
                emit_push(ACC, s); // PUSH 2: Remember ACC for now
                emit_move(S1, ACC, s);

                // copy the proto for the matching case type, allocate it (don't init yet)
                emit_partial_load_address(ACC, s); emit_protobj_ref(case_type ,s); s << endl;
                s << JAL; emit_method_ref(Object, _copy, s); s << endl;
                // Push it onto the stack and add it to the scope
                emit_push(ACC, s); // PUSH 3
                addToScope(case_name, FP, GLOBAL_FP_OFF, &(cgentable->objectST));

                // Emit code to evaluate the expr
                case_expr->code(s, cgentable);
                // ACC has return value

                // Pop the proto object from the stack
                emit_pop_null(1, s); // POP 3

                // Unconditionally branch to the end of the case
                emit_branch(end_case_label, s);

            // Recurse Label
            emit_label_def(recurse_label, s);
            // Get the address of the parent table
            emit_load_address(T1, CLASSPARENTTAB, s);
            // Offset from the begining of the class parent table
            // to the corresponding location
            emit_sll(T2, T2, 2, s);
            emit_addu(T1, T1, T2, s);
            // Load the parent tag from the address
            emit_load(T2, 0, T1,s);
            // Unconditionally branch back to the begining of this case
            // must have classtag in T2
            emit_branch(recurse_loop_label, s);
            cgentable->objectST.exitscope();
        }
        // Finish off
        emit_label_def(next_case_label, s);
        // Abort with no matching caset code
        emit_jal("_case_abort", s);

        emit_label_def(end_case_label, s);
        emit_pop_null(1, s); // POP 2

    emit_pop(S1, s); // POP 1
}

void block_class::code(ostream &s, CgenClassTable* cgentable) {
    for(int i=body->first(); body->more(i); i=body->next(i)) {
        body->nth(i)->code(s, cgentable);
    }
}

void let_class::code(ostream &s, CgenClassTable* cgentable) {
    // If init is of type no_expr we initialize the identifier variable to default value of declared type
    init->code(s, cgentable);

    if(init->get_type() == NULL) {
        // Put init expr into heap memory
        // If there is no initialization then leave default from protobj
        emit_partial_load_address(ACC, s); emit_protobj_ref(type_decl,s); s << endl;
        // Now we have the protobj to copy in ACC
        // Allocate it: (not initializing just yet)
        s << JAL; emit_method_ref(Object, _copy, s); s << endl;
        // DONE
    }

    // Store address on the stack using GLOBAL_FP_OFF
    emit_push(ACC, s);

        // add it to objectST
        addToScope(identifier, FP, GLOBAL_FP_OFF, &(cgentable->objectST));

        // Eval body
        body -> code(s, cgentable);
        // ACC has return value

    // Now we restore the stack,
    emit_pop_null(1, s);

}

void plus_class::code(ostream &s, CgenClassTable* cgentable) {
    // Push temporaries to the stack
    emit_push(S1,s);

    e1->code(s, cgentable);
    emit_move(S1, ACC, s);

    e2->code(s, cgentable);
    // ACC ($a0) has result address.

    // Copy of object passed in $a0: result of e2 :)
    s << JAL;
    emit_method_ref(Object, _copy, s);
    s << endl;
    // Resulting object is in ACC ($a0)

    // Get the actual integers to add:
    // From our object layout, we KNOW the value for the int is offset 3 words
    // from the address of the int object
    emit_fetch_int(T2, ACC, s);
    emit_fetch_int(T1, S1, s);


    // Compute addition
    emit_add(T1, T1, T2, s);

    // Save it to the newly created object
    emit_store_int(T1, ACC, s);

    // Restore temporaries from the stack
    emit_pop(S1, s);

    // At this point, ACC still has a reference to the result of the addition
}

void sub_class::code(ostream &s, CgenClassTable* cgentable) {
    // Push temporaries to the stack
    emit_push(S1,s);


    e1->code(s, cgentable);
    emit_move(S1, ACC, s);

    e2->code(s, cgentable);
    // ACC ($a0) has result address.

    // Copy of object passed in $a0: result of e2 :)
    s << JAL;
    emit_method_ref(Object, _copy, s);
    s << endl;
    // Resulting object is in ACC ($a0)

    // Get the actual integers to add:
    // From our object layout, we KNOW the value for the int is offset 3 words
    // from the address of the int object
    emit_fetch_int(T2, ACC, s);
    emit_fetch_int(T1, S1, s);

    // Compute subtraction
    emit_sub(T1, T1, T2, s);

    // Save it to the newly created object
    emit_store_int(T1, ACC, s);

    // Restore temporaries from
    emit_pop(S1, s);

    // At this point, ACC still has a reference to the result of the addition
}

void mul_class::code(ostream &s, CgenClassTable* cgentable) {
    // Save temporaries to the stack
    emit_push(S1,s);


    e1->code(s, cgentable);
    emit_move(S1, ACC, s);

    e2->code(s, cgentable);
    // ACC ($a0) has result address.

    // Copy of object passed in $a0: result of e2 :)
    s << JAL;
    emit_method_ref(Object, _copy, s);
    s << endl;
    // Resulting object is in ACC ($a0)

    // Get the actual integers to add:
    // From our object layout, we KNOW the value for the int is offset 3 words
    // from the address of the int object
    emit_fetch_int(T2, ACC, s);
    emit_fetch_int(T1, S1, s);

    // Compute multiplication
    emit_mul(T1, T1, T2, s);

    // Save it to the newly created object
    emit_store_int(T1, ACC, s);

    // Restore temporaries from the stack
    emit_pop(S1, s);

    // At this point, ACC still has a reference to the result of the addition
}

void divide_class::code(ostream &s, CgenClassTable* cgentable) {
    // Save temporaries to the stack
    emit_push(S1,s);


    e1->code(s, cgentable);
    emit_move(S1, ACC, s);

    e2->code(s, cgentable);
    // ACC ($a0) has result address.

    // Copy of object passed in $a0: result of e2 :)
    s << JAL;
    emit_method_ref(Object, _copy, s);
    s << endl;
    // Resulting object is in ACC ($a0)

    // Get the actual integers to add:
    // From our object layout, we KNOW the value for the int is offset 3 words
    // from the address of the int object
    emit_fetch_int(T2, ACC, s);
    emit_fetch_int(T1, S1, s);

    // Compute division
    emit_div(T1, T1, T2, s);

    // Save it to the newly created object
    emit_store_int(T1, ACC, s);

    // Restore the contents of S1 we had saved
    emit_pop(S1, s);

    // At this point, ACC still has a reference to the result of the addition
}

void neg_class::code(ostream &s, CgenClassTable* cgentable) {
    e1->code(s, cgentable);
    //Copy of object passed in $a0, this will create memory for our result
    s << JAL;
    emit_method_ref(Object, _copy, s);
    s << endl;
    // ACC holds address to our result

    emit_fetch_int(T1, ACC, s);
    // Compute negation
    emit_neg(T1, T1, s);

    // Save it to our new object
    emit_store_int(T1, ACC, s);
}

void lt_class::code(ostream &s, CgenClassTable* cgentable) {
    int end_label = GLOBAL_LABEL_CTR++;
    // Save temporaries to the stack
    emit_push(S1,s);

    e1->code(s, cgentable);
    emit_move(S1, ACC, s);

    e2->code(s, cgentable);
    // ACC ($a0) has result address.

    // Get the actual integers to compare:
    emit_fetch_int(T1, S1, s);
    emit_fetch_int(T2, ACC, s);

    // Compute blt
    emit_load_bool(ACC, truebool, s);
    emit_blt(T1, T2, end_label, s);
    emit_load_bool(ACC, falsebool, s);
    emit_label_def(end_label, s);

    // Restore temporaries from the stack
    emit_pop(S1, s);
}

void eq_class::code(ostream &s, CgenClassTable* cgentable) {
    int end_label = GLOBAL_LABEL_CTR++;
    // Save temporaries to the stack
    emit_push(T1,s);

    e1->code(s, cgentable);
    emit_move(T1, ACC, s);
    // T1 has the addr of the first argument

    e2->code(s, cgentable);
    emit_move(T2, ACC, s);
    // T2 has the addr of the second argument

    // Compute beq
    emit_load_bool(ACC, truebool, s);
    // Compares the addresses of the arguments,
    // if same, then done, else, check for equality
    emit_beq(T1, T2, end_label, s);
    emit_load_bool(A1, falsebool, s);
    // perform the equality test provided by the runtime
    // Returns A0 if T1 == T2, else A1. Return value in A0
    emit_jal("equality_test", s);
    emit_label_def(end_label, s);

    // Restore temporaries from the stack
    emit_pop(T1, s);
}

void leq_class::code(ostream &s, CgenClassTable* cgentable) {
    int end_label = GLOBAL_LABEL_CTR++;
    // Push temporaries to the stack
    emit_push(S1,s);

    e1->code(s, cgentable);
    emit_move(S1, ACC, s);

    e2->code(s, cgentable);
    // ACC ($a0) has result address.

    // Get the actual integers to compare:
    emit_fetch_int(T1, S1, s);
    emit_fetch_int(T2, ACC, s);

    // Compute bleq
    emit_load_bool(ACC, truebool, s);
    emit_bleq(T1, T2, end_label, s);
    emit_load_bool(ACC, falsebool, s);
    emit_label_def(end_label, s);

    // Restore temporaries from the stack
    emit_pop(S1, s);
}

void comp_class::code(ostream &s, CgenClassTable* cgentable) {
    int end_label = GLOBAL_LABEL_CTR++;
    e1->code(s, cgentable);
    // ACC holds address to our result: BOOL obj

    // Bool value is at same offset as int value in bool proto
    emit_fetch_int(T1, ACC, s);

    // Compute complement
    emit_load_bool(ACC, truebool, s);
    emit_beqz(T1, end_label, s);
    emit_load_bool(ACC, falsebool, s);
    emit_label_def(end_label, s);
}

void int_const_class::code(ostream& s, CgenClassTable* cgentable)
{
    //
    // Need to be sure we have an IntEntry *, not an arbitrary Symbol
    //
    emit_load_int(ACC,inttable.lookup_string(token->get_string()),s);
}

void string_const_class::code(ostream& s, CgenClassTable* cgentable)
{
    emit_load_string(ACC,stringtable.lookup_string(token->get_string()),s);
}

void bool_const_class::code(ostream& s, CgenClassTable* cgentable)
{
    emit_load_bool(ACC, BoolConst(val), s);
}

void new__class::code(ostream &s, CgenClassTable* cgentable) {
    if(type_name == SELF_TYPE){
        // actually need to get the type from s0 reference, from the classtag
        emit_push(S1, s);
        // get the class object table (where the ref to the proto is stored)
        emit_load_address(T1, CLASSOBJTAB, s);
        // get the class object tag from the self object
        emit_load(S1, 0, SELF, s);
        // Multiply this value by 8 (2 words) via a logical shift
        emit_sll(S1, S1, 3, s);
        // Index into the class object table at the corresponding protobj location
        emit_addu(T1, T1, S1, s);
        // Get the protobj location address into S1, so it is saved if Object.copy modifies it
        emit_move(S1, T1, s);
        // Load the proto object address
        emit_load(ACC, 0, S1, s);

        // Now we have the protobj to copy in ACC
        // Allocate it:
        s << JAL; emit_method_ref(Object, _copy, s); s << endl;
        // get the object init address: one offset from the protobj ref (should have been restores by Object.copy)
        emit_load(T1, 1, S1, s);
        // Initialize it:
        emit_jalr(T1, s);

        emit_pop(S1, s);
        // DONE
    }else{
        emit_partial_load_address(ACC, s); emit_protobj_ref(type_name ,s); s << endl;
        // Now we have the protobj to copy in ACC
        // Allocate it:
        s << JAL; emit_method_ref(Object, _copy, s); s << endl;
        // Initialize it:
        s << JAL; emit_init_ref(type_name, s); s << endl;
        // DONE
    }
}

void isvoid_class::code(ostream &s, CgenClassTable* cgentable) {
    int end_label = GLOBAL_LABEL_CTR++;
    e1->code(s, cgentable);
    emit_move(T1, ACC, s);
    // T1 holds the address of the result of the expression
    // Is this just zero when void?
    // If so: Check ACC to see if it is zero
    emit_load_bool(ACC, truebool, s);
    emit_beqz(T1, end_label, s);
    emit_load_bool(ACC, falsebool, s);
    emit_label_def(end_label, s);
}

void no_expr_class::code(ostream &s, CgenClassTable* cgentable) {
}

void object_class::code(ostream &s, CgenClassTable* cgentable) {
    if(cgen_debug) printf("# Object: resolved address (object)\n");
    if(cgen_debug) dump_pair_container();
    if(name == self){
        // TODO: emit code to store ref to self in ACC?
        emit_move(ACC, SELF, s);
        if(cgen_debug) printf("# Object: end resolved address (self)\n");
        return;
    }
    if(cgen_debug) printf("# Object: looking up %s in Symbol Table\n", name->get_string());
    if(cgen_debug) cerr << "# Object: looking up "<< name <<" in Symbol Table\n";
    if(cgen_debug) cgentable->objectST.dump();

    auto* lookup = cgentable->objectST.lookup(name);
    assert(lookup != NULL);
    emit_load(ACC, lookup->second, lookup->first, s);
    if(cgen_debug) printf("# Object: end resolved address (object)\n");
}
