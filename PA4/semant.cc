#include "semant.h"
#include "utilities.h"
#include <map>
#include <set>
#include <sstream>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>

extern int semant_debug;
extern char *curr_filename;

static bool DEBUG_MODE = false;
// static bool DEBUG_MODE = true;
static std::ostringstream nop_sstream;
static std::ostream &log = DEBUG_MODE ? std::cout : nop_sstream;

// ensure each class symbol has its own symbol table
typedef SymbolTable<Symbol, method_class> MethodTable;
static std::map<Symbol, MethodTable> method_table;
static SymbolTable<Symbol, Symbol> attr_table;
static Class_ curr_class = nullptr;
ClassTable *classtable;

//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
static Symbol arg, arg2, Bool, concat, cool_abort, copy, Int, in_int, in_string, IO, length, Main, main_meth, No_class,
    No_type, Object, out_int, out_string, prim_slot, self, SELF_TYPE, Str, str_field, substr, type_name, val;
//
// Initializing the predefined symbols.
//
static void initialize_constants(void) {
  arg = idtable.add_string("arg");
  arg2 = idtable.add_string("arg2");
  Bool = idtable.add_string("Bool");
  concat = idtable.add_string("concat");
  cool_abort = idtable.add_string("abort");
  copy = idtable.add_string("copy");
  Int = idtable.add_string("Int");
  in_int = idtable.add_string("in_int");
  in_string = idtable.add_string("in_string");
  IO = idtable.add_string("IO");
  length = idtable.add_string("length");
  Main = idtable.add_string("Main");
  main_meth = idtable.add_string("main");
  //   _no_class is a symbol that can't be the name of any
  //   user-defined class.
  No_class = idtable.add_string("_no_class");
  No_type = idtable.add_string("_no_type");
  Object = idtable.add_string("Object");
  out_int = idtable.add_string("out_int");
  out_string = idtable.add_string("out_string");
  prim_slot = idtable.add_string("_prim_slot");
  self = idtable.add_string("self");
  SELF_TYPE = idtable.add_string("SELF_TYPE");
  Str = idtable.add_string("String");
  str_field = idtable.add_string("_str_field");
  substr = idtable.add_string("substr");
  type_name = idtable.add_string("type_name");
  val = idtable.add_string("_val");
}

inline bool is_basic_symbol_class(Symbol my_symbol) {
  return (my_symbol == Int || my_symbol == Str || my_symbol == SELF_TYPE || my_symbol == Bool);
}

ClassTable::ClassTable(Classes classes) : semant_errors(0), error_stream(cerr) {

  /* Fill this in */
  install_basic_classes();
  log << "Now check in the class table" << endl;
  /* class_map : symbol -> class */
  for (int i = classes->first(); classes->more(i); i = classes->next(i)) {
    Class_ class_node = classes->nth(i);
    /*
     *  Check redefined class
     */
    if (class_node->get_name() == SELF_TYPE) {
      semant_error(class_node) << "SELF_TYPE redefined!\n";
    }

    // class cannot be declared before
    if (class_map.find(class_node->get_name()) == class_map.end()) {
      class_map.insert(std::make_pair(class_node->get_name(), class_node));
    } else {
      semant_error(class_node) << "Class " << class_node->get_name() << "redefined!\n";
      return;
    }
  }
  if (class_map.find(Main) == class_map.end()) {
    semant_error() << "Class Main is not defined.\n";
  } else {
    Class_ main_class = class_map[Main];
    bool has_main_method = false;
    Features features = main_class->get_features();
    for (int i = features->first(); features->more(i); i = features->next(i)) {
      Feature feature = features->nth(i);
      if (feature->is_method() && static_cast<method_class *>(feature)->get_name() == main_meth)
        has_main_method = true;
    }
    if (!has_main_method)
      semant_error(main_class) << "No main method in Main class.\n";
  }
 
  for (int i = classes->first(); classes->more(i); i = classes->next(i)) {
    Class_ class_node = classes->nth(i);

    Symbol parent = class_node->get_parent();
    if (parent == Object)
      continue;
    while (parent != Object) {
      if (is_basic_symbol_class(parent)) {
        semant_error(class_node) << "Can't inherit from basic class.\n";
        return;
      }
      if (parent == class_node->get_name()) {
        semant_error(class_node) << "Cycle inheritance found.\n";
        return;
      }
      if (class_map.find(parent) == class_map.end()) {
        semant_error(class_node) << "Fail to find parent class definition.\n";
        return;
      }
      class_node = class_map[parent];
      parent = class_node->get_parent();
    }
  }
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
      class_(Object, No_class,
             append_Features(append_Features(single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
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
      class_(IO, Object,
             append_Features(
                 append_Features(
                     append_Features(
                         single_Features(method(out_string, single_Formals(formal(arg, Str)), SELF_TYPE, no_expr())),
                         single_Features(method(out_int, single_Formals(formal(arg, Int)), SELF_TYPE, no_expr()))),
                     single_Features(method(in_string, nil_Formals(), Str, no_expr()))),
                 single_Features(method(in_int, nil_Formals(), Int, no_expr()))),
             filename);

  //
  // The Int class has no methods and only a single attribute, the
  // "val" for the integer.
  //
  Class_ Int_class = class_(Int, Object, single_Features(attr(val, prim_slot, no_expr())), filename);

  //
  // Bool also has only the "val" slot.
  //
  Class_ Bool_class = class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())), filename);

  //
  // The class Str has a number of slots and operations:
  //       val                                  the length of the string
  //       str_field                            the string itself
  //       length() : Int                       returns length of the string
  //       concat(arg: Str) : Str               performs string concatenation
  //       substr(arg: Int, arg2: Int): Str     substring selection
  //
  Class_ Str_class = class_(
      Str, Object,
      append_Features(
          append_Features(append_Features(append_Features(single_Features(attr(val, Int, no_expr())),
                                                          single_Features(attr(str_field, prim_slot, no_expr()))),
                                          single_Features(method(length, nil_Formals(), Int, no_expr()))),
                          single_Features(method(concat, single_Formals(formal(arg, Str)), Str, no_expr()))),
          single_Features(method(substr,
                                 append_Formals(single_Formals(formal(arg, Int)), single_Formals(formal(arg2, Int))),
                                 Str, no_expr()))),
      filename);

  class_map.insert(std::make_pair(Object, Object_class));
  class_map.insert(std::make_pair(IO, IO_class));
  class_map.insert(std::make_pair(Int, Int_class));
  class_map.insert(std::make_pair(Bool, Bool_class));
  class_map.insert(std::make_pair(Str, Str_class));
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

ostream &ClassTable::semant_error(Class_ c) { return semant_error(c->get_filename(), c); }

ostream &ClassTable::semant_error(Symbol filename, tree_node *t) {
  error_stream << filename << ":" << t->get_line_number() << ": ";
  return semant_error();
}

ostream &ClassTable::semant_error() {
  semant_errors++;
  return error_stream;
}

void build_method_table(ClassTable *classtable) {
  // Build method environment
  for (ClassTable::iter iter = classtable->class_map.begin(); iter != classtable->class_map.end(); ++iter) {
    log << "class " << iter->first << ":\n";

    Symbol class_name = iter->first;
    method_table[class_name].enterscope();
    Features class_features = classtable->class_map[class_name]->get_features();
    for (int i = class_features->first(); class_features->more(i); i = class_features->next(i)) {
      class_features->nth(i)->add_method_to_table(class_name);
    }
  }
}

bool check_method_table(ClassTable *classtable) {
  for (ClassTable::iter iter = classtable->class_map.begin(); iter != classtable->class_map.end(); ++iter) {
    Symbol class_name = iter->first;
    curr_class = iter->second;
    // log << "Checking " << class_name << endl;
    if (is_basic_symbol_class(class_name))
      continue;
    // log << " Not basic class, checking\n";
    // Features class_features = classtable->class_map[class_name]->get_features();
    Features class_features = curr_class->get_features();

    ClassTable::chain chains = classtable->get_inheritance_chain(curr_class->get_name());
    // chains.push_back(curr_class);
    // log << "Gain chains successfully\n";
    for (int i = class_features->first(); class_features->more(i); i = class_features->next(i)) {
      Feature class_feature = class_features->nth(i);
      // skip attr
      if (!class_feature->is_method())
        continue;
      method_class *class_method = (method_class *)class_feature;

      Formals method_formals = class_method->get_formals();
      // verify each method signature formals

      // iter each parent class's method sig
      for (ClassTable::chain_iter iter = chains.begin(); iter != chains.end(); ++iter) {
        Class_ parent_class = *iter;
        // log << "   iter sig at " << parent_class << endl;
        Features parent_features = parent_class->get_features();
        method_class *method_sig = method_table[parent_class->get_name()].lookup(class_method->get_name());
        if (method_sig) {
          Formals parent_formals = method_sig->get_formals();
          // iter formals to ensure each type is matched
          int cur_idx = method_formals->first(), p_idx = parent_formals->first();
          for (; method_formals->more(cur_idx) && parent_formals->more(p_idx);
               cur_idx = method_formals->next(cur_idx), p_idx = parent_formals->next(p_idx)) {
            // not matched sig
            if (method_formals->nth(cur_idx)->get_type() != parent_formals->nth(p_idx)->get_type()) {
              classtable->semant_error(curr_class) << " Inconsistent type of Method formals.\n";
            }
          }

          if (method_formals->more(cur_idx) || parent_formals->more(p_idx)) {
            classtable->semant_error(curr_class) << " Inconsistent number of Method formals .\n";
          }
        }
      }
    }

    log << "Now installing attr\n";
    // ClassTable::chain chains = classtable->get_inheritance_chain(curr_class->get_name());

    // add class attribute to its own attr table
    for (ClassTable::chain_iter it = chains.begin(); it != chains.end(); ++it) {
      curr_class = *it;
      Features parent_features = (*it)->get_features();
      attr_table.enterscope();
      for (int idx = parent_features->first(); parent_features->more(idx); idx = parent_features->next(idx)) {
        Feature parent_feature = parent_features->nth(idx);
        parent_feature->add_attr_to_table((*it)->get_name());
      }
    }

    // log << "Now check the remaining attr in the class\n";

    // iter here: the outer iterater of classes
    curr_class = iter->second;
    // check its own feature type
    for (int i = class_features->first(); class_features->more(i); i = class_features->next(i)) {
      class_features->nth(i)->check_type();
    }

    for (int j = 0; j < chains.size(); ++j) {
      attr_table.exitscope();
    }
  }
}

void method_class::add_method_to_table(Symbol class_name) {
  log << "Adding method " << name << std::endl;
  method_table[class_name].addid(name, new method_class(copy_Symbol(name), formals->copy_list(),
                                                        copy_Symbol(return_type), expr->copy_Expression()));
}
void attr_class::add_method_to_table(Symbol class_name) {}
void attr_class::add_attr_to_table(Symbol class_name) {
  log << "Adding attr " << name << endl;

  if (name == self) {
    classtable->semant_error(curr_class) << "Self shoudn't be the attribute in class " << curr_class->get_name()
                                         << endl;
  }
  if (attr_table.lookup(name)) {
    classtable->semant_error(curr_class) << "Attribute " << name << " redefined." << endl;
    return;
  }

  attr_table.addid(name, new Symbol(type_decl));
}

ClassTable::chain ClassTable::get_inheritance_chain(Symbol target_name) {
  /*if (target_class == SELF_TYPE)
    target_class = curr_class->get_name();*/
  if (this->class_map.find(target_name) == this->class_map.end()) {

    log << "[FATAL ERROR] " << target_name << " undefined.\n";
    target_name = curr_class->get_name();
  }
  ClassTable::chain chains;
  Class_ target_class = this->class_map[target_name];
  while (target_class && target_class->get_name() != Object) {
    log << "  Pushing " << target_class->get_name() << endl;
    chains.push_back(target_class);
    target_class = this->class_map[target_class->get_parent()];
    if (!target_class)
      break;
  }
  chains.push_back(this->class_map[Object]);
  return chains;
}

/**
 *  calculate Least Common Ancestors of two symbol node.
 *    note:
 *      as the efficiency dosn't count here and this DAG is actually a tree rather than a forest,
 *         we don't use Tarjan offline, RMQ or other fast algorithm.
 *  Here I just use online calculation and compare these chains from the Object Class.
 */
Symbol ClassTable::LCA(Symbol u, Symbol v) {
  ClassTable::chain chain_u = classtable->get_inheritance_chain(u);
  ClassTable::chain chain_v = classtable->get_inheritance_chain(v);
  // std::vector<Symbol>::reverse_iterator
  std::vector<Class_>::reverse_iterator iter_u = chain_u.rbegin();
  std::vector<Class_>::reverse_iterator iter_v = chain_v.rbegin();

  Symbol cache_lca = Object;
  while (iter_u != chain_u.rend() && iter_v != chain_v.rend()) {
    if (*iter_u == *iter_v) {
      cache_lca = (*iter_u)->get_name();
    } else {
      return cache_lca;
    }
    ++iter_u;
    ++iter_v;
  }
  return cache_lca;
}

bool _common_type_lookup(Symbol target_type) {
  if (target_type == SELF_TYPE)
    return true;
  if (classtable->class_map.find(target_type) == classtable->class_map.end()) {
    classtable->semant_error(curr_class) << "Type " << target_type << " undefined." << endl;
    return false;
  }
  return true;
}

void method_class::check_type() {
  _common_type_lookup(return_type);
  attr_table.enterscope();
  std::set<Symbol> used_names;
  /**
   *  we check the following things:
   * 1. formals declared params names are not redefined among each other.
   * 2.
   *
   */
  for (int i = formals->first(); formals->more(i); i = formals->next(i)) {
    Symbol name = formals->nth(i)->get_name();
    Symbol type = formals->nth(i)->get_type();

    if (used_names.find(name) != used_names.end()) {
      classtable->semant_error(curr_class) << "Formal name redefined. " << endl;
    } else {
      used_names.insert(name);
    }

    _common_type_lookup(type);
    if (name == self || type == SELF_TYPE) {
      classtable->semant_error(curr_class) << "Self shouldn't in formal defination" << endl;
    }
    attr_table.addid(name, new Symbol(type));
  }

  Symbol expr_type = expr->check_expr_type();
  if (expr_type != No_type && !classtable->is_parent_of(return_type, expr_type)) {
    classtable->semant_error(curr_class) << "Inconsistent type between return value and expr value : " << return_type
                                         << " (return type) and " << expr_type << " (expr type) " << endl;
  }
  attr_table.exitscope();
}

// check type recursively
void attr_class::check_type() { init->check_expr_type(); }

bool ClassTable::is_parent_of(Symbol parent, Symbol child) {
  if (parent == SELF_TYPE) {
    return child == SELF_TYPE;
  }
  if (child == SELF_TYPE) {
    child = curr_class->get_name();
  }
  while (child != No_class && class_map.find(child) != class_map.end()) {
    if (child == parent)
      return true;
    child = class_map[child]->get_parent();
  }
  return false;
}

Symbol assign_class::check_expr_type() {
  Symbol *l_type = attr_table.lookup(name);
  Symbol r_type = expr->check_expr_type();
  if (!l_type) {
    classtable->semant_error(curr_class) << "Undefined Left value type " << name << "\n";
    type = Object;
    return Object;
  }
  if (*l_type == SELF_TYPE) {
    classtable->semant_error(curr_class) << "Cann't assign to self." << name << "\n";
    type = Object;
    return Object;
  }
  if (!classtable->is_parent_of(*l_type, r_type)) {
    classtable->semant_error(curr_class) << "Inconsistent type between left value and right value (" << *l_type << " , "
                                         << r_type << " )\n";
    type = Object;
    return Object;
  }
  type = r_type;
  return r_type;
}

// return method pointer if check fine, nullptr if error occurs, set is_error true if method param check failed
method_class *_common_dispatch_checker(Symbol type_name, Expressions actual, Symbol method_name, bool &is_error) {
  bool error = false;

  // search the method defination
  // assert : this chains contains class itself!
  ClassTable::chain chains = classtable->get_inheritance_chain(type_name);
  method_class *method = nullptr;
  for (ClassTable::chain_iter iter = chains.begin(); iter != chains.end(); ++iter) {
    if ((method = method_table[(*iter)->get_name()].lookup(method_name)) != NULL) {
      break;
    }
  }
  if (!method) {
    classtable->semant_error(curr_class) << "Undefined method name: " << method_name << "\n";
    error = true;
  } else {
    // verify the defined params and the actual params
    for (int i = actual->first(); actual->more(i); i = actual->next(i)) {
      Symbol actual_type = actual->nth(i)->check_expr_type();
      Symbol formal_type = method->get_formals()->nth(i)->get_type();
      if (!classtable->is_parent_of(formal_type, actual_type)) {
        classtable->semant_error(curr_class) << "Inconsistent params type between " << actual_type << " (actual) and "
                                             << formal_type << " (defined) .\n";
        error = true;
      }
    }
  }
  is_error = is_error | error;
  return method;
}

Symbol static_dispatch_class::check_expr_type() {
  Symbol expr_type = expr->check_expr_type();
  bool error = false;
  if (!classtable->is_parent_of(type_name, expr_type)) {
    classtable->semant_error(curr_class) << "Inconsistent type between class and expr\n";
    type = Object;
    error = true;
    return Object;
  }
  method_class *method = _common_dispatch_checker(type_name, actual, name, error);
  if (error) {
    type = Object;
  } else {
    type = method->get_ret_type();
    if (type == SELF_TYPE)
      type = expr_type;
  }

  return type;
}

Symbol dispatch_class::check_expr_type() {
  Symbol expr_type = expr->check_expr_type();
  bool error = false;

  method_class *method = _common_dispatch_checker(expr_type, actual, name, error);
  if (error) {
    type = Object;
  } else {
    if (method)
      type = method->get_ret_type();
    if (type == SELF_TYPE)
      type = expr_type;
  }
  return type;
}

Symbol cond_class::check_expr_type() {
  if (pred->check_expr_type() != Bool) {
    classtable->semant_error(curr_class) << "Wrong type of pred expressions. Expect bool.\n";
  }

  Symbol then_type = then_exp->check_expr_type();
  Symbol else_type = else_exp->check_expr_type();
  if (else_type == No_type) {
    // no else branch
    type = then_type;
  } else {
    type = classtable->LCA(then_type, else_type);
  }
  return type;
}
Symbol loop_class::check_expr_type() {
  if (pred->check_expr_type() != Bool) {
    classtable->semant_error(curr_class) << "Wrong type of pred expressions. Expect bool.\n";
  }
  body->check_expr_type();
  type = Object;
  return type;
}
Symbol typcase_class::check_expr_type() {
  Symbol expr_type = expr->check_expr_type();
  Case branch;
  std::vector<Symbol> branch_types;
  std::vector<Symbol> branch_type_decls;

  for (int i = cases->first(); cases->more(i); i = cases->next(i)) {
    branch = cases->nth(i);
    Symbol branch_type = branch->check_branch_type();
    branch_types.push_back(branch_type);
    branch_type_decls.push_back((static_cast<branch_class *>(branch))->get_type_decl());
  }

  for (int i = 0; i < branch_types.size() - 1; ++i) {
    for (int j = i + 1; j < branch_types.size(); ++j) {
      if (branch_type_decls[i] == branch_type_decls[j]) {
        classtable->semant_error(curr_class) << "Same type found among the cases.\n";
      }
    }
  }

  type = branch_types[0];
  for (int i = 1; i < branch_types.size(); ++i) {
    type = classtable->LCA(type, branch_types[i]);
  }
  return type;
}

Symbol block_class::check_expr_type() {
  for (int i = body->first(); body->more(i); i = body->next(i)) {
    type = body->nth(i)->check_expr_type();
  }
  return type;
}
Symbol let_class::check_expr_type() {
  if (identifier == self) {
    classtable->semant_error(curr_class) << "Binding self in let is unexpected.\n";
  }

  // add a new id into the environment
  attr_table.enterscope();
  attr_table.addid(identifier, new Symbol(type_decl));

  Symbol init_type = init->check_expr_type();

  // if there is an initialization expression
  if (init_type != No_type) {
    if (!classtable->is_parent_of(type_decl, init_type)) {
      classtable->semant_error(curr_class) << "Inconsistent type bewteen init value and definition.\n";
    }
  }

  type = body->check_expr_type();
  attr_table.exitscope();
  return type;
}

Symbol _common_operator_checker(Expression e1, Expression e2, Symbol default_type = Int) {
  Symbol type;
  Symbol e1_type = e1->check_expr_type();
  Symbol e2_type = e2->check_expr_type();
  if (e1_type != Int || e2_type != Int) {
    classtable->semant_error(curr_class) << "Each Operator should be Int type.\n";
    type = Object;
  } else {
    type = default_type; // Int;
  }
  return type;
}

Symbol plus_class::check_expr_type() {
  type = _common_operator_checker(e1, e2);
  return type;
}
Symbol sub_class::check_expr_type() {
  type = _common_operator_checker(e1, e2);
  return type;
}
Symbol mul_class::check_expr_type() {
  type = _common_operator_checker(e1, e2);
  return type;
}
Symbol divide_class::check_expr_type() {
  type = _common_operator_checker(e1, e2);
  return type;
}
Symbol neg_class::check_expr_type() {
  if (e1->check_expr_type() != Int) {
    classtable->semant_error(curr_class) << "Each Operator should be Int type." << endl;
    type = Object;
  } else {
    type = Int;
  }
  return type;
}
Symbol lt_class::check_expr_type() {
  type = _common_operator_checker(e1, e2, Bool);
  return type;
}

// basic types shouldn't be cross compared
Symbol eq_class::check_expr_type() {
  Symbol e1_type = e1->check_expr_type();
  Symbol e2_type = e2->check_expr_type();
  if (is_basic_symbol_class(e1_type) || is_basic_symbol_class(e2_type)) {
    if (e1_type != e2_type) {
      classtable->semant_error(curr_class) << "Basic types shouldn't be cross compared.\n" << endl;
      type = Object;
    } else {
      type = Bool;
    }
  } else {
    type = Bool;
  }
  return type;
}
Symbol leq_class::check_expr_type() {
  type = _common_operator_checker(e1, e2, Bool);
  return type;
}
Symbol comp_class::check_expr_type() {
  if (e1->check_expr_type() != Bool) {
    classtable->semant_error(curr_class) << "Comp should use Bool type value." << endl;
    type = Object;
  } else {
    type = Bool;
  }
  return type;
}
Symbol int_const_class::check_expr_type() {
  type = Int;
  return type;
}
Symbol bool_const_class::check_expr_type() {
  type = Bool;
  return type;
}
Symbol string_const_class::check_expr_type() {
  type = Str;
  return type;
}
Symbol new__class::check_expr_type() {
  _common_type_lookup(type_name);
  type = type_name;
  return type;
}
Symbol isvoid_class::check_expr_type() {
  e1->check_expr_type();
  type = Bool;
  return type;
}
Symbol no_expr_class::check_expr_type() { return No_type; }
Symbol object_class::check_expr_type() {
  if (name == self) {
    type = SELF_TYPE;
    return type;
  }

  Symbol *found_type = attr_table.lookup(name);
  if (found_type == NULL) {
    classtable->semant_error(curr_class) << "Object " << name << " undefined" << endl;
    type = Object;
  } else {
    type = *found_type;
  }
  return type;
}

Symbol branch_class::check_branch_type() {
  attr_table.enterscope();

  attr_table.addid(name, new Symbol(type_decl));
  Symbol type = expr->check_expr_type();

  attr_table.exitscope();

  return type;
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

void program_class::semant() {
  initialize_constants();

  /* ClassTable constructor may do some semantic analysis */
  classtable = new ClassTable(classes);

  /* some semantic analysis code may go here */

  if (classtable->errors()) {
    cerr << "Compilation halted due to static semantic errors." << endl;
    exit(1);
  }

  // ----------------- Build & Verify Type Environment -----------------
  build_method_table(classtable);
  check_method_table(classtable);

  if (classtable->errors()) {
    cerr << "Compilation halted due to static semantic errors finally." << endl;
    exit(1);
  }
}