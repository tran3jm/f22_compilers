/**
 * @file p3-analysis.c
 * @brief Compiler phase 3: static analysis
 *
 * @authors Joselyne Tran and Alden Geipel
 */
#include "p3-analysis.h"
void AnalysisVisitor_literal_previsit(NodeVisitor *visitor, ASTNode *node);
void AnalysisVisitor_location_previsit(NodeVisitor *visitor, ASTNode *node);
void AnalysisVisitor_vardecl_postvisit(NodeVisitor *visitor, ASTNode *node);
void AnalysisVisitor_location_postvisit(NodeVisitor *visitor, ASTNode *node);
void AnalysisVisitor_assign_postvisit(NodeVisitor *visitor, ASTNode *node);
void AnalysisVisitor_return_postvisit(NodeVisitor *visitor, ASTNode *node);
void AnalysisVisitor_break_continue_postvisit(NodeVisitor *visitor, ASTNode *node);
void AnalysisVisitor_conditional_postvisit(NodeVisitor *visitor, ASTNode *node);
void AnalysisVisitor_funcdecl_previsit(NodeVisitor *visitor, ASTNode *node);
void AnalysisVisitor_funcdecl_postvisit(NodeVisitor *visitor, ASTNode *node);
void AnalysisVisitor_loop_previsit(NodeVisitor *visitor, ASTNode *node);
void AnalysisVisitor_loop_postvisit(NodeVisitor *visitor, ASTNode *node);
void AnalysisVisitor_program_postvisit(NodeVisitor *visitor, ASTNode *node);
void AnalysisVisitor_block_previsit(NodeVisitor *visitor, ASTNode *node);
void AnalysisVisitor_program_previsit(NodeVisitor *visitor, ASTNode *node);
void AnalysisVisitor_block_previsit(NodeVisitor *visitor, ASTNode *node);
void AnalysisVisitor_unaryop_previsit(NodeVisitor *visitor, ASTNode *node);
void AnalysisVisitor_unaryop_postvisit(NodeVisitor *visitor, ASTNode *node);
void AnalysisVisitor_binaryop_previsit(NodeVisitor *visitor, ASTNode *node);
void AnalysisVisitor_binaryop_postvisit(NodeVisitor *visitor, ASTNode *node);
void AnalysisVisitor_funccall_previsit(NodeVisitor *visitor, ASTNode *node);
void AnalysisVisitor_funccall_postvisit(NodeVisitor *visitor, ASTNode *node);
void AnalysisVisitor_block_postvisit(NodeVisitor *visitor, ASTNode *node);

/**
 * @brief State/data for static analysis visitor
 */
typedef struct AnalysisData
{
    /**
     * @brief List of errors detected
     */
    ErrorList *errors;

    /* BOILERPLATE: TODO: add any new desired state information (and clean it up in AnalysisData_free) */
    char *funcName;
    bool isInLoop;
    SymbolList* currentSymbolList;
    int scopeNum;
    bool isInBlock;

} AnalysisData;

/**
 * @brief Allocate memory for analysis data
 *
 * @returns Pointer to allocated structure
 */
AnalysisData *AnalysisData_new()
{
    AnalysisData *data = (AnalysisData *)calloc(1, sizeof(AnalysisData));
    CHECK_MALLOC_PTR(data);
    data->errors = ErrorList_new();
    data->funcName = ""; // current function vistor is looking at
    data->isInLoop = false; // checking if visitor is currently lookiong at a loop
    data->currentSymbolList = NULL; // current list symbols
    data->scopeNum = 0; // scope for dups
    data->isInBlock = false;

    return data;
}

/**
 * @brief Deallocate memory for analysis data
 *
 * @param data Pointer to the structure to be deallocated
 */
void AnalysisData_free(AnalysisData *data)
{
    /* free everything in data that is allocated on the heap except the error
     * list; it needs to be returned after the analysis is complete */

    /* free "data" itself */
    free(data);
}

/**
 * @brief Macro for more convenient access to the data inside a @ref AnalysisVisitor
 * data structure
 */
#define DATA ((AnalysisData *)visitor->data)

/**
 * @brief Macro for more convenient access to the error list inside a
 * @ref AnalysisVisitor data structure
 */
#define ERROR_LIST (((AnalysisData *)visitor->data)->errors)

/**
 * @brief Wrapper for @ref lookup_symbol that reports an error if the symbol isn't found
 *
 * @param visitor Visitor with the error list for reporting
 * @param node AST node to begin the search at
 * @param name Name of symbol to find
 * @returns The @ref Symbol if found, otherwise @c NULL
 */
Symbol *lookup_symbol_with_reporting(NodeVisitor *visitor, ASTNode *node, const char *name)
{
    Symbol *symbol = lookup_symbol(node, name);
    if (symbol == NULL)
    {
        ErrorList_printf(ERROR_LIST, "Symbol '%s' undefined on line %d", name, node->source_line);
    }
    return symbol;
}

/**
 * @brief Macro for shorter storing of the inferred @c type attribute
 */
#define SET_INFERRED_TYPE(T) ASTNode_set_printable_attribute(node, "type", (void *)(T), \
                                                             type_attr_print, dummy_free)

/**
 * @brief Macro for shorter retrieval of the inferred @c type attribute
 */
#define GET_INFERRED_TYPE(N) (DecafType) ASTNode_get_attribute(N, "type")

ErrorList *analyze(ASTNode *tree)
{
        /* allocate analysis structures */
        NodeVisitor *v = NodeVisitor_new();
        v->data = (void *)AnalysisData_new();
        v->dtor = (Destructor)AnalysisData_free;

        /* perform analysis, save error list, clean up, and return errors */
        ErrorList *errors = ((AnalysisData *)v->data)->errors;
        if (tree) {
            /* BOILERPLATE: TODO: register analysis callbacks */
            v->previsit_program = AnalysisVisitor_program_previsit;
            v->previsit_block= AnalysisVisitor_block_previsit;
            v->previsit_location = AnalysisVisitor_location_previsit;
            v->previsit_literal = AnalysisVisitor_literal_previsit;
            v->previsit_funcdecl = AnalysisVisitor_funcdecl_previsit;
            v->previsit_unaryop = AnalysisVisitor_unaryop_previsit;
            v->previsit_binaryop = AnalysisVisitor_binaryop_previsit;
            v->previsit_funccall = AnalysisVisitor_funccall_previsit;

            /* Post visit methods */
            v->postvisit_location = AnalysisVisitor_location_postvisit;
            v->postvisit_program = AnalysisVisitor_program_postvisit;
            v->postvisit_vardecl = AnalysisVisitor_vardecl_postvisit;
            v->postvisit_conditional = AnalysisVisitor_conditional_postvisit;
            v->postvisit_assignment = AnalysisVisitor_assign_postvisit;
            v->postvisit_return = AnalysisVisitor_return_postvisit;
            v->postvisit_break = AnalysisVisitor_break_continue_postvisit;
            v->postvisit_continue = AnalysisVisitor_break_continue_postvisit;
            v->postvisit_funcdecl = AnalysisVisitor_funcdecl_postvisit;
            v->postvisit_unaryop = AnalysisVisitor_unaryop_postvisit;
            v->postvisit_binaryop = AnalysisVisitor_binaryop_postvisit;
            v->postvisit_funccall = AnalysisVisitor_funccall_postvisit;
            v->postvisit_block = AnalysisVisitor_block_postvisit;

            NodeVisitor_traverse(v, tree);
        } else {
            ErrorList_printf(errors, "No tree");
        }
        NodeVisitor_free(v);
    return errors;
}

/**
 * @brief Helper method to find duplicate symbols
 * 
 * @param symbols Pointer to symbol table
 * @param name Name of symbol trying to find dup of
 * @returns If found dup symbol
 */
bool has_dup_symbol(SymbolList* symbols, char* name) {

    /* How many time that node is found in symbol table*/
    int count = 0;
    FOR_EACH(Symbol*, sym, symbols) {
        if (strncmp(name, sym->name, MAX_ID_LEN) == 0) {
            count += 1;
        }
    }

    /* Returns if there is dups */
    return count > 1;

}

/*-----------------------------PRE-VISTOR PROGRAMS-----------------------------*/

/**
 * @brief Previsitor for program node.
 * 
 * @param vistor Nodevisitor for program
 * @param node Current node to look at
 */
void AnalysisVisitor_program_previsit(NodeVisitor *visitor, ASTNode *node)
{
    SymbolTable* st = ASTNode_get_attribute(node, "symbolTable");
    DATA->currentSymbolList = st->local_symbols;

    /* check through all global variables to find duplicates */
    FOR_EACH(ASTNode*, v, node->program.variables) {
        if (has_dup_symbol(DATA->currentSymbolList, v->vardecl.name)) {
            ErrorList_printf(ERROR_LIST, "Duplicate symbols named \'%s\' scope on line %d",
                         v->vardecl.name, node->source_line);
            break;
        }
    }

    /* check through all functions to find duplicates */
    FOR_EACH(ASTNode*, f, node->program.functions) {
        if (has_dup_symbol(DATA->currentSymbolList, f->funcdecl.name)) {
            ErrorList_printf(ERROR_LIST, "Duplicate symbols named \'%s\' in scope started on line %d",
                         f->funcdecl.name, node->source_line);
            break;
        }
    }
    DATA->scopeNum = node->source_line;
}

/**
 * @brief Previsitor for function declaration node.
 * 
 * @param vistor Nodevisitor for function declaration
 * @param node Current node to look at
 */
void AnalysisVisitor_funcdecl_previsit(NodeVisitor *visitor, ASTNode *node)
{
    SymbolTable* st = ASTNode_get_attribute(node, "symbolTable");

    /* Setting DATA attributes to help during traversal */
    DATA->scopeNum = node->source_line; // scope of dup
    DATA->currentSymbolList = st->local_symbols; // current symbols we're looking at
    DATA->funcName = node->funcdecl.name; // current function visitor is in

    /* Checks if there is dups in parameters */
    FOR_EACH(Parameter*, p, node->funcdecl.parameters) {
        if (has_dup_symbol(DATA->currentSymbolList, p->name)) {
            ErrorList_printf(ERROR_LIST, "Duplicate symbols named \'%s\' scope on line %d",
                         p->name, node->source_line);
            break;
        }
    }
}

/**
 * @brief Previsitor for block node.
 * 
 * @param vistor Nodevisitor for block
 * @param node Current node to look at
 */
void AnalysisVisitor_block_previsit(NodeVisitor *visitor, ASTNode *node)
{
    DATA->isInBlock = true;
    SymbolTable* st = ASTNode_get_attribute(node, "symbolTable");
    DATA->currentSymbolList = st->local_symbols;

    /* check through all variables to find duplicates */
    FOR_EACH(ASTNode*, v, node->block.variables) {
        if (has_dup_symbol(DATA->currentSymbolList, v->vardecl.name)) {
            ErrorList_printf(ERROR_LIST, "Duplicate symbols named \'%s\' scope on line %d",
                         v->vardecl.name, node->source_line);
            break;
        }
    }
    DATA->scopeNum = node->source_line;
}

/**
 * @brief Previsit for location node.
 * 
 * @param vistor Nodevisitor for location
 * @param node Current node to look at
 */
void AnalysisVisitor_location_previsit(NodeVisitor *visitor, ASTNode *node)
{
    Symbol *sym = lookup_symbol_with_reporting(visitor, node, node->location.name);

    /* if location is found in symbol table, we set location type */
    if (sym)
    {
        SET_INFERRED_TYPE(sym->type);
    }
}

/**
 * @brief Previsit for literal node.
 * 
 * @param vistor Nodevisitor for literal
 * @param node Current node to look at
 */
void AnalysisVisitor_literal_previsit(NodeVisitor *visitor, ASTNode *node)
{
    SET_INFERRED_TYPE(node->literal.type);
}

/**
 * @brief Previsit for binary operator node.
 * 
 * @param vistor Nodevisitor for binaryop
 * @param node Current node to look at
 */
void AnalysisVisitor_binaryop_previsit(NodeVisitor *visitor, ASTNode *node)
{
    /* Setting type based on what operator it is */
    switch(node->binaryop.operator)
    {
        case OROP: case ANDOP: case EQOP: case NEQOP: case LTOP: case LEOP: case GEOP: case GTOP:
            SET_INFERRED_TYPE(BOOL);
            break;
        case ADDOP: case SUBOP: case MULOP: case DIVOP: case MODOP:
            SET_INFERRED_TYPE(INT);
            break;
    }
}

/**
 * @brief Previsit for uanryop node.
 * 
 * @param vistor Nodevisitor for uanryop
 * @param node Current node to look at
 */
void AnalysisVisitor_unaryop_previsit(NodeVisitor *visitor, ASTNode *node)
{

    /* Setting type of unaryop based on what operator it uses */
    switch(node->unaryop.operator)
    {
        case NEGOP:
            SET_INFERRED_TYPE(INT);
            break;
        case NOTOP:
            SET_INFERRED_TYPE(BOOL);
            break;
    }
}

/**
 * @brief Previsit for function call node.
 * 
 * @param vistor Nodevisitor for function call
 * @param node Current node to look at
 */
void AnalysisVisitor_funccall_previsit(NodeVisitor *visitor, ASTNode *node)
{
    
    Symbol* symbol = lookup_symbol(node, node->funccall.name);

    /* Checking if function exists in symbol table */
    if (symbol == NULL)
    {
        ErrorList_printf(ERROR_LIST, "Program does not contain \'%s\' function", node->funccall.name, node->source_line);
    
    /* Checking if trying to call a variable as a function*/
    } else if (symbol != NULL && symbol->symbol_type != FUNCTION_SYMBOL) {
        ErrorList_printf(ERROR_LIST, "Invalid call to non-function \'%s\' on line %d",
                         node->location.name,
                         node->source_line);
    }

    SET_INFERRED_TYPE(symbol->type);
}

/**
 * @brief Previsit for loop node.
 * 
 * @param vistor Nodevisitor for loop
 * @param node Current node to look at
 */
void AnalysisVisitor_loop_previsit(NodeVisitor *visitor, ASTNode *node)
{
    DATA->isInLoop = true;
}

/*-----------------------------POST-VISTOR PROGRAMS----------------------------*/

/**
 * @brief Postvisit for variable declaration node.
 * 
 * @param vistor Nodevisitor for function declaration
 * @param node Current node to look at
 */
void AnalysisVisitor_vardecl_postvisit(NodeVisitor *visitor, ASTNode *node)
{
    /* variable cannot be void */
    if (node->vardecl.type == VOID)
    {
        ErrorList_printf(ERROR_LIST, "Void variable '%s' on line %d", node->vardecl.name, node->source_line);
    }

    /* if variable is array, cannot have a size lower than 0 */
    if (node->vardecl.is_array && node->vardecl.array_length <= 0)
    {
        ErrorList_printf(ERROR_LIST, "Array \'%s\' on line %d must have positive non-zero length", node->vardecl.name, node->source_line);
    }

    /* array variable must be declared globally */
    else if (node->vardecl.is_array && DATA->isInBlock)
    {
        ErrorList_printf(ERROR_LIST, "Local variable \'%s\' on line %d cannot be an array", node->vardecl.name, node->source_line);
    }
    
}

/**
 * @brief Postvisit for location node.
 * 
 * @param vistor Nodevisitor for location
 * @param node Current node to look at
 */
void AnalysisVisitor_location_postvisit(NodeVisitor *visitor, ASTNode *node)
{
    Symbol* symbol = lookup_symbol(node, node->location.name);

    /* if location is NOT found in symbol table or if array and has no index to access */
    if (symbol != NULL && symbol->symbol_type == ARRAY_SYMBOL && node->location.index == NULL)
    {
        ErrorList_printf(ERROR_LIST, "Array \'%s\' accessed without index on line %d",
                         node->location.name,
                         node->source_line);

    /* functions cannot be locations */
    } else if (symbol != NULL && symbol->symbol_type == FUNCTION_SYMBOL) {
        ErrorList_printf(ERROR_LIST, "Function \'%s\' accessed as a variable on line %d",
                         node->location.name,
                         node->source_line);
    }
 
}

/**
 * @brief Postvisit for conditional node.
 * 
 * @param vistor Nodevisitor for conditional
 * @param node Current node to look at
 */
void AnalysisVisitor_conditional_postvisit(NodeVisitor *visitor, ASTNode *node)
{
     /* Checking if the conditrional's condition is a boolean */
    if (GET_INFERRED_TYPE(node->conditional.condition) != BOOL)
    {
        ErrorList_printf(ERROR_LIST, "Type mismatch: %s expected but %s found on line %d",
                         DecafType_to_string(BOOL),
                         DecafType_to_string(GET_INFERRED_TYPE(node->conditional.condition)),
                         node->source_line);
    }
}

/**
 * @brief Postvisit for program node.
 * 
 * @param vistor Nodevisitor for program
 * @param node Current node to look at
 */
void AnalysisVisitor_program_postvisit(NodeVisitor *visitor, ASTNode *node)
{
    /* Checking if main exist in program */
    if (lookup_symbol(node, "main") == NULL || lookup_symbol(node, "main")->symbol_type != FUNCTION_SYMBOL)
    {
        ErrorList_printf(ERROR_LIST, "Program does not contain \'main\' function", node->funcdecl.name, node->source_line);
    }

    else if (lookup_symbol(node, "main")->parameters->size != 0)
    {
        ErrorList_printf(ERROR_LIST, "\'main\' must take no parameters");
    }

    else if (lookup_symbol(node, "main")->type != INT)
    {
        ErrorList_printf(ERROR_LIST, "\'main\' must return an integer");
    }
}

/**
 * @brief Postvisit for assignment node.
 * 
 * @param vistor Nodevisitor for assignment
 * @param node Current node to look at
 */
void AnalysisVisitor_assign_postvisit(NodeVisitor *visitor, ASTNode *node)
{
    /* Making sure location is in the symbol table */
    if (lookup_symbol(node, node->assignment.location->location.name)) {
        
        /* Checking if assignment's location and value types match */
        if (GET_INFERRED_TYPE(node->assignment.location) != GET_INFERRED_TYPE(node->assignment.value))
        {
            ErrorList_printf(ERROR_LIST, "Type mismatch: %s is incompatible with %s on line %d",
                            DecafType_to_string((GET_INFERRED_TYPE(node->assignment.location))),
                            DecafType_to_string((GET_INFERRED_TYPE(node->assignment.value))),
                            node->source_line);
        }
    }
}

/**
 * @brief Postvisit for binary operator node.
 * 
 * @param vistor Nodevisitor for binaryop
 * @param node Current node to look at
 */
void AnalysisVisitor_binaryop_postvisit(NodeVisitor *visitor, ASTNode *node)
{
    switch(node->binaryop.operator)

    {
        /* || and && operators */
        case OROP: case ANDOP:

            /* Checking both left and right operands for different error messages */
            if (GET_INFERRED_TYPE(node->binaryop.left) != BOOL) 
            {
                ErrorList_printf(ERROR_LIST, "Type mismatch: %s expected but %s found on line %d",
                                DecafType_to_string(BOOL),
                                DecafType_to_string(GET_INFERRED_TYPE(node->binaryop.left)),
                                node->source_line);
            }

            if (GET_INFERRED_TYPE(node->binaryop.right) != BOOL) 
            {
                ErrorList_printf(ERROR_LIST, "Type mismatch: %s expected but %s found on line %d",
                                DecafType_to_string(BOOL),
                                DecafType_to_string(GET_INFERRED_TYPE(node->binaryop.right)),
                                node->source_line);
            }
            break;

        /* != and == operators */
        case EQOP: case NEQOP: 

            /* Checking if left and right operands have same type*/
            if (GET_INFERRED_TYPE(node->binaryop.left) != GET_INFERRED_TYPE(node->binaryop.right)) 
            {
                ErrorList_printf(ERROR_LIST, "Type mismatch: %s is incompatible with %s on line %d",
                                DecafType_to_string(GET_INFERRED_TYPE(node->binaryop.left)),
                                DecafType_to_string(GET_INFERRED_TYPE(node->binaryop.right)),
                                node->source_line);
            }
            break;

        /* <, <=, >=, >, +, -, *, /, and % oeprators */
        case LTOP: case LEOP: case GEOP: case GTOP: case ADDOP: case SUBOP: case MULOP: case DIVOP: case MODOP:
            
            /* Checking both left and right operands for different error messages */
            if (GET_INFERRED_TYPE(node->binaryop.left) != INT) 
            {
                ErrorList_printf(ERROR_LIST, "Type mismatch: %s expected but %s found on line %d",
                                DecafType_to_string(INT),
                                DecafType_to_string(GET_INFERRED_TYPE(node->binaryop.left)),
                                node->source_line);
            }

            if (GET_INFERRED_TYPE(node->binaryop.right) != INT) 
            {
                ErrorList_printf(ERROR_LIST, "Type mismatch: %s expected but %s found on line %d",
                                DecafType_to_string(INT),
                                DecafType_to_string(GET_INFERRED_TYPE(node->binaryop.right)),
                                node->source_line);
            }
            break;
    }
}
/**
 * @brief Postvisit for uanryop node.
 * 
 * @param vistor Nodevisitor for uanryop
 * @param node Current node to look at
 */
void AnalysisVisitor_unaryop_postvisit(NodeVisitor *visitor, ASTNode *node)
{
    /* Checking if child type is int if unary operator is - */
    if (GET_INFERRED_TYPE(node) == INT && GET_INFERRED_TYPE(node->unaryop.child) != INT) 
    {
        ErrorList_printf(ERROR_LIST, "Type mismatch: %s expected but %s found on line %d",
                         DecafType_to_string(INT),
                         DecafType_to_string((GET_INFERRED_TYPE(node->unaryop.child))),
                         node->source_line);
    }
 
    /* Checking if child type is bool if unary operator is ! */
    if (GET_INFERRED_TYPE(node) == BOOL && GET_INFERRED_TYPE(node->unaryop.child) != BOOL) {
        ErrorList_printf(ERROR_LIST, "Type mismatch: %s expected but %s found on line %d",
                         DecafType_to_string(BOOL),
                         DecafType_to_string((GET_INFERRED_TYPE(node->unaryop.child))),
                         node->source_line);
    } 
    
}

/**
 * @brief Postvisit for return node.
 * 
 * @param vistor Nodevisitor for return
 * @param node Current node to look at
 */
void AnalysisVisitor_return_postvisit(NodeVisitor *visitor, ASTNode *node)
{
    /* checking if we're currently looking in a function */
    if (DATA->funcName[0] == '\0')
    {
        /* Get type of current function */
        DecafType func_type = lookup_symbol(node, DATA->funcName)->type; 
        
        /* Checking if return value's type matches current functions return type */
        if (lookup_symbol(node, DATA->funcName)->type != GET_INFERRED_TYPE(node->funcreturn.value))
        {
            ErrorList_printf(ERROR_LIST, "Type mismatch: %s expected but %s found on line %d",
                             DecafType_to_string(func_type),
                             DecafType_to_string(GET_INFERRED_TYPE(node->funcreturn.value)),
                             node->source_line);
        }
    }
}

/**
 * @brief Postvisit for break and continue nodes.
 * 
 * @param vistor Nodevisitor for conditional
 * @param node Current node to look at
 */
void AnalysisVisitor_break_continue_postvisit(NodeVisitor *visitor, ASTNode *node)
{

    /* Checking if the vistor is currently looking through a loop,
        if not, then break/continue are not in the loop */
    if (!DATA->isInLoop)
    {
        ErrorList_printf(ERROR_LIST, "Break outside of loop found on line %d",
                         node->source_line);
    }
}

/**
 * @brief Postvisit for function declaration node.
 * 
 * @param vistor wxNode visitor for function calls
 * @param node Current node to look at
 */
void AnalysisVisitor_funccall_postvisit(NodeVisitor *visitor, ASTNode *node)
{
    Symbol* symbol = lookup_symbol(node, node->funccall.name);

    /* First check if argument size matches the parameter size */
    if (node->funccall.arguments->size == symbol->parameters->size) {

        ASTNode* curArg = node->funccall.arguments->head; // head of arg linked list
        Parameter* curParam = symbol->parameters->head; // head of param linked list
        int paramNum = 0; // current parameter we're looking at

        /* Loop through each arg and param to check if the types match */
        while (curArg) {
            if (GET_INFERRED_TYPE(curArg) != curParam->type) {
                ErrorList_printf(ERROR_LIST, "Type mismatch in parameter %d of call to '%s': expected %s but found %s on line %d"
                    , paramNum, node->funccall.name, DecafType_to_string(curParam->type), 
                    DecafType_to_string(GET_INFERRED_TYPE(curArg)), node->source_line);
            }

            /* Get next element in the linekd list and update index*/
            curArg = curArg->next;
            curParam = curParam->next;
            paramNum += 1;
        }

    } else {
        ErrorList_printf(ERROR_LIST, "Invalid number of function arguments on line %d",
                    node->source_line);
    }
}

/**
 * @brief Postvisit for function declaration node.
 * 
 * @param vistor Nodevisitor for function declaration
 * @param node Current node to look at
 */
void AnalysisVisitor_funcdecl_postvisit(NodeVisitor *visitor, ASTNode *node)
{
    DATA->funcName = "";
}

/**
 * @brief Postvisit for loop node.
 * 
 * @param vistor Nodevisitor for loop
 * @param node Current node to look at
 */
void AnalysisVisitor_loop_postvisit(NodeVisitor *visitor, ASTNode *node)
{
    DATA->isInLoop = false;
}

/**
 * @brief Postvisit for block node.
 * 
 * @param vistor Nodevisitor for block
 * @param node Current node to look at
 */
void AnalysisVisitor_block_postvisit(NodeVisitor *visitor, ASTNode *node)
{
    DATA->isInBlock = false;
}