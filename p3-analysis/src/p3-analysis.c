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
void AnalysisVisitor_check_location(NodeVisitor *visitor, ASTNode *node);
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
    data->funcName = "";
    data->isInLoop = false;
    data->currentSymbolList = NULL;
    data->scopeNum = 0;

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
    int count = 0;
    FOR_EACH(Symbol*, sym, symbols) {
        if (strncmp(name, sym->name, MAX_ID_LEN) == 0) {
            count += 1;
        }
    }
    return count != 1;
}

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

    FOR_EACH(ASTNode*, v, node->program.variables) {
        if (has_dup_symbol(DATA->currentSymbolList, v->vardecl.name)) {
            ErrorList_printf(ERROR_LIST, "Duplicate symbols named \'%s\' scope on line %d",
                         v->vardecl.name, node->source_line);
            break;
        }
    }

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
    DATA->scopeNum = node->source_line;
    DATA->currentSymbolList = st->local_symbols;
    DATA->funcName = node->funcdecl.name;
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
    SymbolTable* st = ASTNode_get_attribute(node, "symbolTable");
    DATA->currentSymbolList = st->local_symbols;
    FOR_EACH(ASTNode*, v, node->block.variables) {
        if (has_dup_symbol(DATA->currentSymbolList, v->vardecl.name)) {
            ErrorList_printf(ERROR_LIST, "Duplicate symbols named \'%s\' scope on line %d",
                         v->vardecl.name, node->source_line);
            break;
        }
    }
    DATA->scopeNum = node->source_line;
}

void AnalysisVisitor_vardecl_postvisit(NodeVisitor *visitor, ASTNode *node)
{
    if (node->vardecl.type == VOID)
    {
        ErrorList_printf(ERROR_LIST, "Void variable '%s' on line %d", node->vardecl.name, node->source_line);
    }

    if (node->vardecl.is_array && node->vardecl.array_length <= 0)
    {
        ErrorList_printf(ERROR_LIST, "Array \'%s\' on line %d must have positive non-zero length", node->vardecl.name, node->source_line);
    }
    
}

void AnalysisVisitor_location_previsit(NodeVisitor *visitor, ASTNode *node)
{
    Symbol *sym = lookup_symbol_with_reporting(visitor, node, node->location.name);
    if (sym)
    {
        SET_INFERRED_TYPE(sym->type);
    }
}

void AnalysisVisitor_location_postvisit(NodeVisitor *visitor, ASTNode *node)
{
    Symbol* symbol = lookup_symbol(node, node->location.name);
    if (symbol != NULL && symbol->symbol_type == ARRAY_SYMBOL && node->location.index == NULL)
    {
        ErrorList_printf(ERROR_LIST, "Array \'%s\' accessed without index on line %d",
                         node->location.name,
                         node->source_line);
    } else if (symbol != NULL && symbol->symbol_type == FUNCTION_SYMBOL) {
        ErrorList_printf(ERROR_LIST, "Function \'%s\' accessed as a variable on line %d",
                         node->location.name,
                         node->source_line);
    }
    // lookup_symbol_with_reporting(visitor, node, node->location.name);
}

void AnalysisVisitor_conditional_postvisit(NodeVisitor *visitor, ASTNode *node)
{
    if (GET_INFERRED_TYPE(node->conditional.condition) != BOOL)
    {
        ErrorList_printf(ERROR_LIST, "Type mismatch: %s expected but %s found on line %d",
                         DecafType_to_string(GET_INFERRED_TYPE(node->conditional.condition)),
                         DecafType_to_string(BOOL),
                         node->source_line);
    }
}

void AnalysisVisitor_program_postvisit(NodeVisitor *visitor, ASTNode *node)
{

    if (lookup_symbol(node, "main") == NULL || lookup_symbol(node, "main")->symbol_type != FUNCTION_SYMBOL)
    {
        ErrorList_printf(ERROR_LIST, "Program does not contain \'main\' function", node->funcdecl.name, node->source_line);
    }
}

void AnalysisVisitor_literal_previsit(NodeVisitor *visitor, ASTNode *node)
{
    SET_INFERRED_TYPE(node->literal.type);
}

void AnalysisVisitor_assign_postvisit(NodeVisitor *visitor, ASTNode *node)
{
    if (lookup_symbol(node, node->assignment.location->location.name)) {
        if (GET_INFERRED_TYPE(node->assignment.location) != GET_INFERRED_TYPE(node->assignment.value))
        {
            ErrorList_printf(ERROR_LIST, "Type mismatch: %s is incompatible with %s on line %d",
                            DecafType_to_string((GET_INFERRED_TYPE(node->assignment.location))),
                            DecafType_to_string((GET_INFERRED_TYPE(node->assignment.value))),
                            node->source_line);
        }
    }
}

void AnalysisVisitor_binaryop_previsit(NodeVisitor *visitor, ASTNode *node)
{
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

void AnalysisVisitor_binaryop_postvisit(NodeVisitor *visitor, ASTNode *node)
{
    switch(node->binaryop.operator)

    {
        case OROP: case ANDOP:

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
            break;

        case EQOP: case NEQOP: 

            if (GET_INFERRED_TYPE(node->binaryop.left) != GET_INFERRED_TYPE(node->binaryop.right)) 
            {
                ErrorList_printf(ERROR_LIST, "Type mismatch: %s is incompatible with %s on line %d",
                                DecafType_to_string(GET_INFERRED_TYPE(node->binaryop.left)),
                                DecafType_to_string(GET_INFERRED_TYPE(node->binaryop.right)),
                                node->source_line);
            }
            break;
        
        case LTOP: case LEOP: case GEOP: case GTOP: case ADDOP: case SUBOP: case MULOP: case DIVOP: case MODOP:

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

void AnalysisVisitor_unaryop_previsit(NodeVisitor *visitor, ASTNode *node)
{
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

void AnalysisVisitor_unaryop_postvisit(NodeVisitor *visitor, ASTNode *node)
{
    if (GET_INFERRED_TYPE(node) == INT && GET_INFERRED_TYPE(node->unaryop.child) != INT) 
    {
        ErrorList_printf(ERROR_LIST, "Type mismatch: %s expected but %s found on line %d",
                         DecafType_to_string(INT),
                         DecafType_to_string((GET_INFERRED_TYPE(node->unaryop.child))),
                         node->source_line);
    }

    if (GET_INFERRED_TYPE(node) == BOOL && GET_INFERRED_TYPE(node->unaryop.child) != BOOL) {
        ErrorList_printf(ERROR_LIST, "Type mismatch: %s expected but %s found on line %d",
                         DecafType_to_string(BOOL),
                         DecafType_to_string((GET_INFERRED_TYPE(node->unaryop.child))),
                         node->source_line);
    } 
    
}

void AnalysisVisitor_return_postvisit(NodeVisitor *visitor, ASTNode *node)
{

    if (DATA->funcName[0] == '\0')
    {
        DecafType func_type = lookup_symbol(node, DATA->funcName)->type;

        if (lookup_symbol(node, DATA->funcName)->type != GET_INFERRED_TYPE(node->funcreturn.value))
        {
            ErrorList_printf(ERROR_LIST, "Type mismatch: %s expected but %s found on line %d",
                             DecafType_to_string(func_type),
                             DecafType_to_string(GET_INFERRED_TYPE(node->funcreturn.value)),
                             node->source_line);
        }
    }
}

void AnalysisVisitor_break_continue_postvisit(NodeVisitor *visitor, ASTNode *node)
{

    if (!DATA->isInLoop)
    {
        ErrorList_printf(ERROR_LIST, "Break outside of loop found on line %d",
                         node->source_line);
    }
}


void AnalysisVisitor_funccall_previsit(NodeVisitor *visitor, ASTNode *node)
{
    
    Symbol* symbol = lookup_symbol(node, node->funccall.name);
    if (symbol == NULL)
    {
        ErrorList_printf(ERROR_LIST, "Program does not contain \'%s\' function", node->funccall.name, node->source_line);
    } else if (symbol != NULL && symbol->symbol_type != FUNCTION_SYMBOL) {
        ErrorList_printf(ERROR_LIST, "Invalid call to non-function \'%s\' on line %d",
                         node->location.name,
                         node->source_line);
    }
    SET_INFERRED_TYPE(symbol->type);
}

/**
 * @brief Post-visitor for function declaration node.
 * 
 * @param vistor wxNode visitor for function calls
 * @param node Current node to look at
 */
void AnalysisVisitor_funccall_postvisit(NodeVisitor *visitor, ASTNode *node)
{
    Symbol* symbol = lookup_symbol(node, node->funccall.name);

    if (node->funccall.arguments->size == symbol->parameters->size) {
        ASTNode* curArg = node->funccall.arguments->head;
        Parameter* curParam = symbol->parameters->head;
        int paramNum = 0;
        while (curArg) {
            // alden - :^) 
            // joselyne - }:(

            if (GET_INFERRED_TYPE(curArg) != curParam->type) {
                ErrorList_printf(ERROR_LIST, "Type mismatch in parameter %d of call to '%s': expected %s but found %s on line %d"
                    , paramNum, node->funccall.name, DecafType_to_string(curParam->type), 
                    DecafType_to_string(GET_INFERRED_TYPE(curArg)), node->source_line);
            }
            curArg = curArg->next;
            curParam = curParam->next;
            paramNum += 1;
        }

    } else {
        ErrorList_printf(ERROR_LIST, "Invalid number of function arguments on line %d",
                    node->source_line);
    }
}

void AnalysisVisitor_funcdecl_postvisit(NodeVisitor *visitor, ASTNode *node)
{
    DATA->funcName = "";
}

void AnalysisVisitor_loop_previsit(NodeVisitor *visitor, ASTNode *node)
{
    DATA->isInLoop = true;
}

void AnalysisVisitor_loop_postvisit(NodeVisitor *visitor, ASTNode *node)
{
    DATA->isInLoop = false;
}