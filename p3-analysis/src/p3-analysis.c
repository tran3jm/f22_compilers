/**
 * @file p3-analysis.c
 * @brief Compiler phase 3: static analysis
 * 
 * @authors Joselyne Tran and Alden Geipel
 */
#include "p3-analysis.h"
void AnalysisVisitor_check_vardecl(NodeVisitor* visitor, ASTNode* node);
void AnalysisVisitor_check_location(NodeVisitor* visitor, ASTNode* node);
void AnalysisVisitor_check_main(NodeVisitor* visitor, ASTNode* node);
void AnalysisVisitor_assign_check(NodeVisitor* visitor, ASTNode* node);
void AnalysisVisitor_literal_type(NodeVisitor* visitor, ASTNode* node);
void AnalysisVisitor_literal_type_infer(NodeVisitor* visitor, ASTNode* node);
void AnalysisVisitor_location_type_infer(NodeVisitor* visitor, ASTNode* node);

/**
 * @brief State/data for static analysis visitor
 */
typedef struct AnalysisData
{
    /**
     * @brief List of errors detected
     */
    ErrorList* errors;

    /* BOILERPLATE: TODO: add any new desired state information (and clean it up in AnalysisData_free) */

} AnalysisData;

/**
 * @brief Allocate memory for analysis data
 * 
 * @returns Pointer to allocated structure
 */
AnalysisData* AnalysisData_new ()
{
    AnalysisData* data = (AnalysisData*)calloc(1, sizeof(AnalysisData));
    CHECK_MALLOC_PTR(data);
    data->errors = ErrorList_new();
    return data;
}

/**
 * @brief Deallocate memory for analysis data
 * 
 * @param data Pointer to the structure to be deallocated
 */
void AnalysisData_free (AnalysisData* data)
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
#define DATA ((AnalysisData*)visitor->data)

/**
 * @brief Macro for more convenient access to the error list inside a
 * @ref AnalysisVisitor data structure
 */
#define ERROR_LIST (((AnalysisData*)visitor->data)->errors)

/**
 * @brief Wrapper for @ref lookup_symbol that reports an error if the symbol isn't found
 * 
 * @param visitor Visitor with the error list for reporting
 * @param node AST node to begin the search at
 * @param name Name of symbol to find
 * @returns The @ref Symbol if found, otherwise @c NULL
 */
Symbol* lookup_symbol_with_reporting(NodeVisitor* visitor, ASTNode* node, const char* name)
{
    Symbol* symbol = lookup_symbol(node, name);
    if (symbol == NULL) {
        ErrorList_printf(ERROR_LIST, "Symbol '%s' undefined on line %d", name, node->source_line);
    }
    return symbol;
}

/**
 * @brief Macro for shorter storing of the inferred @c type attribute
 */
#define SET_INFERRED_TYPE(T) ASTNode_set_printable_attribute(node, "type", (void*)(T), \
                                 type_attr_print, dummy_free)

/**
 * @brief Macro for shorter retrieval of the inferred @c type attribute
 */
#define GET_INFERRED_TYPE(N) (DecafType)ASTNode_get_attribute(N, "type")

ErrorList* analyze (ASTNode* tree)
{
    /* allocate analysis structures */
    NodeVisitor* v = NodeVisitor_new();
    v->data = (void*)AnalysisData_new();
    v->dtor = (Destructor)AnalysisData_free;

    /* BOILERPLATE: TODO: register analysis callbacks */
    v->previsit_literal = AnalysisVisitor_literal_type_infer;
    v->previsit_location= AnalysisVisitor_location_type_infer;

    v->postvisit_location = AnalysisVisitor_check_location;
    v->postvisit_program = AnalysisVisitor_check_main;
    v->postvisit_vardecl = AnalysisVisitor_check_vardecl;

    v->postvisit_assignment = AnalysisVisitor_assign_check;

    /* perform analysis, save error list, clean up, and return errors */
    NodeVisitor_traverse(v, tree);
    ErrorList* errors = ((AnalysisData*)v->data)->errors;
    NodeVisitor_free(v);
    return errors;
}

/**
 * @brief Helper method to turn types into string for
 * error messages.
 * 
 * @param x type to turn into string
 * @return const char* type as string
 */
const char* infered_type_to_string(DecafType x) {
    
    switch(x)
    {
        case UNKNOWN:
            return "unknown";
        case INT:
            return "int";
        case BOOL:
            return "bool";
        case VOID:
            return "void";
        case STR:
            return "string";
        default:
            return "invalid";
    }
    return "invalid";
}

void AnalysisVisitor_check_vardecl(NodeVisitor* visitor, ASTNode* node) {
    if (node->vardecl.type == VOID) {
        ErrorList_printf(ERROR_LIST, "Void variable '%s' on line %d", node->vardecl.name, node->source_line);
    }

    if (node->vardecl.is_array && node->vardecl.array_length <= 0) {
        ErrorList_printf(ERROR_LIST, "Array \'%s\' on line %d must have positive non-zero length", node->vardecl.name, node->source_line);
    }
}

void AnalysisVisitor_location_type_infer(NodeVisitor* visitor, ASTNode* node) {
    if (lookup_symbol_with_reporting(visitor, node, node->location.name)) {
        SET_INFERRED_TYPE(lookup_symbol_with_reporting(visitor, node, node->location.name)->type);
    } 
}

void AnalysisVisitor_check_location(NodeVisitor* visitor, ASTNode* node) {
    // if (!lookup_symbol_with_reporting(visitor, node, node->location.name)) {
    //     ErrorList_printf(ERROR_LIST, "Invalid location name '%s' on line %d", node->location.name, node->source_line);
    // }
}

void AnalysisVisitor_check_main(NodeVisitor* visitor, ASTNode* node) {
    if (lookup_symbol(node, "main") == NULL) {
        ErrorList_printf(ERROR_LIST, "Program does not contain \'main\' function", node->funcdecl.name, node->source_line);
    }
}

void AnalysisVisitor_literal_type_infer(NodeVisitor* visitor, ASTNode* node) {
    SET_INFERRED_TYPE(node->literal.type);
}

void AnalysisVisitor_assign_check(NodeVisitor* visitor, ASTNode* node) {
    if (GET_INFERRED_TYPE(node->assignment.location) != GET_INFERRED_TYPE(node->assignment.value)) {
        ErrorList_printf(ERROR_LIST, "Type mismatch: %s is incompatible with %s on line %d",
            infered_type_to_string((GET_INFERRED_TYPE(node->assignment.location))), 
            infered_type_to_string((GET_INFERRED_TYPE(node->assignment.value))),
              node->source_line);
    }
}
