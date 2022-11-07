/**
 * @file p4-codegen.c
 * @brief Compiler phase 4: code generation
 * 
 * @author Alden Geipel and Joselyne Tran
 */
#include "p4-codegen.h"

/**
 * @brief State/data for the code generator visitor
 */
typedef struct CodeGenData
{
    /**
     * @brief Reference to the epilogue jump label for the current function
     */
    Operand current_epilogue_jump_label;

    /* add any new desired state information (and clean it up in CodeGenData_free) */
} CodeGenData;

/**
 * @brief Allocate memory for code gen data
 * 
 * @returns Pointer to allocated structure
 */
CodeGenData* CodeGenData_new ()
{
    CodeGenData* data = (CodeGenData*)calloc(1, sizeof(CodeGenData));
    CHECK_MALLOC_PTR(data);
    data->current_epilogue_jump_label = empty_operand();
    return data;
}

/**
 * @brief Deallocate memory for code gen data
 * 
 * @param data Pointer to the structure to be deallocated
 */
void CodeGenData_free (CodeGenData* data)
{
    /* free everything in data that is allocated on the heap */

    /* free "data" itself */
    free(data);
}

/**
 * @brief Macro for more convenient access to the error list inside a @c visitor
 * data structure
 */
#define DATA ((CodeGenData*)visitor->data)

/**
 * @brief Fills a register with the base address of a variable.
 * 
 * @param node AST node to emit code into (if needed)
 * @param variable Desired variable
 * @returns Virtual register that contains the base address
 */
Operand var_base (ASTNode* node, Symbol* variable)
{
    Operand reg = empty_operand();
    switch (variable->location) {
        case STATIC_VAR:
            reg = virtual_register();
            ASTNode_emit_insn(node,
                    ILOCInsn_new_2op(LOAD_I, int_const(variable->offset), reg));
            break;
        case STACK_PARAM:
        case STACK_LOCAL:
            reg = base_register();
            break;
        default:
            break;
    }
    return reg;
}

/**
 * @brief Calculates the offset of a scalar variable reference and fills a register with that offset.
 * 
 * @param node AST node to emit code into (if needed)
 * @param variable Desired variable
 * @returns Virtual register that contains the base address
 */
Operand var_offset (ASTNode* node, Symbol* variable)
{
    Operand op = empty_operand();
    switch (variable->location) {
        case STATIC_VAR:    op = int_const(0); break;
        case STACK_PARAM:
        case STACK_LOCAL:   op = int_const(variable->offset);
        default:
            break;
    }
    return op;
}

#ifndef SKIP_IN_DOXYGEN

/*
 * Macros for more convenient instruction generation
 */

#define EMIT0OP(FORM)             ASTNode_emit_insn(node, ILOCInsn_new_0op(FORM))
#define EMIT1OP(FORM,OP1)         ASTNode_emit_insn(node, ILOCInsn_new_1op(FORM,OP1))
#define EMIT2OP(FORM,OP1,OP2)     ASTNode_emit_insn(node, ILOCInsn_new_2op(FORM,OP1,OP2))
#define EMIT3OP(FORM,OP1,OP2,OP3) ASTNode_emit_insn(node, ILOCInsn_new_3op(FORM,OP1,OP2,OP3))

void CodeGenVisitor_gen_program (NodeVisitor* visitor, ASTNode* node)
{
    /*
     * make sure "code" attribute exists at the program level even if there are
     * no functions (although this shouldn't happen if static analysis is run
     * first); also, don't include a print function here because there's not
     * really any need to re-print all the functions in the program node *
     */
    ASTNode_set_attribute(node, "code", InsnList_new(), (Destructor)InsnList_free);

    /* copy code from each function */
    FOR_EACH(ASTNode*, func, node->program.functions) {
        ASTNode_copy_code(node, func);
    }
}

void CodeGenVisitor_previsit_funcdecl (NodeVisitor* visitor, ASTNode* node)
{
    /* generate a label reference for the epilogue that can be used while
     * generating the rest of the function (e.g., to be used when generating
     * code for a "return" statement) */
    DATA->current_epilogue_jump_label = anonymous_label();
}

void CodeGenVisitor_gen_funcdecl (NodeVisitor* visitor, ASTNode* node)
{
    /* every function begins with the corresponding call label */
    EMIT1OP(LABEL, call_label(node->funcdecl.name));

    /* BOILERPLATE: TODO: implement prologue */
    Operand base = var_base(node, lookup_symbol(node,node->funcdecl.name));

    // EMIT1OP(PUSH, var_base(node, lookup_symbol(node,node->funcdecl.name)));
    // EMIT2OP(I2I, stack_register(), base);

    /* copy code from body */
    ASTNode_copy_code(node, node->funcdecl.body);

    EMIT1OP(LABEL, DATA->current_epilogue_jump_label);
    /* BOILERPLATE: TODO: implement epilogue */
    EMIT0OP(RETURN);
}

void CodeGenVisitor_gen_literal (NodeVisitor* visitor, ASTNode* node)
{
    /* Generates reg to r0 */
    Operand reg = virtual_register();

    /* Setting it to reg */
    ASTNode_set_temp_reg(node, reg);

    /* Handles both case */
    switch (node->literal.type) {
        case INT: case BOOL:
            EMIT2OP(LOAD_I, int_const(node->literal.integer), reg);
            break;
        case STR:
            EMIT2OP(LOAD_I, str_const(node->literal.string), reg);
            break;
        case UNKNOWN: case VOID:
            // ????
            break;
    }

}

/**
 * @brief Post-vistor fpr return statements
 * 
 * @param vistor Visitor for return statement
 * @param node AST node to emit code into (if needed)
 */
void CodeGenVisitor_gen_returnstment (NodeVisitor* visitor, ASTNode* node)
{

    /* Generates return register */
    Operand return_reg = return_register();

    /* Copys code into current node*/
    ASTNode_copy_code(node, node->funcreturn.value);

    /* Storing into temp reg */
    Operand reg = ASTNode_get_temp_reg(node->funcreturn.value);

     /* put into return reg */
    EMIT2OP(I2I, reg, return_reg);

    /* jump */
    // EMIT1OP(JUMP, DATA->current_epilogue_jump_label);
}

/**
 * @brief Post-vistor fpr blocks
 * 
 * @param vistor Visitor for block
 * @param node AST node to emit code into (if needed)
 */
void CodeGenVisitor_gen_block (NodeVisitor* visitor, ASTNode* node)
{
    int stack_space = 0;

    // find a more efficient way also this is most likely not right
    // FOR_EACH(ASTNode*, n, node->block.variables) {
    //     stack_space -= 8;
    // }

    // how much space to allocate
    // Operand offset = int_const(stack_space);
    // EMIT3OP(ADD_I, stack_register(), offset, stack_register());

    FOR_EACH(ASTNode*, n, node->block.statements) {
        ASTNode_copy_code(node, n);
    }
}

// /**
//  * @brief Post-vistor fpr blocks
//  * 
//  * @param vistor Visitor for block
//  * @param node AST node to emit code into (if needed)
//  */
// void CodeGenVisitor_gen_location (NodeVisitor* visitor, ASTNode* node)
// {
//     var_base(node, node->location.name);
// }

/**
 * @brief Post-vistor fpr blocks
 * 
 * @param vistor Visitor for block
 * @param node AST node to emit code into (if needed)
 */
void CodeGenVisitor_gen_assignments (NodeVisitor* visitor, ASTNode* node)
{
    Operand reg = ASTNode_get_temp_reg(node->assignment.value);
    Operand base = var_base(node, lookup_symbol(node, node->assignment.location->location.name));
    Operand stack_offset = var_offset(node, lookup_symbol(node, node->assignment.location->location.name));
    EMIT3OP(STORE_AI, reg, base, stack_offset);
}

/**
 * @brief Helper function for binary operations to reduce code
 * 
 * @param node AST node to emit code into (if needed)
 * @param left Left operand
 * @param right Right operand
 * @param op_instruct Op instruction to use for EMIT3
 */
void helper_binary_ops(NodeVisitor* visitor, ASTNode* node, ASTNode* left, ASTNode* right, InsnForm op_instruct) {

    ASTNode_copy_code(node, left);
    ASTNode_copy_code(node, right);

    /* Storing into temp reg */
    Operand reg_left = ASTNode_get_temp_reg(left);
    Operand reg_right = ASTNode_get_temp_reg(right);

    /* Adding regs */
    Operand reg = virtual_register();
    ASTNode_set_temp_reg(node, reg);

    EMIT3OP(op_instruct, reg_left, reg_right, reg);
}

/**
 * @brief Helper function for binary operations to reduce code
 * 
 * @param node AST node to emit code into (if needed)
 * @param left Left operand
 * @param right Right operand
 * @param op_instruct Op instruction to use for EMIT3
 */
void helper_unary_ops(NodeVisitor* visitor, ASTNode* node, ASTNode* value, InsnForm op_instruct) {

    ASTNode_copy_code(node, value);

    /* Storing into temp reg */
    Operand reg_val = ASTNode_get_temp_reg(value);

    /* Adding regs */
    Operand reg = virtual_register();
    ASTNode_set_temp_reg(node, reg);

    EMIT2OP(op_instruct, reg_val, reg);
}

/**
 * @brief Post-vistor fpr binary operations
 * 
 * @param vistor Visitor for binary operations
 * @param node AST node to emit code into (if needed)
 */
void CodeGenVisitor_gen_binary (NodeVisitor* visitor, ASTNode* node) {
    
    switch(node->binaryop.operator)
    {
        case OROP:
            helper_binary_ops(visitor, node, node->binaryop.left, node->binaryop.right, OR);
            break;
        case ANDOP:
            helper_binary_ops(visitor, node, node->binaryop.left, node->binaryop.right, AND);
            break;
        case EQOP:
            helper_binary_ops(visitor, node, node->binaryop.left, node->binaryop.right, CMP_EQ);
            break;
        case NEQOP:
            helper_binary_ops(visitor, node, node->binaryop.left, node->binaryop.right, CMP_NE);
            break;
        case LTOP:
            helper_binary_ops(visitor, node, node->binaryop.left, node->binaryop.right, CMP_LT);
            break;
        case LEOP:
            helper_binary_ops(visitor, node, node->binaryop.left, node->binaryop.right, CMP_LE);
            break;
        case GEOP:
            helper_binary_ops(visitor, node, node->binaryop.left, node->binaryop.right, CMP_GE);
            break;
        case GTOP:
            helper_binary_ops(visitor, node, node->binaryop.left, node->binaryop.right, CMP_GT);
            break;
        case ADDOP:
            helper_binary_ops(visitor, node, node->binaryop.left, node->binaryop.right, ADD);
            break;
        case SUBOP:
            helper_binary_ops(visitor, node, node->binaryop.left, node->binaryop.right, SUB);
            break;
        case MULOP:
            helper_binary_ops(visitor, node, node->binaryop.left, node->binaryop.right, MULT);
            break;
        case DIVOP:
            helper_binary_ops(visitor, node, node->binaryop.left, node->binaryop.right, DIV);
            break;
        // mod for B tests
        default:
            break;
    }
}

/**
 * @brief Post-vistor fpr binary operations
 * 
 * @param vistor Visitor for binary operations
 * @param node AST node to emit code into (if needed)
 */
void CodeGenVisitor_gen_unary (NodeVisitor* visitor, ASTNode* node) {

    switch(node->unaryop.operator)
    {
        case NEGOP:
            helper_unary_ops(visitor, node, node->unaryop.child, NEG);
            break;
        case NOTOP:
            helper_unary_ops(visitor, node, node->unaryop.child, NOT);
            break;
    }

}

#endif
InsnList* generate_code (ASTNode* tree)
{
    InsnList* iloc = InsnList_new();


    NodeVisitor* v = NodeVisitor_new();
    v->data = CodeGenData_new();
    v->dtor = (Destructor)CodeGenData_free;

    /* Previsits */
    v->previsit_funcdecl     = CodeGenVisitor_previsit_funcdecl;

    /* Postvisits*/
    v->postvisit_program     = CodeGenVisitor_gen_program;
    v->postvisit_funcdecl    = CodeGenVisitor_gen_funcdecl;
    v->postvisit_literal     = CodeGenVisitor_gen_literal;
    v->postvisit_return      = CodeGenVisitor_gen_returnstment;
    v->postvisit_block       = CodeGenVisitor_gen_block;
    v->postvisit_binaryop    = CodeGenVisitor_gen_binary;
    v->postvisit_unaryop    = CodeGenVisitor_gen_unary;
    v->postvisit_assignment  = CodeGenVisitor_gen_assignments;

    /* generate code into AST attributes */
    NodeVisitor_traverse_and_free(v, tree);

    /* copy generated code into new list (the AST may be deallocated before
     * the ILOC code is needed) */
    FOR_EACH(ILOCInsn*, i, (InsnList*)ASTNode_get_attribute(tree, "code")) {
        InsnList_add(iloc, ILOCInsn_copy(i));
    }
    return iloc;
}
