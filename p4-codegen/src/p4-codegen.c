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
    Operand current_loop_jump_label;
    Operand body_loop_jump_label;
    Operand post_loop_jump_label;

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
    data->current_loop_jump_label = empty_operand();
    data->body_loop_jump_label = empty_operand();
    data->post_loop_jump_label = empty_operand();
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
void CodeGenVisitor_previsit_loop (NodeVisitor* visitor, ASTNode* node)
{
    /* generate a label reference for the epilogue that can be used while
     * generating the rest of the function (e.g., to be used when generating
     * code for a "return" statement) */
    DATA->current_loop_jump_label = anonymous_label();
    DATA->body_loop_jump_label = anonymous_label();
    DATA->post_loop_jump_label = anonymous_label();
}

void CodeGenVisitor_gen_funcdecl (NodeVisitor* visitor, ASTNode* node)
{
    /* every function begins with the corresponding call label */
    EMIT1OP(LABEL, call_label(node->funcdecl.name));

    /* BOILERPLATE: TODO: implement prologue */
    Operand base = base_register();
    Operand stack = stack_register();

    EMIT1OP(PUSH, base);
    EMIT2OP(I2I, stack, base);
    
    int stack_space = 0;
    // find a more efficient way also this is most likely not right
    FOR_EACH(ASTNode*, n, node->funcdecl.body->block.variables) {
        stack_space -= 8;
    }

    // how much space to allocate
    Operand offset = int_const(stack_space);
    EMIT3OP(ADD_I, stack_register(), offset, stack_register());

    /* copy code from body */
    ASTNode_copy_code(node, node->funcdecl.body);
    EMIT1OP(LABEL, DATA->current_epilogue_jump_label);

    /* BOILERPLATE: TODO: implement epilogue */
    EMIT2OP(I2I, base, stack);
    EMIT1OP(POP, base);
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
 * @brief Helper method for print functional call
 * 
 * @param node AST node to emit code into (if needed)
 */
void print_helper(ASTNode* node) {

    /* defintely theres gotta be something better than this please hahahha */
    Operand o;

    if (strcmp(node->funccall.name, "str")) {

        FOR_EACH(ASTNode*, n, node->funccall.arguments) {
            EMIT1OP(PRINT, str_const(n->literal.string));
         }

    } else if (strcmp(node->funccall.name, "int")) {
        FOR_EACH(ASTNode*, n, node->funccall.arguments) {
            ASTNode_copy_code(node, n);
            o = ASTNode_get_temp_reg(n);
        }
        EMIT1OP(PRINT, o);
    }


     
}

/**
 * @brief Post-vistor for function calls
 * 
 * @param vistor Visitor for block
 * @param node AST node to emit code into (if needed)
 */
void CodeGenVisitor_gen_funccall (NodeVisitor* visitor, ASTNode* node) {
    
    if (strstr(node->funccall.name, "print")) {
        print_helper(node);

    } else {
        int stack_space = 0;

        FOR_EACH(ASTNode*, n, node->funccall.arguments) {
            stack_space += 8;
            ASTNode_copy_code(node, n);
        }

        if (stack_space != 0) {
            FOR_EACH(ASTNode*, n, node->funccall.arguments) {
                Operand reg = ASTNode_get_temp_reg(n);
                EMIT1OP(PUSH, reg);
            }
        }
        Operand offset = int_const(stack_space);
        Operand return_reg = return_register();
        Operand reg = virtual_register();

        EMIT1OP(CALL, call_label(node->funcdecl.name));
        EMIT3OP(ADD_I, stack_register(), offset, stack_register());
        EMIT2OP(I2I, return_reg, reg);
        ASTNode_set_temp_reg(node, reg);
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
    EMIT1OP(JUMP, DATA->current_epilogue_jump_label);
}

/**
 * @brief Post-vistor fpr blocks
 * 
 * @param vistor Visitor for block
 * @param node AST node to emit code into (if needed)
 */
void CodeGenVisitor_gen_block (NodeVisitor* visitor, ASTNode* node)
{

    FOR_EACH(ASTNode*, n, node->block.statements) {
        ASTNode_copy_code(node, n);
    }
}

/**
 * @brief Post-vistor fpr blocks
 * 
 * @param vistor Visitor for block
 * @param node AST node to emit code into (if needed)
 */
void CodeGenVisitor_gen_assignments (NodeVisitor* visitor, ASTNode* node)
{
    ASTNode_copy_code(node, node->assignment.value);
    Operand reg = ASTNode_get_temp_reg(node->assignment.value);
    Operand base = var_base(node, lookup_symbol(node, node->assignment.location->location.name));
    Operand stack_offset = var_offset(node, lookup_symbol(node, node->assignment.location->location.name));
    EMIT3OP(STORE_AI, reg, base, stack_offset);
}

/**
 * @brief Post-vistor for location
 * 
 * @param vistor Visitor for block
 * @param node AST node to emit code into (if needed)
 */
void CodeGenVisitor_gen_location(NodeVisitor* visitor, ASTNode* node) {
    Operand reg = virtual_register(); 
    // Operand reg = ASTNode_get_temp_reg(node->location.name);
    Operand base = var_base(node, lookup_symbol(node, node->location.name));
    Operand offset = var_offset(node, lookup_symbol(node, node->location.name));
    
    ASTNode_set_temp_reg(node, reg);
    EMIT3OP(LOAD_AI, base, offset, reg);

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
 * @brief Helper function for modulus
 * 
 * @param node AST node to emit code into (if needed)
 * @param left Left operand
 * @param right Right operand
 */
void helper_mod(NodeVisitor* visitor, ASTNode* node, ASTNode* left, ASTNode* right) {

    ASTNode_copy_code(node, left);
    ASTNode_copy_code(node, right);

    /* Storing into temp reg */
    Operand reg_left = ASTNode_get_temp_reg(left);
    Operand reg_right = ASTNode_get_temp_reg(right);

    /* Adding regs */
    Operand reg = virtual_register();
    Operand reg2 = virtual_register();
    Operand reg3 = virtual_register();
    ASTNode_set_temp_reg(node, reg);
    EMIT3OP(DIV, reg_left, reg_right, reg);
    EMIT3OP(MULT, reg_right, reg, reg2);
    EMIT3OP(SUB, reg_left, reg2, reg3);
}

/**
 * @brief Helper function for unary operations to reduce code
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
 * @brief Post-vistor for binary operations
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
        case MODOP:
            helper_mod(visitor, node, node->binaryop.left, node->binaryop.right);
        default:
            break;
    }
    // ASTNode_set_temp_reg(node, virtual_register());
}

/**
 * @brief Post-vistor fpr unary operations
 * 
 * @param vistor Visitor for unary operations
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

/**
 * @brief Post-vistor fpr conditionals 
 * 
 * @param vistor Visitor for conditionals
 * @param node AST node to emit code into (if needed)
 */
void CodeGenVisitor_gen_conditional (NodeVisitor* visitor, ASTNode* node) {
    
    ASTNode_copy_code(node, node->conditional.condition);
    Operand reg = ASTNode_get_temp_reg(node->conditional.condition);

    Operand if_label = anonymous_label();
    Operand post_label = anonymous_label();
    EMIT3OP(CBR, reg, if_label, post_label);
    

    if (node->conditional.if_block) {
        EMIT1OP(LABEL, if_label);
        ASTNode_copy_code(node, node->conditional.if_block);

    }

    if (node->conditional.else_block) {

         /* copy code from body */
        Operand temp_label = anonymous_label();
        EMIT1OP(JUMP, temp_label);
        EMIT1OP(LABEL, post_label);
        ASTNode_copy_code(node, node->conditional.else_block);
        post_label = temp_label;
        
    }

    EMIT1OP(LABEL, post_label);
}

/**
 * @brief Post-vistor for breaks
 * 
 * @param vistor Visitor for breaks
 * @param node AST node to emit code into (if needed)
 */
void CodeGenVisitor_gen_break (NodeVisitor* visitor, ASTNode* node) {
    EMIT1OP(JUMP, DATA->post_loop_jump_label);

}

/**
 * @brief Post-vistor for continues
 * 
 * @param vistor Visitor for continues
 * @param node AST node to emit code into (if needed)
 */
void CodeGenVisitor_gen_continue (NodeVisitor* visitor, ASTNode* node) {
    EMIT1OP(JUMP, DATA->current_loop_jump_label);
}


/**
 * @brief Post-vistor for while loops
 * 
 * @param vistor Visitor for while loops
 * @param node AST node to emit code into (if needed)
 */
void CodeGenVisitor_gen_loop (NodeVisitor* visitor, ASTNode* node) {


    EMIT1OP(LABEL, DATA->current_loop_jump_label);
    ASTNode_copy_code(node, node->whileloop.condition);

    Operand body_label = DATA->body_loop_jump_label;
    Operand post_label = DATA->post_loop_jump_label;

    Operand reg = ASTNode_get_temp_reg(node->whileloop.condition);
    EMIT3OP(CBR, reg, body_label, post_label);

    EMIT1OP(LABEL, body_label);
    ASTNode_copy_code(node, node->whileloop.body);
    EMIT1OP(JUMP, DATA->current_loop_jump_label);
    EMIT1OP(LABEL, post_label);
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
    v->previsit_whileloop    = CodeGenVisitor_previsit_loop;

    /* Postvisits*/
    v->postvisit_program     = CodeGenVisitor_gen_program;
    v->postvisit_funcdecl    = CodeGenVisitor_gen_funcdecl;
    v->postvisit_funccall    = CodeGenVisitor_gen_funccall;
    v->postvisit_literal     = CodeGenVisitor_gen_literal;
    v->postvisit_return      = CodeGenVisitor_gen_returnstment;
    v->postvisit_block       = CodeGenVisitor_gen_block;
    v->postvisit_binaryop    = CodeGenVisitor_gen_binary;
    v->postvisit_unaryop     = CodeGenVisitor_gen_unary;
    v->postvisit_assignment  = CodeGenVisitor_gen_assignments;
    v->postvisit_location    = CodeGenVisitor_gen_location;
    v->postvisit_conditional = CodeGenVisitor_gen_conditional;
    v->postvisit_whileloop   = CodeGenVisitor_gen_loop;
    v->postvisit_break       = CodeGenVisitor_gen_break;
    v->postvisit_continue    = CodeGenVisitor_gen_continue;

    /* generate code into AST attributes */
    if (tree) {
        NodeVisitor_traverse_and_free(v, tree);

        /* copy generated code into new list (the AST may be deallocated before
        * the ILOC code is needed) */
        FOR_EACH(ILOCInsn*, i, (InsnList*)ASTNode_get_attribute(tree, "code")) {
            InsnList_add(iloc, ILOCInsn_copy(i));
        }
    }
    return iloc;
}
