/**
 * @file p5-regalloc.c
 * @brief Compiler phase 5: register allocation
 */
#include "p5-regalloc.h"
#include <math.h>

/**
 * @brief Maximum number of physical registers
 */
#define MAX_PHYSICAL_REGS 32


int ensure (int vr, int name[], int offset[], int num_physical_registers, ILOCInsn* prev_insn);
int allocate (int vr, int name[], int num_physical_registers);
float dist (int vr, ILOCInsn* insn);
void spill (int pr, ILOCInsn* prev_insn, ILOCInsn* local_allocator, int offset[], int name[]);


/**
 * @brief Replace a virtual register id with a physical register id
 * 
 * Every copy of "vr" will be replaced by "pr" in the given instruction.
 * Note that in our implementation, we do not distinguish between virutal
 * and physical registers explicitly.
 * 
 * @param vr Virtual register id that should be replaced
 * @param pr Physical register id that it should be replaced by
 * @param isnsn Instruction to modify
 */
void replace_register(int vr, int pr, ILOCInsn* insn)
{
    for (int i = 0; i < 3; i++) {
        if (insn->op[i].type == VIRTUAL_REG && insn->op[i].id == vr) {
            insn->op[i].id = pr;
        }
    }
}

/**
 * @brief Insert a store instruction to spill a register to the stack
 * 
 * We need to allocate a new slot in the stack from for the current
 * function, so we need a reference to the local allocator instruction.
 * This instruction will always be the third instruction in a function
 * and will be of the form "add SP, -X => SP" where X is the current
 * stack frame size.
 * 
 * @param pr Physical register id that should be spilled
 * @param prev_insn Reference to an instruction; the new instruction will be
 * inserted directly after this one
 * @param local_allocator Reference to the local frame allocator instruction
 * @returns BP-based offset where the register was spilled
 */
int insert_spill(int pr, ILOCInsn* prev_insn, ILOCInsn* local_allocator)
{
    /* adjust stack frame size to add new spill slot */
    int bp_offset = local_allocator->op[1].imm - WORD_SIZE;
    local_allocator->op[1].imm = bp_offset;

    /* create store instruction */
    ILOCInsn* new_insn = ILOCInsn_new_3op(STORE_AI,
            register_with_id(pr), base_register(), int_const(bp_offset));

    /* insert into code */
    new_insn->next = prev_insn->next;
    prev_insn->next = new_insn;

    return bp_offset;
}

/**
 * @brief Insert a load instruction to load a spilled register
 * 
 * @param bp_offset BP-based offset where the register value is spilled
 * @param pr Physical register where the value should be loaded
 * @param prev_insn Reference to an instruction; the new instruction will be
 * inserted directly after this one
 */
void insert_load(int bp_offset, int pr, ILOCInsn* prev_insn)
{
    /* create load instruction */
    ILOCInsn* new_insn = ILOCInsn_new_3op(LOAD_AI,
            base_register(), int_const(bp_offset), register_with_id(pr));

    /* insert into code */
    new_insn->next = prev_insn->next;
    prev_insn->next = new_insn;
}

void allocate_registers (InsnList* list, int num_physical_registers)
{
    if (list) {
        int name[num_physical_registers];
        int offset[num_physical_registers];

        memset(name, -1, sizeof(name));
        memset(offset, -1, sizeof(offset));
        ILOCInsn* stack_alloc_instruct;
        ILOCInsn* prev_insn;

        FOR_EACH(ILOCInsn*, insn, list) 
        {
            // save reference to stack allocator instruction if i is a call label
            // reset name[] and offset[] if i is a leader (jump or call target)
            if (insn->form == LABEL) {
                if (insn->op[0].type == CALL_LABEL) {
                    // the third instruct for each function will always be the stack alloc instruction (3 nexts!)
                    stack_alloc_instruct = insn->next->next->next;
                }
                memset(name, -1, sizeof(name));
                memset(offset, -1, sizeof(offset));
            }

            ILOCInsn* read_regs = ILOCInsn_get_read_registers(insn);

            int pr = -1;
            int vr = -1;

            for (int i = 0; i < 3; i++) { 
                if (read_regs->op[i].type == VIRTUAL_REG) {

                    vr = read_regs->op[i].id;
                    pr = ensure(vr, name, offset, num_physical_registers, prev_insn);
                    replace_register(vr, pr, insn);

                    if (dist(vr, insn) == INFINITY) {    // if no future use
                        name[pr] = -1;                   // then free pr
                    }
                }
            }

            ILOCInsn_free(read_regs);
            Operand write_reg = ILOCInsn_get_write_register(insn);

            if (write_reg.type == VIRTUAL_REG) {
                vr = write_reg.id;
                pr = allocate(vr, name, num_physical_registers);
                replace_register(vr, pr, insn);
            }

            if (insn->form == CALL) {

                for (int i = 0; i < num_physical_registers; i++)
                {
                    if (name[i] != -1) {
                        spill(i, prev_insn, stack_alloc_instruct, name, offset);
                    }
                }
            }
            prev_insn = insn;
        }
    }
}

/**
 * @brief Insert a load instruction to load a spilled register
 * 
 * @param bp_offset BP-based offset where the register value is spilled
 * @param pr Physical register where the value should be loaded
 * @param prev_insn Reference to an instruction; the new instruction will be
 * inserted directly after this one
 */
int ensure (int vr, int name[], int offset[], int num_physical_registers, ILOCInsn* prev_insn) 
{
    int pr = -1;
    for (int i = 0; i < num_physical_registers; i++)
    {
       if (name[i] == vr) {
            return i;
       }
    }

    pr = allocate(vr, name, num_physical_registers);
    if (offset[vr] != -1) {                         // if vr was spilled, load it
        insert_load(offset[vr], pr, prev_insn);     // and use it
    }
    return pr;                           
}

/**
 * @brief Allocates virtual register into first empty slot in name
 * 
 * @param bp_offset BP-based offset where the register value is spilled
 * @param pr Physical register where the value should be loaded
 * @param prev_insn Reference to an instruction; the new instruction will be
 * inserted directly after this one
 */
int allocate (int vr, int name[], int num_physical_registers) 
{
    for (int i = 0; i < num_physical_registers; i++)
    {
        if (name[i] == -1) {
            name[i] = vr;
            return i;
        }
    }

    // TODO:
    // find pr that maximizes dist(name[pr])   // otherwise, find register to spill
    // spill(pr)                               // spill value to stack
    // name[pr] = vr                           // reallocate it
    // return pr                               // and use it
    return vr;
}

/**
 * @brief Insert a load instruction to load a spilled register
 * 
 * @param bp_offset BP-based offset where the register value is spilled
 * @param pr Physical register where the value should be loaded
 * @param prev_insn Reference to an instruction; the new instruction will be
 * inserted directly after this one
 */
float dist (int vr, ILOCInsn* insn) 
{
    ILOCInsn* curr = insn->next;
    int dist = 1;

    while (curr != NULL)
    {
        for (int i = 0; i < 3; i++) {
            if (curr->op[i].type != EMPTY) {
                break;
            }
            if (curr->op[i].id == vr) {
                return dist;
            }
        }
        dist++;
        curr = curr->next;
    }
    return INFINITY;
}


/**
 * @brief Spills the register.
 * 
 * @param pr Physical register where the value should be loaded
 * @param prev_insn Reference to an instruction; the new instruction will be
 * inserted directly after this one
 * @param local_allocator Reference to an instruction; the new instruction will be
 * @param offset Offset array
 * @param name Reference to an instruction; the new instruction will be
 */
void spill (int pr, ILOCInsn* prev_insn, ILOCInsn* local_allocator, int offset[], int name[])
{
    int x = insert_spill(pr, prev_insn, local_allocator);
    offset[name[pr]] = x;
    name[pr] = -1;
}