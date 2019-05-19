#include <stdio.h>
#include <stdint.h>

//
// Object file has the following format:
//  +---------+
//  | magic   |     "BESM6\0"
//  +---------+
//  | entries |     Table of entries
//  +---------+
//  | header  |     10 words or 3 words
//  +---------+
//  | code    |     Executable code
//  +---------+
//  | const   |     Initialized data
//  +---------+
//  | data    |     Transient data for SET section
//  +---------+
//  | set     |     SET directives
//  +---------+
//  | symhdr  |     Name of the subroutine
//  +---------+
//  | symtab  |     Symbol table
//  +---------+
//  | longsym |     Names of 5 characters and more
//  +---------+
//  | debug   |     Debug information
//  +---------+
//
typedef struct _obj_image_t {

    unsigned head_len;          // length of symhdr
    unsigned sym_len;           // length of symtab
    unsigned debug_len;         // length of debug section

    unsigned set_len;           // length of SET section
    unsigned data_len;          // length of data section
    unsigned long_len;          // length of longsym section

    unsigned cmd_len;           // length of code section
    unsigned bss_len;           // length of bss section
    unsigned const_len;         // length of const section

    unsigned head_off;          // offset of header
    unsigned cmd_off;           // offset of code section
    unsigned table_off;         // offset of symhdr section
    unsigned long_off;          // offset of longsym section
    unsigned debug_off;         // offset of debug section
    unsigned comment_off;       // offset of comment section

    unsigned nwords;
    unsigned nentries;
    struct _obj_image_t *next;

    // Last element: can be re-allocated with smaller size.
#define MAXSZ 50000
    uint64_t word[MAXSZ];

} obj_image_t;

//
// Symbol types
//
enum {
    SYM_OFFSET      = 0000,     // Offset from another symbol
    SYM_ADD         = 0001,     // Add two symbols
    SYM_SUBTRACT    = 0003,     // Subtract two symbols
    SYM_MULTIPLY    = 0101,     // Multiply two symbols
    SYM_DIVIDE      = 0103,     // Divide two symbols
    SYM_ABS         = 0400,     // Absolute address
    SYM_RELOC       = 0410,     // Relocatable address
    SYM_EXT_S       = 0430,     // External reference (short name)
    SYM_PRIVATE_S   = 0460,     // Private block (short name)
    SYM_COMMON_S    = 0470,     // Common block (short name)
    SYM_INDIRECT    = 0501,     // Dereference, like *ptr
    SYM_CONST       = 0520,     // Constant value, same as SYM_ABS
    SYM_EXPRESSION  = 0521,     // Expression to compute at load time
    SYM_EXT_L       = 0630,     // External reference (long name)
    SYM_PRIVATE_L   = 0660,     // Private block (long name)
    SYM_COMMON_L    = 0670,     // Common block (long name)
};

//
// First word of object file: magic key.
//
static const uint64_t BESM6_MAGIC = 0x4245534d3600;

//
// Read object image from a file.
// Return negative in case of failure.
//
extern int obj_read_fd(FILE *fd, obj_image_t *obj);

//
// Read object image from a data buffer.
// Return negative in case of failure.
//
extern int obj_read_data(char *data, unsigned nbytes, obj_image_t *obj);

//
// Allocate a copy if object image.
// Return NULL when failed.
//
extern obj_image_t *obj_copy(obj_image_t *from);
