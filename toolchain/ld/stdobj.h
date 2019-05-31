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
    unsigned text_base;         // base address of code
    unsigned data_base;         // base address of data+bss (separate address space)
    unsigned entry;             // entry address

    unsigned head_off;          // offset of header
    unsigned cmd_off;           // offset of code section
    unsigned table_off;         // offset of symhdr section
    unsigned long_off;          // offset of longsym section
    unsigned debug_off;         // offset of debug section

    unsigned nwords;
    unsigned nentries;
    struct _obj_image_t *next;  // next in single-linked list (linker)
    char *filename;             // name in archive (linker)

    // Last element: can be re-allocated with smaller size.
#define MAXSZ 50000
    uint64_t word[MAXSZ];

} obj_image_t;

//
// Item of a symbol table.
//
typedef union {
    uint64_t    u64;
    struct {
        uint64_t n_addr : 15;   // address
        uint64_t n_type : 9;    // symbol type
        uint64_t n_ref  : 24;   // reference to another symbol,
    } f;                        // or short name
} nlist_t;

//
// Symbol types.
//
enum {
    SYM_OFFSET      = 0000,     // Offset from another symbol
    SYM_ADD         = 0001,     // Add two symbols
    SYM_SUBTRACT    = 0003,     // Subtract two symbols
    SYM_MULTIPLY    = 0101,     // Multiply two symbols
    SYM_DIVIDE      = 0103,     // Divide two symbols
    SYM_ABS         = 0400,     // Absolute address
    SYM_RELOC       = 0410,     // Relocatable address
    SYM_DRELOC      = 0411,     // Relocatable address in data space
    SYM_ENTRY_S     = 0414,     // Entry, relocatable (short name)
    SYM_DENTRY_S    = 0415,     // Data entry, relocatable (short name)
    SYM_EXT_S       = 0430,     // External reference (short name)
    SYM_PRIVATE_S   = 0460,     // Private block (short name)
    SYM_PPAGE_S     = 0464,     // Private page-aligned (short name)
    SYM_COMMON_S    = 0470,     // Common block (short name)
    SYM_CPAGE_S     = 0474,     // Common page-aligned (short name)
    SYM_INDIRECT    = 0501,     // Dereference, like *ptr
    SYM_CONST       = 0520,     // Constant value, same as SYM_ABS
    SYM_EXPRESSION  = 0521,     // Expression to compute at load time
    SYM_PSECT_S     = 0560,     // Private sector-aligned (short name)
    SYM_CSECT_S     = 0570,     // Common sector-aligned (short name)
    SYM_ENTRY_L     = 0614,     // Entry, relocatable (long name)
    SYM_DENTRY_L    = 0615,     // Data entry, relocatable (long name)
    SYM_EXT_L       = 0630,     // External reference (long name)
    SYM_PRIVATE_L   = 0660,     // Private block (long name)
    SYM_PPAGE_L     = 0664,     // Private page-aligned (long name)
    SYM_COMMON_L    = 0670,     // Common block (long name)
    SYM_CPAGE_L     = 0674,     // Common page-aligned (long name)
    SYM_PSECT_L     = 0760,     // Private sector-aligned (long name)
    SYM_CSECT_L     = 0770,     // Common sector-aligned (long name)

    SYM_LONG_NAME   = 0200,     // Flag: long name
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
// Read object header only.
//
extern int obj_read_header(FILE *fd, obj_image_t *obj);

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

//
// Write object image to a file.
// Return negative in case of failure.
//
extern int obj_write(FILE *fd, obj_image_t *obj);

//
// Write object header only.
//
extern int obj_write_header(FILE *fd, obj_image_t *obj);
