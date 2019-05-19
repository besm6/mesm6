#include <stdio.h>
#include "stdobj.h"

//
// Read a 48-bit word at the current file position.
//
static uint64_t freadw(FILE *fd)
{
    uint64_t val = 0;
    int i;

    for (i = 0; i < 6; ++i) {
        val <<= 8;
        val |= getc (fd);
    }
    return val;
}

//
// Read object image from a file.
// Return negative in case of failure.
//
int obj_read(const char *fname, obj_image_t *obj)
{
    FILE *fd;

    fd = fopen(fname, "r");
    if (!fd) {
        fprintf(stderr, "dis: %s not found\n", fname);
        return -1;
    }

    // Read file contents.
    obj->nwords = 0;
    obj->nentries = 0;
    obj->word[obj->nwords++] = freadw(fd);
    for (;; obj->nwords++) {
        if (obj->nwords >= MAXSZ) {
            fprintf(stderr, "File too large\n");
            return -1;
        }
        obj->word[obj->nwords] = freadw(fd);
        if (feof(fd))
            break;
    }
    fclose(fd);

    if (obj->word[0] != BESM6_MAGIC) {
        // Check file magic.
        fprintf(stderr, "Bad magic: %#jx\n", (intmax_t)obj->word[0]);
        return -1;
    }

    // Detect packed or unpacked header.
    obj->head_off = 1;
    if ((obj->word[2] >> 45) != 0) {

        // Skip entry table.
        while ((obj->word[obj->head_off + 1] >> 45) != 0) {
            obj->head_off += 2;
            obj->nentries++;
        }

        // Unpacked header.
        obj->head_len  = obj->word[obj->head_off];
        obj->sym_len   = obj->word[obj->head_off + 1];
        // Unknown: obj->word[obj->head_off + 2]
        obj->debug_len = obj->word[obj->head_off + 3];
        obj->long_len  = obj->word[obj->head_off + 4];
        obj->cmd_len   = obj->word[obj->head_off + 5];
        obj->bss_len   = obj->word[obj->head_off + 6];
        obj->const_len = obj->word[obj->head_off + 7];
        obj->data_len  = obj->word[obj->head_off + 8];
        obj->set_len   = obj->word[obj->head_off + 9];

        obj->cmd_off = obj->head_off + 10;
    } else {
        // Packed header.
        obj->head_len  = obj->word[obj->head_off] & 07777;
        obj->sym_len   = (obj->word[obj->head_off] >> 12) & 07777;
        obj->debug_len = (obj->word[obj->head_off] >> 36);

        obj->set_len  = obj->word[obj->head_off + 1] & 077777;
        obj->data_len = (obj->word[obj->head_off + 1] >> 15) & 077777;
        obj->long_len = (obj->word[obj->head_off + 1] >> 30) & 077777;

        obj->cmd_len   = obj->word[obj->head_off + 2] & 077777;
        obj->bss_len   = (obj->word[obj->head_off + 2] >> 15) & 077777;
        obj->const_len = (obj->word[obj->head_off + 2] >> 30) & 077777;

        obj->cmd_off = obj->head_off + 3;
    }

    obj->table_off = obj->cmd_off + obj->cmd_len + obj->const_len + obj->data_len + obj->set_len;
    obj->long_off = obj->table_off + obj->head_len + obj->sym_len;
    obj->debug_off = obj->long_off + obj->long_len;
    obj->comment_off = obj->debug_off + obj->debug_len;
    return 0;
}
