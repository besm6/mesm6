#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "stdobj.h"

//
// Read a 48-bit word at the current file position.
//
static uint64_t fread6(FILE *fd)
{
    uint64_t val = 0;
    int i;

    for (i = 0; i < 6; ++i) {
        val <<= 8;
        val |= getc(fd);
    }
    return val;
}

//
// Read a 48-bit word from data buffer.
//
static uint64_t data_read6(char **pdata, unsigned *pnbytes)
{
    uint64_t val = 0;
    int i;

    if (*pnbytes < 6)
        return 0;
    for (i = 0; i < 6; ++i) {
        val <<= 8;
        val |= (uint8_t) **pdata;
        (*pdata)++;
    }
    *pnbytes -= 6;
    return val;
}

//
// Decode image contents.
// Return negative in case of failure.
//
static int obj_decode(obj_image_t *obj)
{
    obj->nentries = 0;
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

//
// Read object image from a file.
// Return negative in case of failure.
//
int obj_read_fd(FILE *fd, obj_image_t *obj)
{
    // Read file contents.
    obj->nwords = 0;
    obj->word[obj->nwords++] = fread6(fd);
    for (;; obj->nwords++) {
        if (obj->nwords >= MAXSZ) {
            fprintf(stderr, "File too large\n");
            return -1;
        }
        obj->word[obj->nwords] = fread6(fd);
        if (feof(fd))
            break;
    }
    return obj_decode(obj);
}

//
// Read object image from a data buffer.
// Return negative in case of failure.
//
int obj_read_data(char *data, unsigned nbytes, obj_image_t *obj)
{
    // Read file contents.
    obj->nwords = 0;
    obj->word[obj->nwords++] = data_read6(&data, &nbytes);
    for (;; obj->nwords++) {
        if (obj->nwords >= MAXSZ) {
            fprintf(stderr, "File too large\n");
            return -1;
        }
        if (nbytes < 6)
            break;
        obj->word[obj->nwords] = data_read6(&data, &nbytes);
    }
    return obj_decode(obj);
}

//
// Allocate a copy if object image.
// Return NULL when failed.
//
obj_image_t *obj_copy(obj_image_t *from)
{
    unsigned nbytes = sizeof(obj_image_t) - sizeof(from->word) +
                      from->nwords * sizeof(from->word[0]);

    obj_image_t *to = (obj_image_t*) calloc(1, nbytes);

    if (to)
        memcpy(to, from, nbytes);
    return to;
}
