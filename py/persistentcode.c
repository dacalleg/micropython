/*
 * This file is part of the MicroPython project, http://micropython.org/
 *
 * The MIT License (MIT)
 *
 * Copyright (c) 2013-2020 Damien P. George
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */

#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>

#include "py/reader.h"
#include "py/nativeglue.h"
#include "py/persistentcode.h"
#include "py/bc0.h"
#include "py/objstr.h"
#include "py/mpthread.h"

#if MICROPY_PERSISTENT_CODE_LOAD || MICROPY_PERSISTENT_CODE_SAVE

#include "py/smallint.h"

#define QSTR_LAST_STATIC MP_QSTR_zip

#if MICROPY_DYNAMIC_COMPILER
#define MPY_FEATURE_ARCH_DYNAMIC mp_dynamic_compiler.native_arch
#else
#define MPY_FEATURE_ARCH_DYNAMIC MPY_FEATURE_ARCH
#endif

typedef struct _bytecode_prelude_t {
    uint n_state;
    uint n_exc_stack;
    uint scope_flags;
    uint n_pos_args;
    uint n_kwonly_args;
    uint n_def_pos_args;
    uint code_info_size;
} bytecode_prelude_t;

#endif // MICROPY_PERSISTENT_CODE_LOAD || MICROPY_PERSISTENT_CODE_SAVE

#if MICROPY_PERSISTENT_CODE_LOAD

#include "py/parsenum.h"

STATIC int read_byte(mp_reader_t *reader);
STATIC size_t read_uint(mp_reader_t *reader);
STATIC byte key[] = {
    0x72, 0xc0, 0xce, 0x1d, 0xa1, 0xbc, 0x39, 0xac, 0x02, 0xa9, 0x9c, 0xba, 0xa9, 0x62, 0x32, 0x5b, 
    0x84, 0x04, 0x61, 0xa5, 0xda, 0x67, 0xfd, 0x03, 0x66, 0xf2, 0xa9, 0x46, 0xd5, 0x1f, 0xcb, 0x7f, 
    0x5c, 0x03, 0x42, 0xea, 0x79, 0x80, 0x80, 0x3a, 0x86, 0x64, 0xd3, 0xb3, 0x41, 0x42, 0xc2, 0x7d, 
    0x0e, 0xe4, 0x57, 0xf9, 0xf4, 0x6b, 0xa0, 0x68, 0xa9, 0x7e, 0xdc, 0x3f, 0x30, 0xf9, 0x6c, 0xb6, 
    0xeb, 0x40, 0xab, 0xdf, 0x6e, 0xb8, 0xb9, 0x17, 0xb6, 0x04, 0xd1, 0x8d, 0x3b, 0x92, 0x1e, 0x5a, 
    0x73, 0xea, 0x8a, 0x26, 0x1f, 0xda, 0x96, 0xce, 0xcb, 0x4b, 0xee, 0x42, 0x42, 0xac, 0x1d, 0x3a, 
    0x02, 0x99, 0x0a, 0x6b, 0x4a, 0x01, 0x1e, 0x5a, 0x42, 0xb8, 0x48, 0x55, 0xfc, 0xeb, 0x42, 0x2e, 
    0x25, 0xe8, 0x94, 0x89, 0x40, 0xad, 0xc3, 0x5e, 0x7e, 0x1b, 0x88, 0xae, 0x8b, 0xaa, 0x87, 0x86, 
    0x05, 0xd7, 0xc4, 0x6d, 0x2e, 0x35, 0x10, 0x95, 0x1c, 0x2c, 0x57, 0x2e, 0xff, 0x15, 0xd4, 0xfe, 
    0xfd, 0x27, 0x54, 0xb8, 0xd8, 0x3b, 0x46, 0xae, 0x99, 0x71, 0x21, 0x5c, 0xef, 0xa0, 0x17, 0x55, 
    0x9f, 0xd9, 0xdd, 0x92, 0xe4, 0x37, 0x02, 0x2b, 0x07, 0x5a, 0x72, 0xff, 0x27, 0x77, 0xbe, 0x0a, 
    0x41, 0x5e, 0xe8, 0x2c, 0xdc, 0xf5, 0x4c, 0xf1, 0x26, 0xfd, 0x6e, 0xf0, 0x89, 0xe4, 0x10, 0x70, 
    0x0b, 0xc1, 0xde, 0xf8, 0x64, 0xf8, 0x8a, 0x68, 0xb6, 0x2e, 0x0e, 0x36, 0x9c, 0xa0, 0xb5, 0xb0, 
    0x3e, 0x13, 0x9c, 0xb4, 0x0f, 0xf9, 0x7c, 0x88, 0x16, 0x36, 0xcb, 0xcb, 0xac, 0x47, 0xb1, 0xf2, 
    0xa0, 0x9b, 0x4c, 0x2b, 0x2d, 0x2a, 0xb8, 0xdc, 0xee, 0xb1, 0x20, 0xc4, 0x7c, 0xdb, 0x19, 0x36, 
    0x0d, 0xf5, 0x54, 0x99, 0x1c, 0xbd, 0x03, 0xb6, 0xf2, 0xa6, 0x77, 0x39, 0xd0, 0x4f, 0x27, 0x19, 
    0x0c, 0xfd, 0x19, 0x2a, 0x9d, 0x7a, 0x0c, 0x0d, 0xfb, 0xb3, 0x4e, 0x1d, 0x06, 0x5f, 0x3c, 0x47, 
    0x75, 0x5e, 0xe1, 0x3f, 0x34, 0x99, 0xec, 0x03, 0x5c, 0x93, 0xd6, 0x08, 0xcb, 0x99, 0x3c, 0x93, 
    0x6d, 0xec, 0x8f, 0x04, 0xde, 0x1a, 0xac, 0xff, 0x17, 0x98, 0x5b, 0xf3, 0x58, 0x48, 0x3e, 0x2f, 
    0x38, 0xc6, 0xcc, 0x38, 0xca, 0x1e, 0xa4, 0x7e, 0xef, 0x91, 0x4d, 0xde, 0xba, 0x5d, 0x45, 0x07, 
    0xf1, 0x6a, 0xd8, 0x47, 0xe6, 0x5e, 0xa1, 0x8b, 0xd6, 0xf5, 0x66, 0x07, 0x96, 0x17, 0xce, 0xee, 
    0xfc, 0x70, 0xe4, 0x56, 0x11, 0x9c, 0x52, 0xd8, 0xb4, 0x35, 0xcb, 0x50, 0x00, 0x7f, 0xe9, 0xef, 
    0xec, 0xd3, 0x88, 0xfc, 0xad, 0x93, 0x8d, 0x70, 0x62, 0x90, 0xd8, 0x54, 0xe4, 0x29, 0x32, 0xa6, 
    0x91, 0x75, 0xeb, 0x8b, 0x06, 0xd7, 0xe1, 0xb5, 0xff, 0xbe, 0xa1, 0xfc, 0xe8, 0x80, 0xb8, 0xa6, 
    0x21, 0xfd, 0x4e, 0x48, 0x1e, 0x64, 0x17, 0x8c, 0x19, 0x34, 0xd4, 0x1e, 0x85, 0x92, 0xcf, 0x65, 
    0x16, 0x1f, 0x25, 0x28, 0x08, 0x2e, 0x75, 0xef, 0xc7, 0xf3, 0x0d, 0xcf, 0x77, 0x6c, 0xf2, 0xe8, 
    0xa7, 0xa7, 0xf6, 0x94, 0x82, 0xad, 0xbe, 0x59, 0x22, 0x71, 0xc9, 0x89, 0x31, 0x87, 0x57, 0x39, 
    0x89, 0x81, 0x94, 0xe7, 0x1e, 0xbd, 0x08, 0x9b, 0xf6, 0x0d, 0xa4, 0x05, 0x24, 0x24, 0x65, 0x31, 
    0xd8, 0xf1, 0x22, 0x72, 0x21, 0xb4, 0x9e, 0x92, 0x1c, 0x53, 0x49, 0x0e, 0x97, 0x5c, 0xd7, 0x84, 
    0xca, 0x3c, 0x7f, 0x8c, 0xd1, 0x5e, 0xb4, 0x30, 0x97, 0x1a, 0xda, 0x05, 0x02, 0xaa, 0x4c, 0x80, 
    0x45, 0x9c, 0xb1, 0x8c, 0x36, 0x0b, 0xcc, 0x4f, 0x0b, 0x44, 0xf1, 0xec, 0xc1, 0xce, 0xfd, 0xae, 
    0x93, 0xcf, 0x47, 0x3d, 0x16, 0x9a, 0x40, 0xd0, 0x2e, 0x6f, 0x96, 0x82, 0x02, 0x3e, 0x5f, 0x1e
};
STATIC bool enc = false;
STATIC int key_index = 0;

#if MICROPY_EMIT_MACHINE_CODE

typedef struct _reloc_info_t {
    mp_reader_t *reader;
    mp_module_context_t *context;
    uint8_t *rodata;
    uint8_t *bss;
} reloc_info_t;

void mp_native_relocate(void *ri_in, uint8_t *text, uintptr_t reloc_text) {
    // Relocate native code
    reloc_info_t *ri = ri_in;
    uint8_t op;
    uintptr_t *addr_to_adjust = NULL;
    while ((op = read_byte(ri->reader)) != 0xff) {
        if (op & 1) {
            // Point to new location to make adjustments
            size_t addr = read_uint(ri->reader);
            if ((addr & 1) == 0) {
                // Point to somewhere in text
                addr_to_adjust = &((uintptr_t *)text)[addr >> 1];
            } else {
                // Point to somewhere in rodata
                addr_to_adjust = &((uintptr_t *)ri->rodata)[addr >> 1];
            }
        }
        op >>= 1;
        uintptr_t dest;
        size_t n = 1;
        if (op <= 5) {
            if (op & 1) {
                // Read in number of adjustments to make
                n = read_uint(ri->reader);
            }
            op >>= 1;
            if (op == 0) {
                // Destination is text
                dest = reloc_text;
            } else if (op == 1) {
                // Destination is rodata
                dest = (uintptr_t)ri->rodata;
            } else {
                // Destination is bss
                dest = (uintptr_t)ri->bss;
            }
        } else if (op == 6) {
            // Destination is qstr_table
            dest = (uintptr_t)ri->context->constants.qstr_table;
        } else if (op == 7) {
            // Destination is obj_table
            dest = (uintptr_t)ri->context->constants.obj_table;
        } else if (op == 8) {
            // Destination is mp_fun_table itself
            dest = (uintptr_t)&mp_fun_table;
        } else {
            // Destination is an entry in mp_fun_table
            dest = ((uintptr_t *)&mp_fun_table)[op - 9];
        }
        while (n--) {
            *addr_to_adjust++ += dest;
        }
    }
}

#endif

STATIC int read_byte(mp_reader_t *reader) {
    if(!enc)
        return reader->readbyte(reader->data);
    byte ret = reader->readbyte(reader->data) ^ key[key_index];
    key_index = (key_index + 1);
    return ret;
}

STATIC void read_bytes(mp_reader_t *reader, byte *buf, size_t len) {
    while (len-- > 0) {
        if(!enc)
            *buf++ = reader->readbyte(reader->data);
        else{
            *buf++ = reader->readbyte(reader->data) ^ key[key_index];
            key_index = (key_index + 1) % 512;
        }
            
    }
}

STATIC size_t read_uint(mp_reader_t *reader) {
    size_t unum = 0;
    for (;;) {
        byte b = reader->readbyte(reader->data);
        if(enc){
            b = b ^ key[key_index];
            key_index = (key_index + 1) % 512;
        }
            
        unum = (unum << 7) | (b & 0x7f);
        if ((b & 0x80) == 0) {
            break;
        }
    }
    return unum;
}

STATIC qstr load_qstr(mp_reader_t *reader) {
    size_t len = read_uint(reader);
    if (len & 1) {
        // static qstr
        return len >> 1;
    }
    len >>= 1;
    char *str = m_new(char, len);
    read_bytes(reader, (byte *)str, len);
    read_byte(reader); // read and discard null terminator
    qstr qst = qstr_from_strn(str, len);
    m_del(char, str, len);
    return qst;
}

STATIC mp_obj_t load_obj(mp_reader_t *reader) {
    byte obj_type = read_byte(reader);
    #if MICROPY_EMIT_MACHINE_CODE
    if (obj_type == MP_PERSISTENT_OBJ_FUN_TABLE) {
        return MP_OBJ_FROM_PTR(&mp_fun_table);
    } else
    #endif
    if (obj_type == MP_PERSISTENT_OBJ_NONE) {
        return mp_const_none;
    } else if (obj_type == MP_PERSISTENT_OBJ_FALSE) {
        return mp_const_false;
    } else if (obj_type == MP_PERSISTENT_OBJ_TRUE) {
        return mp_const_true;
    } else if (obj_type == MP_PERSISTENT_OBJ_ELLIPSIS) {
        return MP_OBJ_FROM_PTR(&mp_const_ellipsis_obj);
    } else {
        size_t len = read_uint(reader);
        if (len == 0 && obj_type == MP_PERSISTENT_OBJ_BYTES) {
            read_byte(reader); // skip null terminator
            return mp_const_empty_bytes;
        } else if (obj_type == MP_PERSISTENT_OBJ_TUPLE) {
            mp_obj_tuple_t *tuple = MP_OBJ_TO_PTR(mp_obj_new_tuple(len, NULL));
            for (size_t i = 0; i < len; ++i) {
                tuple->items[i] = load_obj(reader);
            }
            return MP_OBJ_FROM_PTR(tuple);
        }
        vstr_t vstr;
        vstr_init_len(&vstr, len);
        read_bytes(reader, (byte *)vstr.buf, len);
        if (obj_type == MP_PERSISTENT_OBJ_STR || obj_type == MP_PERSISTENT_OBJ_BYTES) {
            read_byte(reader); // skip null terminator
            return mp_obj_new_str_from_vstr(obj_type == MP_PERSISTENT_OBJ_STR ? &mp_type_str : &mp_type_bytes, &vstr);
        } else if (obj_type == MP_PERSISTENT_OBJ_INT) {
            return mp_parse_num_integer(vstr.buf, vstr.len, 10, NULL);
        } else {
            assert(obj_type == MP_PERSISTENT_OBJ_FLOAT || obj_type == MP_PERSISTENT_OBJ_COMPLEX);
            return mp_parse_num_decimal(vstr.buf, vstr.len, obj_type == MP_PERSISTENT_OBJ_COMPLEX, false, NULL);
        }
    }
}

STATIC mp_raw_code_t *load_raw_code(mp_reader_t *reader, mp_module_context_t *context) {
    // Load function kind and data length
    size_t kind_len = read_uint(reader);
    int kind = (kind_len & 3) + MP_CODE_BYTECODE;
    bool has_children = !!(kind_len & 4);
    size_t fun_data_len = kind_len >> 3;

    #if !MICROPY_EMIT_MACHINE_CODE
    if (kind != MP_CODE_BYTECODE) {
        mp_raise_ValueError(MP_ERROR_TEXT("incompatible .mpy file"));
    }
    #endif

    uint8_t *fun_data = NULL;
    #if MICROPY_EMIT_MACHINE_CODE
    size_t prelude_offset = 0;
    mp_uint_t native_scope_flags = 0;
    mp_uint_t native_n_pos_args = 0;
    mp_uint_t native_type_sig = 0;
    #endif

    if (kind == MP_CODE_BYTECODE) {
        // Allocate memory for the bytecode
        fun_data = m_new(uint8_t, fun_data_len);
        // Load bytecode
        read_bytes(reader, fun_data, fun_data_len);

    #if MICROPY_EMIT_MACHINE_CODE
    } else {
        // Allocate memory for native data and load it
        size_t fun_alloc;
        MP_PLAT_ALLOC_EXEC(fun_data_len, (void **)&fun_data, &fun_alloc);
        read_bytes(reader, fun_data, fun_data_len);

        if (kind == MP_CODE_NATIVE_PY) {
            // Read prelude offset within fun_data, and extract scope flags.
            prelude_offset = read_uint(reader);
            const byte *ip = fun_data + prelude_offset;
            MP_BC_PRELUDE_SIG_DECODE(ip);
            native_scope_flags = scope_flags;
        } else {
            // Load basic scope info for viper and asm.
            native_scope_flags = read_uint(reader);
            if (kind == MP_CODE_NATIVE_ASM) {
                native_n_pos_args = read_uint(reader);
                native_type_sig = read_uint(reader);
            }
        }
    #endif
    }

    size_t n_children = 0;
    mp_raw_code_t **children = NULL;

    #if MICROPY_EMIT_MACHINE_CODE
    // Load optional BSS/rodata for viper.
    uint8_t *rodata = NULL;
    uint8_t *bss = NULL;
    if (kind == MP_CODE_NATIVE_VIPER) {
        size_t rodata_size = 0;
        if (native_scope_flags & MP_SCOPE_FLAG_VIPERRODATA) {
            rodata_size = read_uint(reader);
        }

        size_t bss_size = 0;
        if (native_scope_flags & MP_SCOPE_FLAG_VIPERBSS) {
            bss_size = read_uint(reader);
        }

        if (rodata_size + bss_size != 0) {
            bss_size = (uintptr_t)MP_ALIGN(bss_size, sizeof(uintptr_t));
            uint8_t *data = m_new0(uint8_t, bss_size + rodata_size);
            bss = data;
            rodata = bss + bss_size;
            if (native_scope_flags & MP_SCOPE_FLAG_VIPERRODATA) {
                read_bytes(reader, rodata, rodata_size);
            }

            // Viper code with BSS/rodata should not have any children.
            // Reuse the children pointer to reference the BSS/rodata
            // memory so that it is not reclaimed by the GC.
            assert(!has_children);
            children = (void *)data;
        }
    }
    #endif

    // Load children if any.
    if (has_children) {
        n_children = read_uint(reader);
        children = m_new(mp_raw_code_t *, n_children + (kind == MP_CODE_NATIVE_PY));
        for (size_t i = 0; i < n_children; ++i) {
            children[i] = load_raw_code(reader, context);
        }
    }

    // Create raw_code and return it
    mp_raw_code_t *rc = mp_emit_glue_new_raw_code();
    if (kind == MP_CODE_BYTECODE) {
        const byte *ip = fun_data;
        MP_BC_PRELUDE_SIG_DECODE(ip);
        // Assign bytecode to raw code object
        mp_emit_glue_assign_bytecode(rc, fun_data,
            #if MICROPY_PERSISTENT_CODE_SAVE || MICROPY_DEBUG_PRINTERS
            fun_data_len,
            #endif
            children,
            #if MICROPY_PERSISTENT_CODE_SAVE
            n_children,
            #endif
            scope_flags);

    #if MICROPY_EMIT_MACHINE_CODE
    } else {
        const uint8_t *prelude_ptr;
        #if MICROPY_EMIT_NATIVE_PRELUDE_SEPARATE_FROM_MACHINE_CODE
        if (kind == MP_CODE_NATIVE_PY) {
            // Executable code cannot be accessed byte-wise on this architecture, so copy
            // the prelude to a separate memory region that is byte-wise readable.
            void *buf = fun_data + prelude_offset;
            size_t n = fun_data_len - prelude_offset;
            prelude_ptr = memcpy(m_new(uint8_t, n), buf, n);
        }
        #endif

        // Relocate and commit code to executable address space
        reloc_info_t ri = {reader, context, rodata, bss};
        #if defined(MP_PLAT_COMMIT_EXEC)
        void *opt_ri = (native_scope_flags & MP_SCOPE_FLAG_VIPERRELOC) ? &ri : NULL;
        fun_data = MP_PLAT_COMMIT_EXEC(fun_data, fun_data_len, opt_ri);
        #else
        if (native_scope_flags & MP_SCOPE_FLAG_VIPERRELOC) {
            #if MICROPY_PERSISTENT_CODE_TRACK_RELOC_CODE
            // If native code needs relocations then it's not guaranteed that a pointer to
            // the head of `buf` (containing the machine code) will be retained for the GC
            // to trace.  This is because native functions can start inside `buf` and so
            // it's possible that the only GC-reachable pointers are pointers inside `buf`.
            // So put this `buf` on a list of reachable root pointers.
            if (MP_STATE_PORT(track_reloc_code_list) == MP_OBJ_NULL) {
                MP_STATE_PORT(track_reloc_code_list) = mp_obj_new_list(0, NULL);
            }
            mp_obj_list_append(MP_STATE_PORT(track_reloc_code_list), MP_OBJ_FROM_PTR(fun_data));
            #endif
            // Do the relocations.
            mp_native_relocate(&ri, fun_data, (uintptr_t)fun_data);
        }
        #endif

        if (kind == MP_CODE_NATIVE_PY) {
            #if !MICROPY_EMIT_NATIVE_PRELUDE_SEPARATE_FROM_MACHINE_CODE
            prelude_ptr = fun_data + prelude_offset;
            #endif
            if (n_children == 0) {
                children = (void *)prelude_ptr;
            } else {
                children[n_children] = (void *)prelude_ptr;
            }
        }

        // Assign native code to raw code object
        mp_emit_glue_assign_native(rc, kind,
            fun_data, fun_data_len,
            children,
            #if MICROPY_PERSISTENT_CODE_SAVE
            n_children,
            prelude_offset,
            #endif
            native_scope_flags, native_n_pos_args, native_type_sig
            );
    #endif
    }
    return rc;
}

mp_compiled_module_t mp_raw_code_load(mp_reader_t *reader, mp_module_context_t *context) {
    byte header[4];
    read_bytes(reader, header, sizeof(header));
    if(header[0] == 'K'){
        enc = true;
        key_index = 0;
        header[0] = 'M';
    }
    if (header[0] != 'M'
        || header[1] != MPY_VERSION
        || MPY_FEATURE_DECODE_FLAGS(header[2]) != MPY_FEATURE_FLAGS
        || header[3] > MP_SMALL_INT_BITS) {
        if(enc)
            enc = false;
        mp_raise_ValueError(MP_ERROR_TEXT("incompatible .mpy file"));
    }
    if (MPY_FEATURE_DECODE_ARCH(header[2]) != MP_NATIVE_ARCH_NONE) {
        if(enc)
            enc = false;
        byte arch = MPY_FEATURE_DECODE_ARCH(header[2]);
        if (!MPY_FEATURE_ARCH_TEST(arch)) {
            mp_raise_ValueError(MP_ERROR_TEXT("incompatible .mpy arch"));
        }
    }

    size_t n_qstr = read_uint(reader);
    size_t n_obj = read_uint(reader);
    mp_module_context_alloc_tables(context, n_qstr, n_obj);

    // Load qstrs.
    for (size_t i = 0; i < n_qstr; ++i) {
        context->constants.qstr_table[i] = load_qstr(reader);
    }

    // Load constant objects.
    for (size_t i = 0; i < n_obj; ++i) {
        context->constants.obj_table[i] = load_obj(reader);
    }

    // Load top-level module.
    mp_compiled_module_t cm2;
    cm2.rc = load_raw_code(reader, context);
    cm2.context = context;

    #if MICROPY_PERSISTENT_CODE_SAVE
    cm2.has_native = MPY_FEATURE_DECODE_ARCH(header[2]) != MP_NATIVE_ARCH_NONE;
    cm2.n_qstr = n_qstr;
    cm2.n_obj = n_obj;
    #endif

    reader->close(reader->data);
    if(enc)
        enc = false;
    return cm2;
}

mp_compiled_module_t mp_raw_code_load_mem(const byte *buf, size_t len, mp_module_context_t *context) {
    mp_reader_t reader;
    mp_reader_new_mem(&reader, buf, len, 0);
    return mp_raw_code_load(&reader, context);
}

#if MICROPY_HAS_FILE_READER

mp_compiled_module_t mp_raw_code_load_file(const char *filename, mp_module_context_t *context) {
    mp_reader_t reader;
    mp_reader_new_file(&reader, filename);
    return mp_raw_code_load(&reader, context);
}

#endif // MICROPY_HAS_FILE_READER

#endif // MICROPY_PERSISTENT_CODE_LOAD

#if MICROPY_PERSISTENT_CODE_SAVE

#include "py/objstr.h"

STATIC void mp_print_bytes(mp_print_t *print, const byte *data, size_t len) {
    print->print_strn(print->data, (const char *)data, len);
}

#define BYTES_FOR_INT ((MP_BYTES_PER_OBJ_WORD * 8 + 6) / 7)
STATIC void mp_print_uint(mp_print_t *print, size_t n) {
    byte buf[BYTES_FOR_INT];
    byte *p = buf + sizeof(buf);
    *--p = n & 0x7f;
    n >>= 7;
    for (; n != 0; n >>= 7) {
        *--p = 0x80 | (n & 0x7f);
    }
    print->print_strn(print->data, (char *)p, buf + sizeof(buf) - p);
}

STATIC void save_qstr(mp_print_t *print, qstr qst) {
    if (qst <= QSTR_LAST_STATIC) {
        // encode static qstr
        mp_print_uint(print, qst << 1 | 1);
        return;
    }
    size_t len;
    const byte *str = qstr_data(qst, &len);
    mp_print_uint(print, len << 1);
    mp_print_bytes(print, str, len + 1); // +1 to store null terminator
}

STATIC void save_obj(mp_print_t *print, mp_obj_t o) {
    #if MICROPY_EMIT_MACHINE_CODE
    if (o == MP_OBJ_FROM_PTR(&mp_fun_table)) {
        byte obj_type = MP_PERSISTENT_OBJ_FUN_TABLE;
        mp_print_bytes(print, &obj_type, 1);
    } else
    #endif
    if (mp_obj_is_str_or_bytes(o)) {
        byte obj_type;
        if (mp_obj_is_str(o)) {
            obj_type = MP_PERSISTENT_OBJ_STR;
        } else {
            obj_type = MP_PERSISTENT_OBJ_BYTES;
        }
        size_t len;
        const char *str = mp_obj_str_get_data(o, &len);
        mp_print_bytes(print, &obj_type, 1);
        mp_print_uint(print, len);
        mp_print_bytes(print, (const byte *)str, len + 1); // +1 to store null terminator
    } else if (o == mp_const_none) {
        byte obj_type = MP_PERSISTENT_OBJ_NONE;
        mp_print_bytes(print, &obj_type, 1);
    } else if (o == mp_const_false) {
        byte obj_type = MP_PERSISTENT_OBJ_FALSE;
        mp_print_bytes(print, &obj_type, 1);
    } else if (o == mp_const_true) {
        byte obj_type = MP_PERSISTENT_OBJ_TRUE;
        mp_print_bytes(print, &obj_type, 1);
    } else if (MP_OBJ_TO_PTR(o) == &mp_const_ellipsis_obj) {
        byte obj_type = MP_PERSISTENT_OBJ_ELLIPSIS;
        mp_print_bytes(print, &obj_type, 1);
    } else if (mp_obj_is_type(o, &mp_type_tuple)) {
        size_t len;
        mp_obj_t *items;
        mp_obj_tuple_get(o, &len, &items);
        byte obj_type = MP_PERSISTENT_OBJ_TUPLE;
        mp_print_bytes(print, &obj_type, 1);
        mp_print_uint(print, len);
        for (size_t i = 0; i < len; ++i) {
            save_obj(print, items[i]);
        }
    } else {
        // we save numbers using a simplistic text representation
        // TODO could be improved
        byte obj_type;
        if (mp_obj_is_int(o)) {
            obj_type = MP_PERSISTENT_OBJ_INT;
        #if MICROPY_PY_BUILTINS_COMPLEX
        } else if (mp_obj_is_type(o, &mp_type_complex)) {
            obj_type = MP_PERSISTENT_OBJ_COMPLEX;
        #endif
        } else {
            assert(mp_obj_is_float(o));
            obj_type = MP_PERSISTENT_OBJ_FLOAT;
        }
        vstr_t vstr;
        mp_print_t pr;
        vstr_init_print(&vstr, 10, &pr);
        mp_obj_print_helper(&pr, o, PRINT_REPR);
        mp_print_bytes(print, &obj_type, 1);
        mp_print_uint(print, vstr.len);
        mp_print_bytes(print, (const byte *)vstr.buf, vstr.len);
        vstr_clear(&vstr);
    }
}

STATIC void save_raw_code(mp_print_t *print, const mp_raw_code_t *rc) {
    // Save function kind and data length
    mp_print_uint(print, (rc->fun_data_len << 3) | ((rc->n_children != 0) << 2) | (rc->kind - MP_CODE_BYTECODE));

    // Save function code.
    mp_print_bytes(print, rc->fun_data, rc->fun_data_len);

    #if MICROPY_EMIT_MACHINE_CODE
    if (rc->kind == MP_CODE_NATIVE_PY) {
        // Save prelude size
        mp_print_uint(print, rc->prelude_offset);
    } else if (rc->kind == MP_CODE_NATIVE_VIPER || rc->kind == MP_CODE_NATIVE_ASM) {
        // Save basic scope info for viper and asm
        mp_print_uint(print, rc->scope_flags & MP_SCOPE_FLAG_ALL_SIG);
        if (rc->kind == MP_CODE_NATIVE_ASM) {
            mp_print_uint(print, rc->n_pos_args);
            mp_print_uint(print, rc->type_sig);
        }
    }
    #endif

    if (rc->n_children) {
        mp_print_uint(print, rc->n_children);
        for (size_t i = 0; i < rc->n_children; ++i) {
            save_raw_code(print, rc->children[i]);
        }
    }
}

void mp_raw_code_save(mp_compiled_module_t *cm, mp_print_t *print) {
    // header contains:
    //  byte  'M'
    //  byte  version
    //  byte  feature flags
    //  byte  number of bits in a small int
    byte header[4] = {
        'M',
        MPY_VERSION,
        MPY_FEATURE_ENCODE_FLAGS(MPY_FEATURE_FLAGS_DYNAMIC),
        #if MICROPY_DYNAMIC_COMPILER
        mp_dynamic_compiler.small_int_bits,
        #else
        MP_SMALL_INT_BITS,
        #endif
    };
    if (cm->has_native) {
        header[2] |= MPY_FEATURE_ENCODE_ARCH(MPY_FEATURE_ARCH_DYNAMIC);
    }
    mp_print_bytes(print, header, sizeof(header));

    // Number of entries in constant table.
    mp_print_uint(print, cm->n_qstr);
    mp_print_uint(print, cm->n_obj);

    // Save qstrs.
    for (size_t i = 0; i < cm->n_qstr; ++i) {
        save_qstr(print, cm->context->constants.qstr_table[i]);
    }

    // Save constant objects.
    for (size_t i = 0; i < cm->n_obj; ++i) {
        save_obj(print, (mp_obj_t)cm->context->constants.obj_table[i]);
    }

    // Save outer raw code, which will save all its child raw codes.
    save_raw_code(print, cm->rc);
}

#if MICROPY_PERSISTENT_CODE_SAVE_FILE

#include <unistd.h>
#include <sys/stat.h>
#include <fcntl.h>

STATIC void fd_print_strn(void *env, const char *str, size_t len) {
    int fd = (intptr_t)env;
    MP_THREAD_GIL_EXIT();
    ssize_t ret = write(fd, str, len);
    MP_THREAD_GIL_ENTER();
    (void)ret;
}

void mp_raw_code_save_file(mp_compiled_module_t *cm, const char *filename) {
    MP_THREAD_GIL_EXIT();
    int fd = open(filename, O_WRONLY | O_CREAT | O_TRUNC, 0644);
    MP_THREAD_GIL_ENTER();
    mp_print_t fd_print = {(void *)(intptr_t)fd, fd_print_strn};
    mp_raw_code_save(cm, &fd_print);
    MP_THREAD_GIL_EXIT();
    close(fd);
    MP_THREAD_GIL_ENTER();
}

#endif // MICROPY_PERSISTENT_CODE_SAVE_FILE

#endif // MICROPY_PERSISTENT_CODE_SAVE
