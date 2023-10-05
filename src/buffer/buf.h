

/*
** State of a buffer 
*/

#define BUF_MASTER  0x10
#define BUF_SLAVE   0x20

/*
** A finalize arrived for the master, but there are still slaves.
** Mark the master. After the last slave was finalized, free
** the master memory.
*/

#define BUF_DYING   0x40    

/*
** The master and its slave buffers are put in a double linked
** chain.
*/

struct caml_buf {
    int size;
    unsigned char* data;  
    int state;
    struct caml_buf *prev;
    struct caml_buf *next;
};

#define Buf_val(v) (*((struct caml_buf **) Data_custom_val(v)))


