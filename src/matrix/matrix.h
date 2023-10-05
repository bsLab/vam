/*
**      ==================================
**      OOOO   OOOO OOOO  O      O   OOOO
**      O   O  O    O     O     O O  O   O
**      O   O  O    O     O     O O  O   O
**      OOOO   OOOO OOOO  O     OOO  OOOO
**      O   O     O    O  O    O   O O   O
**      O   O     O    O  O    O   O O   O
**      OOOO   OOOO OOOO  OOOO O   O OOOO
**      ==================================
**      BSSLAB, Dr. Stefan Bosse sci@bsslab.de
**
**    PROTECTED BY AND DISTRIBUTED UNDER THE TERMS OF:
**    Free Software Foundation-Europe, GNU GPL License, Version 2
**
**    $AUTHORS:     Stefan Bosse
**    $INITIAL:     (C) 2005 BSSLAB
**    $CREATED:     17.7.2005
**    $VERSION:     1.01
**
**    $INFO:
**
**  VAM native matrix support (C-Layout).
**
**    $ENDOFINFO
**
*/


#define MAX_DIM 10

/*
** Keep consistent with Matrix.Matrix_type!!!
*/
enum matrix_type {
    M_AUTO,
    M_FLOAT,            /* caml double          */
    M_COMPLEX,          /* (M_FLOAT,M_FLOAT) tuples */
    M_FLOAT32,          /* 4 byte float         */
    M_FLOAT64,          /* 8 byte float         */
    M_INT,              /* caml integer         */
    M_INT8,             /* machtype int8        */
    M_INT16,            /* machtype int16       */
    M_INT32,            /* machtype int32       */
    M_INT64,            /* machtype int64       */
    M_BYTE              /* caml char <=> int    */
};

struct caml_matrix {
    int dim;
    int type;           /* matrix_type  */
    int size[MAX_DIM];
    unsigned char* data;
    int state;    
};
typedef struct caml_matrix caml_matrix_t,*caml_matrix_p;

extern value matrix_set(value matrix,value dim,value v);
extern value matrix_get(value matrix,value dim);
extern value matrix_create(value dim,value kind,value vinit);
extern void print_matrix_info(char *buf,char *end,caml_matrix_p);

#define Matrix_val(v) (*((struct caml_matrix **) Data_custom_val(v)))

/*
** C-style layout: row major, indices start at 0 
*/
#define MATRIX_OFF(m,index,offset) {                                    \
int i; char msg[200]; offset=0;                                         \
for (i = 0; i < m->dim; i++) {                                          \
    if ((unsigned long) (index[i]) >= (unsigned long) m->size[i]) {     \
        sprintf(msg,"Matrix: out-of-bound access: [dim %d] got %d, expected 0..%d: ", \
                 i+1,(int)index[i],(int) m->size[i]-1);                 \
        print_matrix_info(&msg[strlen(msg)],&msg[200],m);               \
        invalid_argument(msg);                                          \
    }                                                                   \
    offset = offset * m->size[i] + index[i];                            \
}}

#define MATRIX_INIT(m,tp,v) {                                           \
    tp *  data=(tp *)m->data;                                           \
    int i; int size=1;                                                  \
    for(i=0;i<m->dim;i++) size=size*m->size[i];                         \
    for(i=0;i<size;i++) {                                               \
        data[i]=(tp)v;                                                  \
}}

