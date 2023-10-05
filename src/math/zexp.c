
#include "_math.h"

int azexp_(ar, ai, br, bi)
doublereal *ar, *ai, *br, *bi;
{
    /* Builtin functions */
    double exp(), cos(), sin();

    /* Local variables */
    static doublereal ca, cb, zm;

/* ***BEGIN PROLOGUE  AZEXP */
/* ***REFER TO  ZBESH,ZBESI,ZBESJ,ZBESK,ZBESY,ZAIRY,ZBIRY */

/*     DOUBLE PRECISION COMPLEX EXPONENTIAL FUNCTION B=EXP(A) */

/* ***ROUTINES CALLED  (NONE) */
/* ***END PROLOGUE  AZEXP */
    zm = exp(*ar);
    ca = zm * cos(*ai);
    cb = zm * sin(*ai);
    *br = ca;
    *bi = cb;
    return 0;
} /* azexp_ */

