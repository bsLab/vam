
#include "_math.h"

doublereal azabs_(zr, zi)
doublereal *zr, *zi;
{
    /* System generated locals */
    doublereal ret_val;

    /* Builtin functions */
    double sqrt();

    /* Local variables */
    static doublereal q, s, u, v;

/* ***BEGIN PROLOGUE  AZABS */
/* ***REFER TO  ZBESH,ZBESI,ZBESJ,ZBESK,ZBESY,ZAIRY,ZBIRY */

/*     AZABS COMPUTES THE ABSOLUTE VALUE OR MAGNITUDE OF A DOUBLE */
/*     PRECISION COMPLEX VARIABLE CMPLX(ZR,ZI) */

/* ***ROUTINES CALLED  (NONE) */
/* ***END PROLOGUE  AZABS */
    u = abs(*zr);
    v = abs(*zi);
    s = u + v;
/* ----------------------------------------------------------------------- */
/*     S*1.0D0 MAKES AN UNNORMALIZED UNDERFLOW ON CDC MACHINES INTO A */
/*     TRUE FLOATING ZERO */
/* ----------------------------------------------------------------------- */
    s *= 1.;
    if (s == 0.) {
	goto L20;
    }
    if (u > v) {
	goto L10;
    }
    q = u / v;
    ret_val = v * sqrt(q * q + 1.);
    return ret_val;
L10:
    q = v / u;
    ret_val = u * sqrt(q * q + 1.);
    return ret_val;
L20:
    ret_val = 0.;
    return ret_val;
} /* azabs_ */

