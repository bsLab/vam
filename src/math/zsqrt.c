
#include "_math.h"

int azsqrt_(ar, ai, br, bi)
doublereal *ar, *ai, *br, *bi;
{
    /* Initialized data */

    static doublereal drt = .7071067811865475244008443621;
    static doublereal dpi = 3.141592653589793238462643383;

    /* Builtin functions */
    double sqrt(), atan(), cos(), sin();

    /* Local variables */
    extern doublereal azabs_();
    static doublereal zm, dtheta;

/* ***BEGIN PROLOGUE  AZSQRT */
/* ***REFER TO  ZBESH,ZBESI,ZBESJ,ZBESK,ZBESY,ZAIRY,ZBIRY */

/*     DOUBLE PRECISION COMPLEX SQUARE ROOT, B=CSQRT(A) */

/* ***ROUTINES CALLED  AZABS */
/* ***END PROLOGUE  AZSQRT */
    zm = azabs_(ar, ai);
    zm = sqrt(zm);
    if (*ar == 0.) {
	goto L10;
    }
    if (*ai == 0.) {
	goto L20;
    }
    dtheta = atan(*ai / *ar);
    if (dtheta <= 0.) {
	goto L40;
    }
    if (*ar < 0.) {
	dtheta -= dpi;
    }
    goto L50;
L10:
    if (*ai > 0.) {
	goto L60;
    }
    if (*ai < 0.) {
	goto L70;
    }
    *br = 0.;
    *bi = 0.;
    return 0;
L20:
    if (*ar > 0.) {
	goto L30;
    }
    *br = 0.;
    *bi = sqrt((abs(*ar)));
    return 0;
L30:
    *br = sqrt(*ar);
    *bi = 0.;
    return 0;
L40:
    if (*ar < 0.) {
	dtheta += dpi;
    }
L50:
    dtheta *= .5;
    *br = zm * cos(dtheta);
    *bi = zm * sin(dtheta);
    return 0;
L60:
    *br = zm * drt;
    *bi = zm * drt;
    return 0;
L70:
    *br = zm * drt;
    *bi = -zm * drt;
    return 0;
} /* azsqrt_ */

