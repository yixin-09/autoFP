/* From wcet benchmark */
#include "../../fpdebug.h"
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "physic.h"
int main(int argc, char *argv[])
{
    printf("Test program: gsl_function\n");
    char input[800],*entptr;
    double a = strtod(argv[1],&entptr);
    double b = strtod(argv[1],&entptr);
    double r = semf( a,  b);
    printf("%.10e\n",r);
    if (RUNNING_ON_VALGRIND) {
		printf("Running on valgrind\n");
	} else {
		printf("Not running on valgrind\n");
	}

	VALGRIND_PRINT_ERROR(&"val", &r);

	VALGRIND_DUMP_ERROR_GRAPH(&"sin.vcg", &r);
    return 0;
}
