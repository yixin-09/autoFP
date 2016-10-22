/* From wcet benchmark */
#include "../../fpdebug.h"
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
int main(int argc, char *argv[])
{
    printf("Test program: gsl_function\n");
    char input[800],*entptr;
    float a = 7.0;
    float b = strtod(argv[1],&entptr);
    float c,r;
    c =2.0;
  
    r=(-b+ sqrt(b*b-4.0*a*c))/(2.0*a);

    printf("%.10e\n",r);
    if (RUNNING_ON_VALGRIND) {
		printf("Running on valgrind\n");
	} else {
		printf("Not running on valgrind\n");
	}

	VALGRIND_PRINT_ERROR(&"val", &r);

	VALGRIND_DUMP_ERROR_GRAPH(&"root.vcg", &r);
    return 0;
}
