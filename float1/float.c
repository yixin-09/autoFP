/* From wcet benchmark */
#include "../../fpdebug.h"
#include <stdio.h>
#include <stdlib.h>
int main(int argc, char *argv[])
{
    printf("Test program: gsl_function\n");
    char input[800],*entptr;
    double x = strtod(argv[1],&entptr);
    printf("%.10e\n",x);
    int i;
    int n = 8192;
    double y,z;
    y=z=n;
    if (x<0) x=-x;
    for(i=0;i<n;i++)
	y=y+x;
    y=y/z;
    double r=(0.125f+x)/(y-0.875f);
    printf("%.10e\n",r);
    if (RUNNING_ON_VALGRIND) {
		printf("Running on valgrind\n");
	} else {
		printf("Not running on valgrind\n");
	}

	VALGRIND_PRINT_ERROR(&"val", &r);

	VALGRIND_DUMP_ERROR_GRAPH(&"sqrt2.vcg", &r);
    return 0;
}
