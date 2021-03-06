/* From wcet benchmark */
#include "../../../fpdebug.h"
#include <stdio.h>
#include <stdlib.h>
#include "sqr2.h"
int main(int argc, char *argv[])
{
    printf("Test program: gsl_function\n");
    char input[800],*entptr;
    float r = 1.8408964152537146-1;
    r = r+1;
    if (r>0.5){
	printf("hit");}
    else 
	printf("miss");
    printf("%.20e\n",r);
    if (RUNNING_ON_VALGRIND) {
		printf("Running on valgrind\n");
	} else {
		printf("Not running on valgrind\n");
	}

	VALGRIND_PRINT_ERROR(&"val", &r);

	VALGRIND_DUMP_ERROR_GRAPH(&"sqrt2.vcg", &r);
    return 0;
}
