/* From wcet benchmark */
#include "../../fpdebug.h"
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
int main(int argc, char *argv[])
{
    printf("Test program: gsl_function\n");
    char input[800],*entptr;
    double x = strtod(argv[1],&entptr);
    printf("%.10e\n",x);
    int i;
    int n = 8192;
    double y,z,b[20];
    y=z=n;
    if (x<0) x=1;
    for(i=0;i<n;i++)
	{y=y+x;
	x=1;
	
	n=n;}

    y=y/z;
    b[0] = cos(z)+sin(z+1)+1;
    if (z+100>300){double r=(0.125+x+n)/(y-b[0]+(-0.875));}
    return 0;
}
