#include <stdio.h>
#include <math.h>
double semf(double a, double b){

    // Constants from formula, units in MeV(millions of electron volts)
    double a1 = 15.67;          
    double a2 = 17.23;
    double a3 = 0.75;
    double a4 = 93.2;
    double a5;
    if(fmod(b,2) == 0 && fmod((a - b),2) == 0)

        a5 = 12.0;

    else if(fmod(b,2) != 0 && fmod((a - b),2) != 0)

        a5 = -12.0;

    else

        a5 = 0;
    // Formula for to compute the binding energy
    double c =a1 * a - a2 * pow(a, 2.0/3.0) - a3 * (pow(b, 2.0) / pow(a, 1.0/3.0)) - a4 * (pow(a - 2.0 * b, 2.0) / a) + (a5 / pow(b, 0.5));

    // Formula for to compute the binding energy per nucleon
    double d = c/a;
    return d;
}
