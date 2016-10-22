#include <stdio.h>
#include <math.h>
double semf(double a, double b){

    // Constants from formula, units in MeV(millions of electron volts)
    double a1 = 15.67;          
    double a2 = 17.23;
    double a3 = 0.75;
    double a4 = 93.2;
    double a5,c,d;
    if(fmod(b,2.0) == 0 && fmod((a - b),2.0) == 0)

        a5 = 12.0;

    else if(fmod(b,2.0) != 0 && fmod((a - b),2.0) != 0)

        a5 = -12.0;

    else

        a5 = 0;
    // Formula for to compute the binding energy
    c =((15.67 * a - pow(a,((3.0)/(2.0))) * 17.23) - ((0.75 * pow(b,2.0))/(pow(a,((1.0)/(3.0)))) + 93.2 * (a * 1.0 - 4.0 * b))) - ((93.2 * (pow(b,(2.0 * 1.0)))/(a)) * pow((pow((-2.0),2.0)),(pow(1.0,2.0))) - (a5)/(pow(b,0.5)));

    // Formula for to compute the binding energy per nucleon
    d =c/a;
    return d;
}
