#include "leap.h"

bool divisible_by(int num, int divisor);

bool leap::is_leap_year(int year) {
    return (divisible_by(year, 4) && !(divisible_by(year, 100))) ||
        divisible_by(year, 400);
}

bool divisible_by(int num, int divisor) {
    return num % divisor == 0;
}
