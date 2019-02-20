#include <iostream>

namespace triangle {
    enum flavor {
        equilateral,
        isosceles,
        scalene,
    };

    flavor kind(double a, double b, double c) {
        if (a > b + c || b > c + a || c > a + b
            || a < 0 || b < 0 || c < 0
            || a + b + c == 0) {
            throw std::domain_error("Invalid triangle");
        }
        if (a == b && b == c) {
            return equilateral;
        } else if (a == b || b == c || c == a) {
            return isosceles;
        } else {
            return scalene;
        }
    }
}
