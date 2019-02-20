#include "triangle.h"

class Triangle {
    double a;
    double b;
    double c;

public:
    Triangle(double a, double b, double c) : a(a), b(b), c(c) {}

    triangle::flavor kind();
    bool isValidTringle();
    bool isInvalidTriangle();

private:
    bool allSidesPositive();
    bool allSidesLessThanSumOfOthers();
    bool allSidesEqual();
    bool atLeastTwoSidesEqual();
};



namespace triangle {
    flavor kind(double a, double b, double c) {
        Triangle t(a, b, c);
        return t.kind();
    }
}



triangle::flavor Triangle::kind() {
    if (isInvalidTriangle()) {
        throw std::domain_error("Invalid triangle");
    }
    if (allSidesEqual()) {
        return triangle::equilateral;
    } else if (atLeastTwoSidesEqual()) {
        return triangle::isosceles;
    } else {
        return triangle::scalene;
    }
}

bool Triangle::isValidTringle() {
    return allSidesPositive()
        && allSidesLessThanSumOfOthers();
}

bool Triangle::isInvalidTriangle() {
    return !isValidTringle();
}

bool Triangle::allSidesPositive() {
    return (a > 0)
        && (b > 0)
        && (c > 0);
}

bool Triangle::allSidesLessThanSumOfOthers() {
    return (a < b + c)
        && (b < c + a)
        && (c < a + b);
}

bool Triangle::allSidesEqual() {
    return (a == b)
        && (b == c);
}

bool Triangle::atLeastTwoSidesEqual() {
    return (a == b)
        || (b == c)
        || (c == a);
}
