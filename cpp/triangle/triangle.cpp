#include "triangle.h"
#include <vector>
#include <algorithm>

class Triangle {
    const double a;
    const double b;
    const double c;

public:
    static Triangle create(double x, double y, double z);

    triangle::flavor kind() const;
    bool isValidTringle() const;
    bool isInvalidTriangle() const;

private:
    Triangle(double a, double b, double c) : a(a), b(b), c(c) {};
    bool allSidesPositive() const;
    bool allSidesLessThanSumOfOthers() const;
    bool allSidesEqual() const;
    bool atLeastTwoSidesEqual() const;
};



namespace triangle {
    flavor kind(double a, double b, double c) {
        Triangle t = Triangle::create(a, b, c);
        return t.kind();
    }
}


Triangle Triangle::create(double x, double y, double z) {
    std::vector<double> sides { x, y, z };
    std::sort(sides.begin(), sides.end());
    Triangle t = Triangle(sides[0], sides[1], sides[2]);
    if (t.isInvalidTriangle()) {
        throw std::domain_error("Invalid triangle");
    }
    return t;
}

triangle::flavor Triangle::kind() const {
    if (allSidesEqual()) {
        return triangle::equilateral;
    } else if (atLeastTwoSidesEqual()) {
        return triangle::isosceles;
    } else {
        return triangle::scalene;
    }
}

bool Triangle::isValidTringle() const {
    return allSidesPositive()
        && allSidesLessThanSumOfOthers();
}

bool Triangle::isInvalidTriangle() const {
    return !isValidTringle();
}

bool Triangle::allSidesPositive() const {
    return (a > 0)
        && (b > 0)
        && (c > 0);
}

bool Triangle::allSidesLessThanSumOfOthers() const {
    // Since c is the largest side, it's the only one we need to worry about
    return c < a + b;
}

bool Triangle::allSidesEqual() const {
    return (a == b)
        && (b == c);
}

bool Triangle::atLeastTwoSidesEqual() const {
    return (a == b)
        || (b == c)
        || (c == a);
}
