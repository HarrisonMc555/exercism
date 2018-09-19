"""Data type for rational numbers"""

from __future__ import division


class Rational:
    """A class for rational numbers

    A rational number is the quotient of two integers.
    """
    def __init__(self, numer, denom):
        self.numer = numer
        self.denom = denom

    def __eq__(self, other):
        return self.numer == other.numer and self.denom == other.denom

    def __repr__(self):
        return '{}/{}'.format(self.numer, self.denom)

    def __add__(self, other):
        numer1 = self.numer * other.denom
        numer2 = other.numer * self.denom
        numer = numer1 + numer2
        denom = self.denom * other.denom
        return Rational(numer, denom)

    def __sub__(self, other):
        numer1 = self.numer * other.denom
        numer2 = other.numer * self.denom
        numer = numer1 - numer2
        denom = self.denom * other.denom
        return Rational(numer, denom)

    def __mul__(self, other):
        numer = self.numer * other.numer
        denom = self.denom * other.denom
        return Rational(numer, denom)

    def __truediv__(self, other):
        numer = self.numer * other.denom
        denom = other.numer * self.denom
        if denom == 0:
            raise ZeroDivisionError
        return Rational(numer, denom)

    def __abs__(self):
        numer = abs(self.numer)
        denom = abs(self.denom)
        return Rational(numer, denom)

    def __pow__(self, power):
        if power >= 0:
            numer = self.numer ** power
            denom = self.denom ** power
        else:
            numer = self.denom ** power
            denom = self.numer ** power
        return Rational(numer, denom)

    def __rpow__(self, base):
        pass
