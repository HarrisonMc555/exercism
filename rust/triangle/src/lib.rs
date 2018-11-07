pub struct Triangle([u64; 3]);

impl Triangle {
    pub fn build(sides: [u64; 3]) -> Option<Triangle> {
        let mut sides = sides;
        if Triangle::is_valid_triangle(&sides) {
            sides.sort();
            Some(Triangle(sides))
        } else {
            None
        }
    }

    pub fn is_equilateral(&self) -> bool {
        let (a, b, c) = (self.0[0], self.0[1], self.0[2]);
        a == b && a == c
    }

    pub fn is_scalene(&self) -> bool {
        let (a, b, c) = (self.0[0], self.0[1], self.0[2]);
        a != b && a != c && b != c
    }

    pub fn is_isosceles(&self) -> bool {
        let (a, b, c) = (self.0[0], self.0[1], self.0[2]);
        !self.is_scalene() && (a == b || a == c || b == c)
    }

    fn is_valid_triangle(sides: &[u64; 3]) -> bool {
        let (a, b, c) = (sides[0], sides[1], sides[2]);
        a + b > c && a + c > b && b + c > a
    }
}
