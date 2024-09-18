pub mod fraction {
    use std::ops::{Add, Div, Mul, Neg, Sub};

    use num_bigint::BigInt;

    #[derive(Clone, Debug, PartialEq, Eq, Hash)]
    pub struct Fraction {
        pub numerator: BigInt,
        pub denominator: BigInt,
    }

    impl Fraction {
        pub fn zero() -> Self {
            Fraction {
                numerator: BigInt::ZERO,
                denominator: BigInt::from(1),
            }
        }

        pub fn one() -> Self {
            Fraction {
                numerator: BigInt::from(1),
                denominator: BigInt::from(1),
            }
        }

        pub fn new(numerator: BigInt, denominator: BigInt) -> Self {
            if denominator < BigInt::ZERO {
                Fraction {
                    numerator: -numerator,
                    denominator: -denominator,
                }
            } else {
                Fraction { numerator, denominator }
            }
        }

        pub fn is_non_negative_integer(&self) -> bool {
            self.denominator == BigInt::from(1) && self.numerator >= BigInt::ZERO
        }

        pub fn is_integer(&self) -> bool {
            self.denominator == BigInt::from(1)
        }

        pub fn to_u32(&self) -> Option<u32> {
            if self.is_non_negative_integer() {
                if self.numerator.bits() <= u32::BITS as u64 {
                    return self.numerator.to_u32_digits().1.into_iter().next()
                }
            }
            None
        }
    }

    impl Add for &Fraction {
        type Output = Fraction;

        fn add(self, other: Self) -> Fraction {
            let numerator = &self.numerator * &other.denominator + &other.numerator * &self.denominator;
            let denominator = &self.denominator * &other.denominator;

            let gcd = gcd(&numerator, &denominator);
            let numerator = numerator / &gcd;
            let denominator = denominator / &gcd;

            Fraction { numerator, denominator }
        }
    }

    impl Neg for &Fraction {
        type Output = Fraction;

        fn neg(self) -> Fraction {
            Fraction {
                numerator: -&self.numerator,
                denominator: self.denominator.clone(),
            }
        }
    }

    impl Sub for &Fraction {
        type Output = Fraction;

        fn sub(self, other: Self) -> Fraction {
            let numerator = &self.numerator * &other.denominator - &other.numerator * &self.denominator;
            let denominator = &self.denominator * &other.denominator;

            let gcd = gcd(&numerator, &denominator);
            let numerator = numerator / &gcd;
            let denominator = denominator / &gcd;

            Fraction { numerator, denominator }
        }
    }

    impl Mul for &Fraction {
        type Output = Fraction;

        fn mul(self, other: Self) -> Fraction {
            let numerator = &self.numerator * &other.numerator;
            let denominator = &self.denominator * &other.denominator;

            let gcd = gcd(&numerator, &denominator);
            let numerator = numerator / &gcd;
            let denominator = denominator / &gcd;

            Fraction { numerator, denominator }
        }
    }
    
    impl Div for &Fraction {
        type Output = Fraction;

        fn div(self, other: Self) -> Fraction {
            let numerator = &self.numerator * &other.denominator;
            let denominator = &self.denominator * &other.numerator;

            let mut gcd = gcd(&numerator, &denominator);
            if denominator < BigInt::ZERO {
                gcd = -gcd;
            }
            let numerator = numerator / &gcd;
            let denominator = denominator / &gcd;

            Fraction { numerator, denominator }
        }
    }

    impl std::fmt::Display for Fraction {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            if self.denominator == BigInt::from(1) {
                write!(f, "{}", self.numerator)
            } else {
                write!(f, "{}/{}", self.numerator, self.denominator)
            }
        }
    }

    fn gcd(a: &BigInt, b: &BigInt) -> BigInt {
        let mut a = if a < &BigInt::ZERO { -a.clone() } else { a.clone() };
        let mut b = if b < &BigInt::ZERO { -b.clone() } else { b.clone() };

        if a < b {
            std::mem::swap(&mut a, &mut b);
        }

        while b != BigInt::ZERO {
            let r = &a % &b;
            a = b;
            b = r;
        }

        a
    }

    #[cfg(test)]
    mod tests {
        use super::*;

        #[test]
        fn test_fraction() {
            let a = Fraction::new(BigInt::from(1), BigInt::from(2));
            let b = Fraction::new(BigInt::from(1), BigInt::from(3));
            let c = Fraction::new(BigInt::from(1), BigInt::from(6));
            
            assert_eq!(&(&a + &b) + &c, Fraction::one());
            println!("{}", &(&a + &b) + &c);
            println!("{}", &(&a * &b) + &c);
        }
    }
}