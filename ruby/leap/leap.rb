# frozen_string_literal: true

# Determine whether or not a year is a leap year
class Year
  def self.leap?(year)
    divisible_by4 = divisible_by?(year, 4)
    divisible_by100 = divisible_by?(year, 100)
    divisible_by400 = divisible_by?(year, 400)
    divisible_by4 && !divisible_by100 || divisible_by400
  end

  private_class_method def self.divisible_by?(number, divisor)
    (number % divisor).zero?
  end
end
