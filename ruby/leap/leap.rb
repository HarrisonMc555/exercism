class Year
  def self.divisible_by?(number, divisor)
    number % divisor == 0
  end
  def self.leap?(year)
    divisible_by_4 = divisible_by?(year, 4)
    divisible_by_100 = divisible_by?(year, 100)
    divisible_by_400 = divisible_by?(year, 400)
    divisible_by_4 && !divisible_by_100 || divisible_by_400
  end
end
