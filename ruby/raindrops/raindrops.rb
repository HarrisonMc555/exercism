# Create raindrop sounds for a number based on divisors
class Raindrops
  def self.convert(num)
    s = ''
    s += 'Pling' if divisible?(num, 3)
    s += 'Plang' if divisible?(num, 5)
    s += 'Plong' if divisible?(num, 7)
    s = num.to_s if s.empty?
    s
  end

  def self.divisible?(num, div)
    (num % div).zero?
  end
end
