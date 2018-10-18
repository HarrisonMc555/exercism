# Calculate square of sum and sum of squares
class Squares
  def initialize(num)
    nums = (1..num)
    @square_of_sum = (sum nums)**2
    @sum_of_squares = sum(nums.map { |x| x**2 })
    @difference = @square_of_sum - @sum_of_squares
  end
  attr_reader :square_of_sum
  attr_reader :sum_of_squares
  attr_reader :difference
end

def sum(enumerable)
  enumerable.reduce(0, :+)
end
