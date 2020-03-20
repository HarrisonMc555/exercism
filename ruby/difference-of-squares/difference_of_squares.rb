# frozen_string_literal: true

# Calculate square of sum and sum of squares
class Squares
  def initialize(num)
    @nums = (1..num)
  end

  def square_of_sum
    @_square_of_sum ||= calc_square_of_sum
    @_square_of_sum
  end

  def sum_of_squares
    @_sum_of_squares ||= calc_sum_of_squares
    @_sum_of_squares
  end

  def difference
    @_difference ||= calc_difference
    @_difference
  end

  private

  def calc_square_of_sum
    @nums.sum**2
  end

  def calc_sum_of_squares
    @nums.map { |x| x**2 }.sum
  end

  def calc_difference
    square_of_sum - sum_of_squares
  end
end
