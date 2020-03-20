# frozen_string_literal: true

# Determines if a number is an Armstrong number
class ArmstrongNumbers
  def self.include?(num)
    num == armstrong_sum(num)
  end

  private

  def self.armstrong_sum(num)
    digits = get_digits(num)
    num_digits = digits.length
    digits.map { |x| x**num_digits }.sum
  end

  def self.get_digits(num)
    digits = []
    while num.positive?
      digits.append(num % 10)
      num /= 10
    end
    digits
  end

  private_class_method :armstrong_sum
  private_class_method :get_digits
end
