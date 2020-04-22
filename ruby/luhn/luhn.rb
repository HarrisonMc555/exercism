# frozen_string_literal: true

# Validate luhn numbers
class Luhn
  def self.valid?(input)
    Luhn.new(input).valid?
  end

  def initialize(input)
    @input = input
  end

  def valid?
    valid_format? && valid_digit_sum?
  end

  private

  def valid_format?
    return false if @input.match(/[^ 0-9]/)

    digits.length > 1
  end

  def valid_digit_sum?
    (processed_digits.sum % 10).zero?
  end

  def digits
    @input.tr(' ', '').chars.map(&:to_i)
  end

  def processed_digits
    digits
      .reverse
      .each_with_index
      .map { |digit, i| process_digit(digit, i) }
  end

  def process_digit(digit, index)
    if index.odd?
      double(digit)
    else
      digit
    end
  end

  def double(number)
    doubled = number * 2
    if doubled > 9
      doubled - 9
    else
      doubled
    end
  end
end
