require 'minitest/autorun'
require_relative 'collatz_conjecture'

# Common test data version: 1.1.1 25c4479
class CollatzConjectureTest < Minitest::Test
  def test_zero_steps_for_one
    assert_equal 0, CollatzConjecture.steps(1)
  end

  def test_divide_if_even
    assert_equal 4, CollatzConjecture.steps(16)
  end

  def test_even_and_odd_steps
    assert_equal 9, CollatzConjecture.steps(12)
  end

  def test_large_number_of_even_and_odd_steps
    assert_equal 152, CollatzConjecture.steps(1_000_000)
  end

  def test_zero_is_an_error
    assert_raises(ArgumentError) { CollatzConjecture.steps(0) }
  end

  def test_negative_value_is_an_error
    assert_raises(ArgumentError) { CollatzConjecture.steps(-15) }
  end
end
