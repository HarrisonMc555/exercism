# Calculate total steps for Collatz sequences
class CollatzConjecture
  def self.steps(num)
    if num <= 0
      raise ArgumentError, 'Starting collatz sequence with non-positive number'
    end
    num_steps = 0
    while num != 1
      num_steps += 1
      num = next_collatz(num)
    end
    num_steps
  end

  private_class_method def self.next_collatz(num)
    if num.even?
      num / 2
    else
      3 * num + 1
    end
  end
end
