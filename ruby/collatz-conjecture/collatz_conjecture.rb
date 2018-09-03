class CollatzConjecture
  def self.next_collatz(n)
    if n.even? then
      n / 2
    else
      3*n + 1
    end
  end
  def self.steps(n)
    if n <= 0 then
      raise ArgumentError, 'Starting collatz sequence with non-positive number'
    end
    num_steps = 0
    while n != 1
      num_steps += 1
      n = next_collatz(n)
    end
    num_steps
  end
end
