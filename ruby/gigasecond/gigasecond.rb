# frozen_string_literal: true

# Calculate date a gigasecond away from input
class Gigasecond
  GIGASECOND = 10**9
  def self.from(time)
    # According to documentation, you can add a number and it will add that many
    # seconds.
    # https://ruby-doc.org/core-2.3.3/Time.html
    time + GIGASECOND
  end
end
