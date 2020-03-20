# frozen_string_literal: true

# Create raindrop sounds for a number based on divisors
class Raindrops
  NUM_SOUND_PAIRS = {
    3 => 'Pling',
    5 => 'Plang',
    7 => 'Plong',
  }.freeze

  def self.convert(num)
    sounds = NUM_SOUND_PAIRS
             .select { |key| divisible?(num, key) }
             .values
             .join
    sounds.empty? ? num.to_s : sounds
  end

  def self.divisible?(num, div)
    (num % div).zero?
  end
end
