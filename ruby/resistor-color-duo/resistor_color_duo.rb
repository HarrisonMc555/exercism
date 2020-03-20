# frozen_string_literal: true

# Convert resistor string values into numeric values
class ResistorColorDuo
  def self.value(colors)
    colors[0...2].reduce('') do |s, c|
      return nil if s.nil?

      n = COLOR_VALUE[c]
      return nil if n.nil?

      s + n.to_s
    end.to_i
  end

  COLOR_VALUE =
    {
      'black' => 0,
      'brown' => 1,
      'red' => 2,
      'orange' => 3,
      'yellow' => 4,
      'green' => 5,
      'blue' => 6,
      'violet' => 7,
      'grey' => 8,
      'white' => 9,
    }.freeze
end
