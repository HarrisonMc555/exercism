class ResistorColorDuo
  def self.value(colors)
    colors[0...2].reduce("") do |s, c|
      return nil if s.nil?
      n = self.color_value c
      return nil if n.nil?
      s.concat n.to_s
    end.to_i
  end

  private
  def self.color_value(color)
    {
      "black" => 0,
      "brown" => 1,
      "red" => 2,
      "orange" => 3,
      "yellow" => 4,
      "green" => 5,
      "blue" => 6,
      "violet" => 7,
      "grey" => 8,
      "white" => 9,
    }[color]
  end
end
