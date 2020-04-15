# frozen_string_literal: true

# Store a 2D matrix
class Matrix
  attr_reader :rows

  def initialize(string)
    @rows = parse(string)
  end

  def columns
    @rows.transpose
  end

  private

  def parse(string)
    string.split(/\n+/).map(&method(:parse_line))
  end

  def parse_line(line)
    line.split.map(&:to_i)
  end
end
