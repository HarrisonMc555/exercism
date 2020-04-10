# frozen_string_literal: true

# Store a 2D matrix
class Matrix
  attr_reader :rows
  attr_reader :columns

  def initialize(string)
    @rows = Matrix.parse(string)
    @columns = @rows.transpose
  end

  def self.parse(string)
    string.split(/\n+/).map(&Matrix.method(:parse_line))
  end

  def self.parse_line(line)
    line.split.map(&:to_i)
  end
end
