# frozen_string_literal: true

# Determine if all brackets are matched
class Brackets
  def self.paired?(str)
    stack = []
    str.chars.each do |c|
      if BRACKET_MAP.key?(c)
        stack.push(c)
      elsif BRACKET_MAP.value?(c)
        return false unless paired_brackets?(stack.pop, c)
      end
    end
    # If there is anything left over, brackets weren't matched
    stack.empty?
  end

  def self.paired_brackets?(opening, closing)
    return false if opening.nil? || closing.nil?

    BRACKET_MAP[opening] == closing
  end

  BRACKET_MAP = {
    '[' => ']',
    '{' => '}',
    '(' => ')',
  }.freeze
end
