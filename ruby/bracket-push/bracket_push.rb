class Brackets
  def self.paired?(s)
    stack = []
    for c in s.chars
      if BRACKET_MAP.has_key?(c)
        stack.push(c)
      elsif BRACKET_MAP.has_value?(c)
        return false if !paired_brackets?(stack.pop, c)
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
