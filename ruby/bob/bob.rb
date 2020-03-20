# frozen_string_literal: true

# Create responses from a lackadaisical teenager
class Bob
  YELLING_QUESTION_RESPONSE = "Calm down, I know what I'm doing!"
  YELLING_RESPONSE = 'Whoa, chill out!'
  QUESTION_RESPONSE = 'Sure.'
  NOTHING_RESPONSE = 'Fine. Be that way!'
  OTHER_RESPONSE = 'Whatever.'
  def self.hey(remark)
    yelling = yelling? remark
    question = question? remark
    nothing = nothing? remark
    return YELLING_QUESTION_RESPONSE if yelling && question
    return YELLING_RESPONSE if yelling
    return QUESTION_RESPONSE if question
    return NOTHING_RESPONSE if nothing

    OTHER_RESPONSE
  end

  private_class_method def self.question?(remark)
    remark.rstrip[-1] == '?'
  end

  private_class_method def self.yelling?(remark)
    has_upper = !remark.match(/[A-Z]/).nil?
    has_lower = !remark.match(/[a-z]/).nil?
    has_upper && !has_lower
  end

  private_class_method def self.nothing?(remark)
    remark.strip.empty?
  end
end
