# Create responses from a lackadaisical teenager
class Bob
  def self.hey(remark)
    yelling = yelling? remark
    question = question? remark
    nothing = nothing? remark
    if yelling and question
      "Calm down, I know what I'm doing!"
    elsif yelling
      'Whoa, chill out!'
    elsif question
      'Sure.'
    elsif nothing
      'Fine. Be that way!'
    else
      'Whatever.'
    end
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