# Determines whether a string is an isogram or not
class Isogram
  def self.isogram?(str)
    letters = str.downcase.scan(/\w/)
    letters.length == letters.uniq.length
  end
end
