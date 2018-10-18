require 'set'

# Determines whether a string is an isogram or not
class Isogram
  def self.isogram?(str)
    letters = str.delete('^a-zA-Z')
    letters.length == letters.downcase.chars.to_set.length
  end
end
