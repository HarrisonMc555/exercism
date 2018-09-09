# Create an  acronym for a phrase
class Acronym
  def self.abbreviate(phrase)
    clean_phrase = phrase.tr('-', ' ')
    words = clean_phrase.split(' ')
    first_letters = words.map { |w| w[0] }
    first_letters.join('').upcase
  end
end
