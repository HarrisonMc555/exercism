# frozen_string_literal: true

# Create an  acronym for a phrase
class Acronym
  def self.abbreviate(phrase)
    phrase.scan(/\b\w/).join('').upcase
  end
end
