# Count the number of occurences of each word in a phrase
class Phrase
  def initialize(phrase)
    @phrase = phrase
  end

  def word_count
    punctuation_pattern = %r{[`~!@#\$%^&*\(\)\-_=+\[\]\\\{\}\|;:,.<>/?]}
    clean_phrase = @phrase.gsub(punctuation_pattern, ' ')
    words = clean_phrase.split(' ').map(&:downcase)
                        .map { |w| Surround.strip_surrounding_quotes(w) }
    words.each_with_object(Hash.new(0)) do |word, counts|
      counts[word] += 1
    end
  end
end

# Helper class for stripping surrounding strings
class Surround
  def self.strip_surrounding_quotes(word)
    word = strip_surrounding_string(word, '"') if surrounded_by(word, '"')
    word = strip_surrounding_string(word, "'") if surrounded_by(word, "'")
    # In case surrounded by single quotes first
    word = strip_surrounding_string(word, '"') if surrounded_by(word, '"')
    # We could recursively call for arbitrarily nested quotes, but let's not
    word
  end

  def self.strip_surrounding_string(string, surrounding)
    string.gsub(/^[#{surrounding}*]/, '').gsub(/#{surrounding}*$/, '')
  end

  def self.surrounded_by(string, surrounding)
    string.start_with?(surrounding) && string.end_with?(surrounding)
  end
end
