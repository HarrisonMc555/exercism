require 'minitest/autorun'
require_relative 'acronym'

# Common test data version: 1.1.0 cae7ae1
class AcronymTest < Minitest::Test
  def test_basic
    assert_equal "PNG", Acronym.abbreviate('Portable Network Graphics')
  end

  def test_lowercase_words
    assert_equal "ROR", Acronym.abbreviate('Ruby on Rails')
  end

  def test_punctuation
    assert_equal "FIFO", Acronym.abbreviate('First In, First Out')
  end

  def test_all_caps_words
    assert_equal "PHP", Acronym.abbreviate('PHP: Hypertext Preprocessor')
  end

  def test_non_acronym_all_caps_word
    assert_equal "GIMP", Acronym.abbreviate('GNU Image Manipulation Program')
  end

  def test_hyphenated
    assert_equal "CMOS", Acronym.abbreviate('Complementary metal-oxide semiconductor')
  end
end
