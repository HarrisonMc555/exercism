require 'minitest/autorun'
require_relative 'hamming'

# Common test data version: 2.0.1 f79dfd7
class HammingTest < Minitest::Test
  def test_empty_strands
    assert_equal 0, Hamming.compute('', '')
  end

  def test_identical_strands
    assert_equal 0, Hamming.compute('A', 'A')
  end

  def test_long_identical_strands
    assert_equal 0, Hamming.compute('GGACTGA', 'GGACTGA')
  end

  def test_complete_distance_in_single_nucleotide_strands
    assert_equal 1, Hamming.compute('A', 'G')
  end

  def test_complete_distance_in_small_strands
    assert_equal 2, Hamming.compute('AG', 'CT')
  end

  def test_small_distance_in_small_strands
    assert_equal 1, Hamming.compute('AT', 'CT')
  end

  def test_small_distance
    assert_equal 1, Hamming.compute('GGACG', 'GGTCG')
  end

  def test_small_distance_in_long_strands
    assert_equal 2, Hamming.compute('ACCAGGG', 'ACTATGG')
  end

  def test_non_unique_character_in_first_strand
    assert_equal 1, Hamming.compute('AAG', 'AAA')
  end

  def test_non_unique_character_in_second_strand
    assert_equal 1, Hamming.compute('AAA', 'AAG')
  end

  def test_same_nucleotides_in_different_positions
    assert_equal 2, Hamming.compute('TAG', 'GAT')
  end

  def test_large_distance
    assert_equal 4, Hamming.compute('GATACA', 'GCATAA')
  end

  def test_large_distance_in_off_by_one_strand
    assert_equal 9, Hamming.compute('GGACGGATTCTG', 'AGGACGGATTCT')
  end

  def test_disallow_first_strand_longer
    assert_raises(ArgumentError) { Hamming.compute('AATG', 'AAA') }
  end

  def test_disallow_second_strand_longer
    assert_raises(ArgumentError) { Hamming.compute('ATA', 'AGTG') }
  end
end
