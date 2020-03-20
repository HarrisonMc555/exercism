# frozen_string_literal: true

# Create sliding window slices into a series
class Series
  def initialize(series)
    @series = series
  end

  def slices(slice_length)
    if slice_length > @series.length
      raise ArgumentError, 'Slice greater than series length'
    end

    @series.chars.each_cons(slice_length).map { |cs| cs.join('') }
  end
end
