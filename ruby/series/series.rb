class Series
  def initialize(series)
    @series = series
  end
  def slices(n)
    if n > @series.length then
      raise ArgumentError, 'Slice greater than series length'
    end
    @series.chars.each_cons(n).map { |cs| cs.join('') }
  end
end
