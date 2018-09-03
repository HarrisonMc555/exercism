class FlattenArray
  def self.recursive_flat_map(elem)
    if elem.class != Array then
      return elem
    end
    elem.flat_map { |e| recursive_flat_map(e) }.reject { |e| e.nil? }
  end
  def self.flatten(arr)
    recursive_flat_map(arr)
  end
end
