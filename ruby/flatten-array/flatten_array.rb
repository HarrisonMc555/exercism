# frozen_string_literal: true

# Recursively flatten arrays, filtering out nil
class FlattenArray
  def self.flatten(arr)
    recursive_flat_map(arr)
  end

  private_class_method def self.recursive_flat_map(elem)
    return elem if elem.class != Array

    elem.flat_map { |e| recursive_flat_map(e) }.reject(&:nil?)
  end
end
