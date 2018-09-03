# Testing private class methods
class Foo
  def initialize(arg)
    @arg = arg
  end

  def bar
    puts @arg
    Foo.pcm
  end

  private_class_method def self.pcm
    puts 'Private class method'
  end

  def self.go
    puts 'Going now'
    pcm
  end
end

Foo.go

f = Foo.new('Hi!')
f.bar
