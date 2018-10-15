var Pangram = require('./pangram');

describe('Pangram()', function ()  {
  it('sentence empty', function () {
    var pangram = new Pangram('');
    expect(pangram.isPangram()).toBe(false);
  });

  it('recognizes a perfect lower case pangram', function ()  {
    var pangram = new Pangram('abcdefghijklmnopqrstuvwxyz');
    expect(pangram.isPangram()).toBe(true);
  });

  it('pangram with only lower case', function ()  {
    var pangram = new Pangram('the quick brown fox jumps over the lazy dog');
    expect(pangram.isPangram()).toBe(true);
  });

  it("missing character 'x'", function () {
    var pangram = new Pangram('a quick movement of the enemy will jeopardize five gunboats');
    expect(pangram.isPangram()).toBe(false);
  });

  it("another missing character, e.g. 'h'", function () {
    var pangram = new Pangram('five boxing wizards jump quickly at it');
    expect(pangram.isPangram()).toBe(false);
  });

  it('pangram with underscores', function () {
    var pangram = new Pangram('the_quick_brown_fox_jumps_over_the_lazy_dog');
    expect(pangram.isPangram()).toBe(true);
  });

  it('pangram with numbers', function () {
    var pangram = new Pangram('the 1 quick brown fox jumps over the 2 lazy dog');
    expect(pangram.isPangram()).toBe(true);
  });

  it('missing letters replaced by numbers', function ()  {
    var pangram = new Pangram('7h3 qu1ck brown fox jumps ov3r 7h3 lazy dog');
    expect(pangram.isPangram()).toBe(false);
  });

  it('pangram with mixed case and punctuation', function ()  {
    var pangram = new Pangram('\"Five quacking Zephyrs jolt my wax bed.\"');
    expect(pangram.isPangram()).toBe(true);
  });

  it('upper and lower case versions of the same character should not be counted separately', function ()  {
    var pangram = new Pangram('the quick brown fox jumps over with lazy FX');
    expect(pangram.isPangram()).toBe(false);
  });
});
