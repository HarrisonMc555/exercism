# frozen_string_literal: true

# Create a clean phone number
class PhoneNumber
  def self.clean(phone)
    just_numbers = phone.gsub(/[^0-9]/, '')
    if international?(just_numbers)
      return nil unless valid_international_code?(just_numbers)
    end
    stripped_numbers = strip_international_code(just_numbers)
    valid_phone_number?(stripped_numbers) ? stripped_numbers : nil
  end

  def self.valid_phone_number?(phone)
    return false unless phone.length == 10

    area_code = phone[0..2]
    local_number = phone[3..-1]
    !area_code.start_with?('0', '1') && !local_number.start_with?('0', '1')
  end

  def self.strip_international_code(phone)
    phone.length == 11 ? phone[1..-1] : phone
  end

  def self.international?(phone)
    phone.length == 11
  end

  def self.valid_international_code?(phone)
    phone[0] == '1'
  end
end
