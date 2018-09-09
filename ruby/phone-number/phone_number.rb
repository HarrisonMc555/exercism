class PhoneNumber
  def self.clean(phone)
    just_numbers = phone.gsub(/[^0-9]/, '')

    case just_numbers.length
    when 10
      # OK
    when 11
      if just_numbers.start_with?('1')
        just_numbers = just_numbers[1..-1]
      else
        return nil
      end
    else
      return nil
    end

    # Test if valid
    if valid_phone_number?(just_numbers)
      just_numbers
    else
      nil # Invalid phone number => nil
    end
  end

  def self.valid_phone_number?(phone)
    area_code = phone[0..2]
    local_number = phone[3..-1]
    !area_code.start_with?('0', '1') && !local_number.start_with?('0', '1')
  end
end
