import unittest

from allergies import Allergies

class AllergiesTests(unittest.TestCase):
    def test_allergic_to_eggs_in_addition_to_other_stuff(self):
        allergies = Allergies(5)
        # print('lst:', allergies.lst)
        self.assertIs(allergies.is_allergic_to('eggs'), True)
        self.assertIs(allergies.is_allergic_to('shellfish'), True)
        self.assertIs(allergies.is_allergic_to('strawberries'), False)


if __name__ == '__main__':
    unittest.main()
