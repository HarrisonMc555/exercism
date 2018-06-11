class Allergies(object):
    _scores_to_ingredients = {
        1: 'eggs',
        2: 'peanuts',
        4: 'shellfish',
        8: 'strawberries',
        16: 'tomatoes',
        32: 'chocolate',
        64: 'pollen',
        128: 'cats',
    }
    _ingredients_to_scores = None # build based off of _scores_to_ingredients
    _lst = None

    def __init__(self, score):
        self.lst = Allergies.create_list(score)
        Allergies._ingredients_to_scores = {}
        for score, ingredient in Allergies._scores_to_ingredients.items():
            Allergies._ingredients_to_scores[ingredient] = score

    def is_allergic_to(self, item):
        return item in self.lst

    @property
    def lst(self):
        return self._lst

    @lst.setter
    def lst(self, value):
        self._lst = value

    @staticmethod
    def create_list(score):
        return [Allergies.score_to_ingredient(s)
                for s in Allergies._scores_to_ingredients
                if Allergies.contains_ingredient(score, s)]

    @staticmethod
    def contains_ingredient(score, ingredient_score):
        overlap = score & ingredient_score
        return bool(overlap)

    @staticmethod
    def score_to_ingredient(score):
        if score not in Allergies._scores_to_ingredients:
            raise Exception('Invalid score for allergies: {}'.format(score))
        return Allergies._scores_to_ingredients[score]
