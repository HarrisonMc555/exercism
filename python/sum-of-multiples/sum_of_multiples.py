"""Return the sum of multiples up to a limit"""

def sum_of_multiples(limit, multiples):
    """Return the sum of multiples up to a limit"""
    nums = set()
    for multiple in multiples:
        nums.update(range(multiple, limit, multiple))
    return sum(nums)
