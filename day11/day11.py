def transform_stone(stone: int) -> list[int]:
    digits = str(stone)
    length = len(digits)
    if stone == 0:
        return [1]
    elif length % 2 == 0:
        return [int(digits[: length // 2]), int(digits[length // 2 :])]
    else:
        return [stone * 2024]


memo: dict[tuple[int, int], int] = {}


def count_stones(n: int, stone: int) -> int:
    if (n, stone) in memo:
        return memo[(n, stone)]
    elif n == 1:
        return len(transform_stone(stone))
    next_stones = transform_stone(stone)
    count = 0
    for next_stone in next_stones:
        length = count_stones(n - 1, next_stone)
        memo[(n - 1, next_stone)] = length
        count += length
    return count


def count_all(n: int, stones: list[int]) -> int:
    count = 0
    for stone in stones:
        count += count_stones(n, stone)
    return count


stones = [int(stone) for stone in open("input").read().split()]

print(count_all(25, stones))
print(count_all(75, stones))
