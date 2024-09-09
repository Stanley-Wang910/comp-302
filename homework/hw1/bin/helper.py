import math


def binomial(n, k):
    return math.factorial(n) / (math.factorial(n - k) * math.factorial(k))


def create_ocaml_tests(random_tuples):
    res = []
    for tuple in random_tuples:
        n, k = tuple
        b = binomial(n, k)
        res.append((tuple, b))
    return res


random_tuples = [
    (38, 27),
    (40, 12),
    (29, 15),
    (33, 33),
    (19, 7),
    (36, 22),
    (25, 18),
    (31, 9),
    (22, 3),
    (37, 26),
    (14, 11),
    (39, 35),
    (28, 20),
    (17, 5),
    (34, 16),
]

res = create_ocaml_tests(random_tuples)
for item in res:
    print(f"{item}\n")
