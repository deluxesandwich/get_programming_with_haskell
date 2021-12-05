def counter(x):
    return (lambda x:
                (lambda x:
                    (lambda x: x)(x) + 1
                )(x) + 1
            )(x)

print(
    counter(1)
)
