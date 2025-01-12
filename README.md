# abstractmath - a toy algebra system in pure Python

## Use

Import `abstractmath.mathobj`.

Mathematical expressions are represented as a binary tree. All mathematical
expressions objects are instances of a subclass of MathObject. Existing
subclasses are:
- `Number` and `Variable`
- `Power`
- `BinaryOperation`
- ``CommutativeAssociativeOperation` (subclass of `BinaryOperation`)
- `Sum and `Product`, subclasses of `CommutativeAssociativeOperation`

MathObject can create mathematical objects from strings in the form:
`(operation operand1 operand2)`
For example: `(+ (+ (* 8 (^ x 2)) (* 5 x)) 3)` means 8x² + 5x + 3.

These strings can be parsed using `MathObject.parse`:

```python
>>> m = MathObject.parse("(+ (+ (* 8 (^ x 2)) (* 5 x)) 3)")
>>> m
Sum.parse(
    '(+ (+ (* 8 (^ x 2)) (* 5 )) 3)'
)
```

You can also create objects using the constructor:
```python
>>> n = Number(5)
>>> n
Number.parse(
    '5'
)
>>> Variable('x')
Variable.parse(
    'x'
)
>>> Product('x', n)
Product.parse(
    '(* x 5)'
)
```

You can then use standard python operators on these objects:
```python
>>> m * n
Product.parse(
    '(* (+ (+ (* 8 (^ x 2)) (* 5 x)) 3) 5)'
)
```
