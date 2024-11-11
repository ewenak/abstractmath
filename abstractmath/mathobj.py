#! /usr/bin/env python3

def flatten_associative(kind, *nodes):
    for el in nodes:
        if isinstance(el, kind):
            yield from el.iter_factors()
        else:
            yield el


class MathObject:
    operation_types = {}

    def __init_subclass__(cls, **kwargs):
        super().__init_subclass__(**kwargs)

        if hasattr(cls, 'operator'):
            MathObject.operation_types[cls.operator] = cls

    @classmethod
    def parse(cls, string):
        tokens = cls.tokenize(string)
        obj_stack = []
        for token in tokens:
            if token == '(':
                key = next(tokens, None)
                if key is None:
                    raise SyntaxError(
                        'expected operation after (, not EOF'
                    )
                obj_type = MathObject.operation_types.get(key)
                if obj_type is None:
                    raise SyntaxError(f'unknown operation: {key!r}')
                obj_stack.append((obj_type, []))
            elif token == ')':
                if not obj_stack:
                    raise SyntaxError('Unexpected )')
                obj_type, args = obj_stack.pop()
                try:
                    obj = obj_type(*args)
                except (TypeError, ValueError):
                    raise SyntaxError(
                        f'Incorrect arguments for obj: {args}'
                    ) from None
                if obj_stack:
                    obj_stack[-1][1].append(obj)
                else:
                    if next(tokens, None) is not None:
                        raise SyntaxError('Expected EOF after )')
                    return obj
            elif token.startswith(':'):
                obj_stack[-1][1].append(Variable(token[1:]))
            elif '0' < token[0] < '9' or token[0] == '-':
                try:
                    obj_stack[-1][1].append(Number(int(token)))
                except ValueError:
                    raise SyntaxError(
                        f'Was expecting integer after {token[0]!r}'
                    ) from None
            else:
                raise SyntaxError(
                    f"Invalid token: {token!r} (shouldn't have happened)"
                )
        raise SyntaxError("Unclosed parentheses")

    @classmethod
    def tokenize(cls, string):
        current_token = []
        string_delimiter = None
        for c in string:
            if string_delimiter == c:
                string_delimiter = None
            elif string_delimiter is not None:
                current_token.append(c)
            elif c.isspace():
                if current_token:
                    yield ''.join(current_token)
                    current_token = []
                continue
            elif c in '()':
                if current_token:
                    yield ''.join(current_token)
                    current_token = []
                yield c
            elif c in ['"', "'"]:
                string_delimiter = c
            else:
                current_token.append(c)

    def __mul__(self, b):
        return Product(self, b)

    def __add__(self, b):
        return Sum(self, b)

    def iter_subfactors(self):
        yield self

    def simplify(self):
        return self

    def __repr__(self):
        return f"""{self.__class__.__name__}.parse(
    {str(self)!r}
)"""


class CommutativeMixin:
    def __init__(self, a: MathObject, b: MathObject):
        self._a = a
        self._b = b

    @property
    def a(self):
        return self._a

    @property
    def b(self):
        return self._b

    def __hash__(self):
        return hash(tuple(sorted((hash(self.a), hash(self.b)))))

    def __eq__(self, other):
        if not isinstance(other, type(self)):
            return NotImplemented
        return ((self.a == other.a and self.b == other.b)
                or (self.a == other.b and self.b == other.a))


class Sum(CommutativeMixin, MathObject):
    operator = '+'

    def simplify(self):
        a = self.a.simplify()
        b = self.b.simplify()
        if isinstance(a, Number) and isinstance(b, Number):
            return Number(a.value + b.value)
        return Sum(a, b)

    def iter_subfactors(self):
        yield self
        yield from self.common_factors()

    def common_factors(self):
        a_subfactors = set(self.a.iter_subfactors())
        b_subfactors = set(self.b.iter_subfactors())
        return a_subfactors.intersection(b_subfactors)

    def factorize(self, factor):
        return Product(
            factor,
            Sum(
                Product(self.a, Power(factor, Number(-1))),
                Product(self.b, Power(factor, Number(-1)))
            )
        )

    def __str__(self):
        return f'(+ {self.a} {self.b})'


class Product(MathObject, CommutativeMixin):
    operator = '*'

    def simplify(self):
        a = self.a.simplify()
        b = self.b.simplify()
        if (
            isinstance(a, Number) and isinstance(b, Number)
            and a.rational and b.rational
        ):
            return Number(a.value * b.value)
        factor_counter = {}
        for el in flatten_associative(Product, a, b):
            if (isinstance(el, Power) and isinstance(el.exponent, Number)
                    and el.exponent.rational):
                factor_counter[el.base] = (factor_counter.get(el.base, 0)
                                           + el.exponent.value)
                continue
            factor_counter[el] = factor_counter.get(el, 0) + 1
        factors = (a if power == 1 else
                   Number(1) if power == 0 else Power(a, power)
                   for a, power in factor_counter.items())
        a = next(factors)
        for b in factors:
            if b == 1:
                continue
            elif a == 1:
                a = b
            else:
                a = Product(a, b)
        return a

    def expand(self):
        if isinstance(self.a, Sum):
            return self.a.a * self.b + self.a.b * self.b
        elif isinstance(self.b, Sum):
            return self.b.a * self.a + self.b.b * self.a
        else:
            return self

    def iter_factors(self):
        return flatten_associative(Product, self.a, self.b)

    def iter_subfactors(self):
        yield self
        yield from self.a.iter_subfactors()
        yield from self.b.iter_subfactors()

    def __str__(self):
        return f'(* {self.a} {self.b})'


class Power(MathObject):
    operator = '^'

    def __init__(self, base, exponent):
        self._base = base
        self._exponent = exponent

    @property
    def base(self):
        return self._base

    @property
    def exponent(self):
        return self._exponent

    def __eq__(self, other):
        if not isinstance(other, Power):
            return NotImplemented
        return self.base == other.base and self.exponent == other.exponent

    def __hash__(self):
        return hash((self.base, self.exponent))

    def __str__(self):
        return f'(^ {self.base} {self.exponent})'


class Number(MathObject):
    def __init__(self, value: int, *, rational: bool = True):
        self.rational = rational
        if isinstance(value, Number):
            self.value = value.value
        elif isinstance(value, int):
            self.value = value
        else:
            raise TypeError('only int type is supported for numbers as of now')

    def __eq__(self, other):
        if isinstance(other, int):
            return self.rational and self.value == other
        elif not isinstance(other, Number):
            return NotImplemented
        return (self.rational == other.rational
                and (self.value == other.value if self.rational else
                     self.value - other.value < 1e-6))

    def __hash__(self):
        return hash(self.value)

    def __str__(self):
        return str(self.value)


class Variable(MathObject):
    def __init__(self, symbol: str):
        self.symbol = symbol

    def __eq__(self, other):
        if not isinstance(other, Variable):
            return NotImplemented
        return self.symbol == other.symbol

    def __hash__(self):
        return hash(self.symbol)

    def __str__(self):
        if all('a' <= c <= 'z' or 'A' <= c <= 'Z' for c in self.symbol):
            return f':{self.symbol}'
        else:
            return f':{self.symbol!r}'
