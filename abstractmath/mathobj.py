#! /usr/bin/env python3

from collections import Counter
from math import prod

_UNSET = object()


def as_binary_tree(iterable, element_type, default=_UNSET):
    iterable = iter(iterable)

    try:
        a = next(iterable)
    except StopIteration:
        if default is _UNSET:
            raise TypeError('as_binary_tree of empty iterable with no default '
                            'value') from None
        return default

    for b in iterable:
        a = element_type(a, b)
    return a


class MathObject:
    operation_types = {}
    operator = None

    def __init_subclass__(cls, **kwargs):
        super().__init_subclass__(**kwargs)

        if getattr(cls, 'operator', None) is not None:
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
            elif token[0].isalpha():
                obj_stack[-1][1].append(Variable(token))
            elif '0' <= token[0] <= '9' or token[0] == '-':
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

    @classmethod
    def fromobj(cls, obj):
        if isinstance(obj, MathObject):
            return obj
        elif isinstance(obj, int):
            return Number(obj)
        elif isinstance(obj, str):
            try:
                return MathObject.parse(obj)
            except SyntaxError:
                return Variable(obj)
        raise TypeError(f'Unknown object type: {type(obj)}')

    def __mul__(self, b):
        return Product(self, b)

    def __rmul__(self, a):
        return Product(a, self)

    def __add__(self, b):
        return Sum(self, b)

    def __radd__(self, a):
        return Sum(a, self)

    def __truediv__(self, b):
        return Product(self, MathObject.fromobj(b) ** -1)

    def __rtruediv__(self, a):
        return Product(a, self ** -1)

    def __sub__(self, b):
        return Sum(self, -MathObject.fromobj(b))

    def __rsub__(self, a):
        return Sum(a, -self)

    def __neg__(self):
        return Product(-1, self)

    def __pow__(self, exponent):
        return Power(self, exponent)

    def __rpow__(self, base):
        return Power(base, self)

    def iter_subfactors(self):
        yield self

    def simplify(self):
        return self

    def iter_nodes(self, *, simplify=False):
        if simplify:
            yield self.simplify()
        else:
            yield self

    def __repr__(self):
        return f"""{self.__class__.__name__}.parse(
    {str(self)!r}
)"""


class BinaryOperation(MathObject):
    def __init__(self, a: MathObject, b: MathObject):
        self._a = MathObject.fromobj(a)
        self._b = MathObject.fromobj(b)

    @property
    def a(self):
        return self._a

    @property
    def b(self):
        return self._b

    def iter_nodes(self, *, simplify=False):
        if simplify:
            yield self.a.simplify()
            yield self.b.simplify()
        else:
            yield self.a
            yield self.b

    def __hash__(self):
        return hash(tuple(sorted((hash(self.a), hash(self.b)))))

    def __eq__(self, other):
        if not isinstance(other, type(self)):
            return NotImplemented
        return ((self.a == other.a and self.b == other.b)
                or (self.a == other.b and self.b == other.a))


class CommutativeAssociativeOperation(BinaryOperation):
    def iter_nodes(self, *, simplify=False):
        if not simplify:
            nodes = (self.a, self.b)
        else:
            nodes = (self.a.simplify(), self.b.simplify())

        for node in nodes:
            if isinstance(node, type(self)):
                yield from node.iter_nodes(simplify=simplify)
            else:
                yield node

    def separate_numbers_and_nodes(self):
        numbers = []
        nodes = []
        for node in self.iter_nodes(simplify=True):
            n = node.simplify()
            if isinstance(n, Number) and n.rational:
                numbers.append(n.value)
            else:
                nodes.append(n)

        return numbers, nodes

    def __hash__(self):
        return hash(tuple(sorted(
            hash(x) for x in self.iter_nodes()
        )))

    def __eq__(self, other):
        if not isinstance(other, type(self)):
            return NotImplemented
        return Counter(self.iter_nodes()) == Counter(other.iter_nodes())


class Sum(CommutativeAssociativeOperation):
    operator = '+'

    def simplify(self):
        numbers, terms = self.separate_numbers_and_nodes()
        term_count = {Number(1): sum(numbers)}

        for term in terms:
            if isinstance(term, Product):
                numbers, factors = term.separate_numbers_and_nodes()
                f = as_binary_tree(factors, Product)
                term_count[f] = term_count.get(f, 0) + prod(numbers)
            else:
                term_count[term] = term_count.get(term, 0) + 1

        final_terms = (t if m == 1 else Product(m, t).simplify()
                       for t, m in term_count.items()
                       if m != 0)
        return as_binary_tree(final_terms, Sum, default=Number(0))

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


class Product(CommutativeAssociativeOperation):
    operator = '*'

    def simplify(self):
        numbers, factors = self.separate_numbers_and_nodes()
        factor_counter = {Number(prod(numbers)): 1}

        for el in factors:
            factor = el.simplify()

            if factor == 0:
                return factor

            if (isinstance(factor, Power)
                    and isinstance(factor.exponent, Number)
                    and factor.exponent.rational):
                factor_counter[factor.base] = (
                    factor_counter.get(factor.base, 0)
                    + factor.exponent.value
                )
                continue

            factor_counter[factor] = factor_counter.get(factor, 0) + 1

        factors = (
            a if power == 1 else Number(1) if power == 0 else Power(a, power)
            for a, power in factor_counter.items()
            if power != 0 and a != 1
        )
        return as_binary_tree(factors, Product, Number(1))

    def expand(self):
        if isinstance(self.a, Sum):
            return self.a.a * self.b + self.a.b * self.b
        elif isinstance(self.b, Sum):
            return self.b.a * self.a + self.b.b * self.a
        else:
            return self

    def iter_subfactors(self):
        yield self
        yield from self.a.iter_subfactors()
        yield from self.b.iter_subfactors()

    def __str__(self):
        return f'(* {self.a} {self.b})'


class Power(MathObject):
    operator = '^'

    def __init__(self, base, exponent):
        self._base = MathObject.fromobj(base)
        self._exponent = MathObject.fromobj(exponent)

    @property
    def base(self):
        return self._base

    @property
    def exponent(self):
        return self._exponent

    def simplify(self):
        if isinstance(self.base, Number) and isinstance(self.exponent, Number):
            return Number(self.base.value ** self.exponent.value)
        return Power(self.base.simplify(), self.exponent.simplify())

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
        self._rational = rational
        if isinstance(value, Number):
            self._value = value.value
        elif isinstance(value, int):
            self._value = value
        else:
            raise TypeError('only int type is supported for numbers as of now')

    @property
    def rational(self):
        return self._rational

    @property
    def value(self):
        return self._value

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
        self._symbol = symbol

    @property
    def symbol(self):
        return self._symbol

    def __eq__(self, other):
        if not isinstance(other, Variable):
            return NotImplemented
        return self.symbol == other.symbol

    def __hash__(self):
        return hash(self.symbol)

    def __str__(self):
        if all('a' <= c <= 'z' or 'A' <= c <= 'Z' for c in self.symbol):
            return f'{self.symbol}'
        else:
            return f'{self.symbol!r}'
