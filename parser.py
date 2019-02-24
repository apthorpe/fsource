#!/usr/bin/env python
from __future__ import print_function
import lexer
import re

class TokenStream:
    def __init__(self, lexer):
        self.lexer = iter(lexer)
        self.cat = None
        self.token = None
        self.advance()

    def advance(self):
        token = self.token
        try:
            self.cat, self.token = next(self.lexer)
            print ("ADV", self.cat, self.token)
        except StopIteration:
            self.cat, self.token = 0, '<$>'
            print ("END OF INPUT")
        return token

    def expect(self, expected):
        if self.token.lower() != expected:
            raise ValueError("Expected %s, got %s", expected, self.token)
        self.advance()

    def expect_cat(self, expected):
        if self.cat != expected:
            raise ValueError("Expected cat %d, got %d", expected, self.cat)
        self.advance()

    def marker(self, marker):
        if self.token.lower() == marker:
            self.advance()
            return True
        else:
            return False


class IgnoreHandler:
    def __init__(self):
        self.glue = 100000

    def expr(self, tokens, head_result):
        tokens.advance()
        return head_result


class LiteralHandler:
    def __init__(self, action):
        self.action = action
        self.glue = 100000

    def expr(self, tokens, head_result):
        if head_result is not None:
            raise ValueError("Two literals in a row are forbidden")
        return self.action(tokens.advance())


class PrefixHandler:
    def __init__(self, parser, symbol, subglue, action):
        self.parser = parser
        self.symbol = symbol
        self.subglue = subglue
        self.action = action

    def expr(self, tokens, head_result):
        if head_result is not None:
            raise ValueError("%s is a prefix operator" % self.symbol)
        tokens.advance()
        result = self.parser.expression(tokens, self.subglue)
        return self.action(result)


class InfixHandler:
    def __init__(self, parser, symbol, glue, assoc, action):
        if assoc not in ('left', 'right'):
            raise ValueError("Associativity must be either left or right")

        self.parser = parser
        self.symbol = symbol
        self.glue = glue
        self.subglue = glue + (0 if assoc == 'right' else 1)
        self.action = action

    def expr(self, tokens, head_result):
        if head_result is None:
            raise ValueError("%s is an infix operator" % self.symbol)
        tokens.advance()
        tail_result = self.parser.expression(tokens, self.subglue)
        return self.action(head_result, tail_result)


class PrefixOrInfix:
    def __init__(self, prefix, infix):
        self.prefix = prefix
        self.infix = infix
        self.glue = infix.glue

    def expr(self, tokens, head_result):
        if head_result is None:
            return self.prefix.expr(tokens, None)
        else:
            return self.infix.expr(tokens, head_result)


class ParensHandler:
    def __init__(self, parser, parens_action, impl_do_action, do_control_action):
        self.parser = parser
        self.parens_action = parens_action
        self.impl_do_action = impl_do_action
        self.do_control_action = do_control_action

    def handle_implied_do(self, tokens, head_result):
        result = head_result
        args = []
        while tokens.token == ',':
            tokens.advance()
            args.append(result)
            result = self.parser.expression(tokens)

        dovar = result
        tokens.expect('=')
        start = self.parser.expression(tokens)
        tokens.expect(',')
        stop = self.parser.expression(tokens)
        if tokens.marker(','):
            step = self.parser.expression(tokens)
        else:
            step = None
        tokens.expect(')')
        return self.impl_do_action(
                        self.do_control_action(dovar, start, stop, step),
                        *args)

    def expr(self, tokens, head_result):
        if head_result is not None:
            raise ValueError("Invalid call type %s" % self.begin_symbol)
        tokens.advance()
        result = self.parser.expression(tokens)

        if tokens.token == ',':
            return self.handle_implied_do(tokens, result)

        tokens.expect(")")
        return self.parens_action(result)


class SliceHandler:
    def __init__(self, parser, glue, inner_glue, action):
        self.parser = parser
        self.glue = glue
        self.inner_glue = inner_glue
        self.action = action

    def expr(self, tokens, head_result):
        slice_begin = head_result
        tokens.advance()
        slice_end = self.parser.expression(tokens, self.inner_glue, False)
        if tokens.marker(":"):
            slice_stride = self.parser.expression(tokens, self.inner_glue)
            if tokens.token == ":":
                raise ValueError("Malformed slice: Too many ::")
        else:
            slice_stride = None
        return self.action(slice_begin, slice_end, slice_stride)


class SubscriptHandler:
    def __init__(self, parser, glue, seq_action, slice_action):
        self.parser = parser
        self.glue = glue
        self.slice_handler = SliceHandler(parser, -2, 0, slice_action)
        self.action = seq_action

    def expr(self, tokens, head_result):
        if head_result is None:
            raise ValueError("Invalid parenthesis type ()")

        tokens.advance()
        seq = []
        result = None
        while True:
            # Expression eats comments, cont'ns, etc., so we should be OK.
            result = self.parser.expression(tokens, expect_result=False)
            token = tokens.token
            if tokens.token == ":":
                result = self.slice_handler.expr(tokens, result)
                token = tokens.token
            if token == "," or token == ")":
                if result is None:
                    raise ValueError("Expecting result here")
                tokens.advance()
                seq.append(result)
                result = None
                if token == ")":
                    return self.action(head_result, *seq)
            else:
                raise ValueError("Unexpected token %s" % token)

class InplaceArrayHandler:
    def __init__(self, parser, seq_action):
        self.parser = parser
        self.glue = 100000
        self.action = seq_action

    def expr(self, tokens, head_result):
        if head_result is not None:
            raise ValueError("Invalid subscript type (/ /)")

        tokens.advance()
        seq = []
        result = None
        while True:
            result = self.parser.expression(tokens, expect_result=False)
            token = tokens.token
            if token == ",":
                if result is None:
                    raise ValueError("Expecting result here")
                tokens.advance()
                seq.append(result)
                result = None
            elif token == "/)":
                if result is None:
                    if seq: raise ValueError("Expecting expression")
                else:
                    seq.append(result)
                tokens.advance()
                return self.action(*seq)
            else:
                raise ValueError("Unexpected token %s" % token)


class EndExpressionMarker:
    def __init__(self):
        self.glue = -1

    def expr(self, tokens, head_result):
        raise ValueError("Empty expression")


def _opt(x): return "null" if x is None else x

class DefaultActions:
    # Blocks
    def block(self, *s): return "(block %s)" % " ".join(map(_opt, s))
    def if_(self, *b): return "(if %s)" % " ".join(map(_opt, b))
    def arith_if(self, a, b, c): return "(arith_if %s %s %s)" % (a, b, c)

    # Expression actions
    def eqv(self, l, r): return "(eqv %s %s)" % (l, r)
    def neqv(self, l, r): return "(neqv %s %s)" % (l, r)
    def or_(self, l, r): return "(or %s %s)" % (l, r)
    def and_(self, l, r): return "(and %s %s)" % (l, r)
    def not_(self, op): return "(not %s)" % op
    def eq(self, l, r): return "(eq %s %s)" % (l, r)
    def ne(self, l, r): return "(ne %s %s)" % (l, r)
    def le(self, l, r): return "(le %s %s)" % (l, r)
    def lt(self, l, r): return "(lt %s %s)" % (l, r)
    def ge(self, l, r): return "(ge %s %s)" % (l, r)
    def gt(self, l, r): return "(gt %s %s)" % (l, r)
    def concat(self, l, r): return "(// %s %s)" % (l, r)
    def plus(self, l, r): return "(+ %s %s)" % (l, r)
    def pos(self, op): return "(pos %s)" % op
    def minus(self, l, r): return "(- %s %s)" % (l, r)
    def neg(self, op): return "(neg %s)" % op
    def mul(self, l, r): return "(* %s %s)" % (l, r)
    def div(self, l, r): return "(/ %s %s)" % (l, r)
    def pow(self, l, r): return "(pow %s %s)" % (l, r)
    def resolve(self, l, r): return "(%% %s %s)" % (l, r)
    def kind(self, l, r): return "(_ %s %s)" % (l, r)
    def parens(self, op): return op
    def call(self, fn, *args): return "(() %s %s)" % (fn, " ".join(args))
    def slice(self, b, e, s): return "(: %s %s %s)" % tuple(map(_opt, (b,e,s)))
    def array(self, *args): return "(array %s)" % " ".join(args)
    def impl_do(self, c, *args): return "(implied_do %s %s)" % (c, " ".join(args))
    def do_control(self, v, b, e, s):
        return "(do_control %s %s %s %s)" % (v, b, e, _opt(s))
    def unary(self, op): return "(unary %s)" % op
    def binary(self, l, r): return "(binary %s %s)" % (l, r)

    # Literals actions
    def true(self): return "true"
    def false(self): return "false"
    def int(self, tok): return tok
    def float(self, tok): return tok
    def string(self, tok): return "(string %s)" % repr(lexer.parse_string(tok))
    def radix(self, tok): return "(radix %s)" % tok
    def word(self, tok): return repr(tok)


class ExpressionParser:
    def __init__(self, actions):
        operators = {
            ",":      EndExpressionMarker(),
            ":":      EndExpressionMarker(),
            "=":      EndExpressionMarker(),
            ".eqv.":  InfixHandler(self, ".eqv.",  20, 'right', actions.eqv),
            ".neqv.": InfixHandler(self, ".neqv.", 20, 'right', actions.neqv),
            ".or.":   InfixHandler(self, ".or.",   30, 'right', actions.or_),
            ".and.":  InfixHandler(self, ".and.",  40, 'right', actions.and_),
            ".not.":  PrefixHandler(self, ".not.",  50, actions.not_),
            ".eq.":   InfixHandler(self, ".eq.",   60, 'left', actions.eq),
            ".ne.":   InfixHandler(self, ".neq.",  60, 'left', actions.ne),
            ".le.":   InfixHandler(self, ".le.",   60, 'left', actions.le),
            ".lt.":   InfixHandler(self, ".lt.",   60, 'left', actions.lt),
            ".ge.":   InfixHandler(self, ".ge.",   60, 'left', actions.ge),
            ".gt.":   InfixHandler(self, ".gt.",   60, 'left', actions.gt),
            "//":     InfixHandler(self, "//",     70, 'left', actions.concat),
            "+":      PrefixOrInfix(
                        PrefixHandler(self, "+",   110, actions.pos),
                        InfixHandler (self, "+",    80, 'left', actions.plus)
                        ),
            "-":      PrefixOrInfix(
                        PrefixHandler(self, "-",   110, actions.neg),
                        InfixHandler(self, "-",    80, 'left', actions.minus)
                        ),
            "*":      InfixHandler(self, "*",      90, 'left', actions.mul),
            "/":      InfixHandler(self, "/",      90, 'left', actions.div),
            "**":     InfixHandler(self, "**",    100, 'right', actions.pow),
            "%":      InfixHandler(self, "%",     130, 'left', actions.resolve),
            "_":      InfixHandler(self, "_",     130, 'left', actions.kind),
            "(":      PrefixOrInfix(
                        ParensHandler(self, actions.parens, actions.impl_do,
                                      actions.do_control),
                        SubscriptHandler(self, 140, actions.call, actions.slice)
                        ),
            ")":      EndExpressionMarker(),
            "(/":     InplaceArrayHandler(self, actions.array),
            "/)":     EndExpressionMarker(),
            ".true.": LiteralHandler(actions.true),
            ".false.": LiteralHandler(actions.false),
            }

        # Fortran 90 operator aliases
        operators["=="] = operators[".eq."]
        operators["/="] = operators[".ne."]
        operators["<="] = operators[".le."]
        operators[">="] = operators[".ge."]
        operators["<"]  = operators[".lt."]
        operators[">"]  = operators[".gt."]

        cat_switch = (
            EndExpressionMarker(),                  # 0 end of input
            IgnoreHandler(),                        # 1 line number
            IgnoreHandler(),                        # 2 preprocessor
            EndExpressionMarker(),                  # 3 end of stmt
            LiteralHandler(actions.string),         # 4 string
            LiteralHandler(actions.float),          # 5 float
            LiteralHandler(actions.int),            # 6 int
            LiteralHandler(actions.radix),          # 7 radix
            EndExpressionMarker(),                  # 8 bracketed slash
            None,                                   # 9 operator
            PrefixOrInfix(
                PrefixHandler(self, '.unary.', 120, actions.unary),
                InfixHandler(self, '.binary.', 10, 'left', actions.binary)
                ),
            LiteralHandler(actions.word),           # 10 word
            )

        self._operators = operators
        self._cat_switch = cat_switch

    def _get_handler(self, cat, token):
        if cat == 9:
            return self._operators[token.lower()]
        else:
            return self._cat_switch[cat]

    def expression(self, tokens, min_glue=0, expect_result=True):
        result = None
        while True:
            handler = self._get_handler(tokens.cat, tokens.token)
            #print (tokens.cat, tokens.token, handler, result)
            if handler.glue < min_glue:
                if expect_result and result is None:
                    raise ValueError("Expecting expression")
                return result
            result = handler.expr(tokens, result)


class DiscardStatement:
    def __init__(self): pass

    def stmt(self, tokens, label, block_name):
        while tokens.cat != 3 and tokens.cat != 0:
            tokens.advance()
        tokens.advance()
        return None


class IfConstruct:
    def __init__(self, parser, if_action=None, arith_if_action=None):
        self.parser = parser
        self.if_action = if_action
        self.arith_if_action = arith_if_action

    def _handle_blockname(self, tokens, block_name):
        if tokens.cat == 10:
            if tokens.token.lower() != block_name:
                raise ValueError("Block name inconsistent")
            tokens.advance()
        tokens.expect_cat(3)  # EOS

    def _handle_condition(self, tokens):
        tokens.expect('(')
        condition = self.parser.expression(tokens)
        tokens.expect(')')
        return condition

    def stmt(self, tokens, label, block_name):
        tokens.advance()
        cases = []
        condition = self._handle_condition(tokens)

        if tokens.marker('then'):
            # IF BLOCK
            tokens.expect_cat(3)
            then_block = self.parser.block(tokens)
            cases += [condition, then_block]

            while tokens.marker('else'):
                if tokens.marker('if'):
                    condition = self._handle_condition(tokens)
                    tokens.expect('then')
                    self._handle_blockname(tokens, block_name)
                    then_block = self.parser.block(tokens)
                    cases += [condition, then_block]
                else:
                    self._handle_blockname(tokens, block_name)
                    then_block = self.parser.block(tokens)
                    cases += [None, then_block]

            tokens.expect('end')
            tokens.marker('if')
            self._handle_blockname(tokens, block_name)
            return self.if_action(*cases)
        elif tokens.cat == 6:
            # ARITHMETIC IF STATEMENT
            first = tokens.advance()
            tokens.expect(",")
            second = tokens.advance()
            tokens.expect(",")
            third = tokens.advance()
            tokens.expect_cat(3)
            return self.arith_if_action(first, second, third)
        else:
            # IF STATEMENT
            then_block = self.parser.stmt(tokens)
            return self.if_action(condition, then_block)


class WhereConstruct:
    def __init__(self, parser, action=None):
        self.parser = parser
        self.action = action

    def _handle_blockname(self, tokens, block_name):
        if tokens.cat == 10:
            if tokens.token.lower() != block_name:
                raise ValueError("Block name inconsistent")
            tokens.advance()
        tokens.expect_cat(3)  # EOS

    def _handle_condition(self, tokens):
        tokens.expect('(')
        condition = self.parser.expression(tokens)
        tokens.expect(')')
        return condition

    def stmt(self, tokens, label, block_name):
        tokens.advance()
        cases = []
        condition = self._handle_condition(tokens)

        if tokens.cat == 3:
            # WHERE BLOCK
            tokens.advance()
            where_block = self.parser.block(tokens)
            cases += [condition, where_block]

            while tokens.marker('elsewhere'):
                if tokens.token == '(':
                    condition = self._handle_condition(tokens)
                    self._handle_blockname(tokens, block_name)
                    where_block = self.parser.block(tokens)
                    cases += [condition, where_block]
                else:
                    self._handle_blockname(tokens, block_name)
                    where_block = self.parser.block(tokens)
                    cases += [None, where_block]

            tokens.expect('end')
            tokens.marker('where')
            self._handle_blockname(tokens, block_name)
            return self.action(*cases)
        else:
            # WHERE STATEMENT
            where_block = self.parser.stmt(tokens)
            return self.action(condition, where_block)


class BlockParser:
    def __init__(self, actions):
        self.expr_parser = ExpressionParser(actions)
        self.actions = actions
        self._stmt_headers = {
            # unnecessary statements to understand
            'cycle': DiscardStatement(),
            'exit': DiscardStatement(),
            'return': DiscardStatement(),
            'stop': DiscardStatement(),
            'call': DiscardStatement(),
            'entry': DiscardStatement(),
            'data': DiscardStatement(),
            'allocate': DiscardStatement(),
            'deallocate': DiscardStatement(),
            'assign': DiscardStatement(),
            'nullify': DiscardStatement(),
            'open': DiscardStatement(),
            'backspace': DiscardStatement(),
            'inquire': DiscardStatement(),
            'format': DiscardStatement(),
            'read': DiscardStatement(),
            'write': DiscardStatement(),
            'print': DiscardStatement(),
            'rewind': DiscardStatement(),
            'pause': DiscardStatement(),
            'close': DiscardStatement(),
            'endfile': DiscardStatement(),
            'if': IfConstruct(self, actions.if_, actions.arith_if),
            'where': WhereConstruct(self),
            }
        self._endblock_markers = {
            'end',
            'else',
            'elsewhere',
            }

    def expression(self, tokens, min_glue=0, expect_result=True):
        return self.expr_parser.expression(tokens, min_glue, expect_result)

    def stmt(self, tokens, label=None, block_name=None):
        stmt_type = self._stmt_headers[tokens.token.lower()]
        return stmt_type.stmt(tokens, label, block_name)

    def block(self, tokens):
        stmts = []
        while True:
            if tokens.cat == 3:  # empty statement
                tokens.advance()
                continue
            if tokens.cat == 1:  # line number
                label = self.actions.label(self.tokens.advance())
            else:
                label = None
            if tokens.cat == 0 or tokens.token.lower() in self._endblock_markers:
                break
            stmts.append(self.stmt(tokens, label))

        return self.actions.block(*stmts)


lexre = lexer.LEXER_REGEX
#program = """x(3:1, 4, 5::2) * &   ! something
#&  (3 + 5)"""
#program = "+1 + 3 * x(::1, 2:3) * (/ /) * 4 ** (5 + 1) ** sin(6, 1) + (/ 1, 2, (i, i=1,5), 3 /)"
program = """
    if (x == 3) then
        call something(3)
        return
    else if (x == 5) then
        call something(4)
    else
        call something(5)
    end if
    """
slexer = lexer.tokenize_regex(lexre, program)
tokens = TokenStream(slexer)
#parser = ExpressionParser(DefaultActions())
#print (parser.expression(tokens))
parser = BlockParser(DefaultActions())
print (parser.block(tokens))
