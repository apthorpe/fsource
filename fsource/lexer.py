"""
Lexical analysis for free-form Fortran, up to version 2008.

Uses regular expressions to split up a Fortran source file into a sequence
of tokens, where each token is has a category attached to it signifying its
type (literal, identifier, comment, etc.).

Generate pairs of the form `(cat, token)`, where `cat` is a category code
between 0 and 12 signifying the type of token returned, and `token` is the
portion of source code matching the token.  Whitespace is generally
ignored.

Lexical analysis must deal with three ambiguities in the Fortran grammar:

 1. The string '::' can mean an empty slice or a separator token.  The
    lexer always returns single ':', which means one gets '::' as a
    sequence of two ':' and cannot detect whitespace within a seperator.

 2. '(//)' can mean an empty inplace array or an overloaded '//' operator,
    and '(/)', which is ambiguous for a similar reason.  To work around
    this lexer will return a token of category `CAT_BRACKETED_SLASH`, and
    the application must disambiguate.

 3. The 'FORMAT' statement is a bit of an oddball, as it allows tokens that
    are illegal everywhere else, e.g., 3I6 or ES13.2.  The lexer works
    around this by returning the format line as single token of category
    `CAT_FORMAT`.

Copyright 2019 Markus Wallerberger
Released under the GNU Lesser General Public License, Version 3 only.
See LICENSE.txt for permissions on usage, modification and distribution
"""
from __future__ import print_function
import sys
import re

from . import common
from . import splicer

# Python 2/3 compatibility
if sys.version_info >= (3,):
    _string_like_types = str, bytes,
    _maketrans = str.maketrans
else:
    import string
    _string_like_types = basestring,
    _maketrans = string.maketrans


class LexerError(common.ParsingError):
    @property
    def error_type(self): return "lexer error"


def tokenize_regex(regex, text, lineno=None):
    """Tokenizes text using the groups in the regex specified

    Expects a `regex`, where different capturing groups correspond to different
    categories of tokens.  Iterates through all matches of the regex on `text`,
    returning the highest matching category with the associated token (group
    text).
    """
    try:
        for match in regex.finditer(text):
            cat = match.lastindex
            yield lineno, match.start(cat), cat, match.group(cat)
    except IndexError as e:
        raise LexerError(None, lineno, match.start(), match.end(), text,
                         "invalid token")


def get_lexer_regex(preproc=False):
    """Return regular expression for parsing free-form Fortran 2008"""
    endline = r"""(?:\n|\r\n?)"""
    comment = r"""(?:![^\r\n]*)"""
    skip_ws = r"""[\t ]*"""
    postq = r"""(?!['"\w])"""
    dq_string = r""""(?:""|[^"\r\n])*"{postq}""".format(postq=postq)
    sq_string = r"""'(?:''|[^'\r\n])*'{postq}""".format(postq=postq)
    postnum = r"""(?!['"&0-9A-Za-z]|\.[0-9])"""
    integer = r"""\d+{postnum}""".format(postnum=postnum)
    decimal = r"""(?:\d+\.\d*|\.\d+)"""
    exponent = r"""(?:[dDeE][-+]?\d+)"""
    real = r"""(?:{decimal}{exponent}?|\d+{exponent}){postnum}""" \
           .format(decimal=decimal, exponent=exponent, postnum=postnum)
    binary = r"""[Bb](?:'[01]+'|"[01]+"){postq}""".format(postq=postq)
    octal = r"""[Oo](?:'[0-7]+'|"[0-7]+"){postq}""".format(postq=postq)
    hexadec = r"""[Zz](?:'[0-9A-Fa-f]+'|"[0-9A-Fa-f]+"){postq}""" \
              .format(postq=postq)
    operator = r"""\(/?|\)|[-+,:_%\[\]]|=[=>]?|\*\*?|\/[\/=)]?|[<>]=?"""
    builtin_dot = r"""(?:eq|ne|l[te]|g[te]|n?eqv|not|and|or)"""
    dotop = r"""[A-Za-z]+"""
    word = r"""[A-Za-z][A-Za-z0-9_]*(?![A-Za-z0-9_&])"""
    fortran_token = r"""(?ix)
          {skipws}(?:
            (; | {comment}?{endline})           #  1 end of statement
          | ({sqstring} | {dqstring})           #  2 strings
          | ({real})                            #  3 real
          | ({int})                             #  4 ints
          | ({binary} | {octal} | {hex})        #  5 radix literals
          | \.\s* (?:
              ( true | false )                  #  6 boolean
            | ( {builtin_dot} )                 #  7 built-in dot operator
            | ( {dotop} )                       #  8 custom dot operator
            ) \s*\.
          | \( {skipws} (//?) {skipws} \)       #  9 bracketed slashes
          | ({operator})                        # 10 symbolic operator
          | ({word})                            # 11 word
          | [^ \t]+                             #    invalid token
          )
        """.format(
                skipws=skip_ws, endline=endline, comment=comment,
                sqstring=sq_string, dqstring=dq_string,
                real=real, int=integer, binary=binary, octal=octal,
                hex=hexadec, operator=operator, builtin_dot=builtin_dot,
                dotop=dotop, word=word
                )
    return re.compile(fortran_token)


CAT_DOLLAR = 0
CAT_EOS = 1
CAT_STRING = 2
CAT_FLOAT = 3
CAT_INT = 4
CAT_RADIX = 5
CAT_BOOLEAN = 6
CAT_BUILTIN_DOT = 7
CAT_CUSTOM_DOT = 8
CAT_BRACKETED_SLASH = 9
CAT_SYMBOLIC_OP = 10
CAT_WORD = 11
CAT_PREPROC = 12
CAT_INCLUDE = 13
CAT_FORMAT = 14

CAT_NAMES = ('eof', 'eos', 'string', 'float', 'int', 'radix',
             'bool', 'dotop', 'custom_dotop', 'bracketed_slash', 'symop',
             'word', 'preproc', 'include', 'format')

LINECAT_TO_CAT = {
    splicer.LINECAT_PREPROC: CAT_PREPROC,
    splicer.LINECAT_INCLUDE: CAT_INCLUDE,
    splicer.LINECAT_FORMAT: CAT_FORMAT
    }


def _string_lexer_regex(quote):
    pattern = r"""(?x) ({quote}{quote}) | ([^{quote}]+)""".format(quote=quote)
    return re.compile(pattern)


def _string_lexer_actions():
    return (None,
            lambda tok: tok[0],
            lambda tok: tok,
            )


STRING_LEXER_REGEX = {
    "'": _string_lexer_regex("'"),
    '"': _string_lexer_regex('"'),
    }
STRING_LEXER_ACTIONS = _string_lexer_actions()


def parse_string(tok):
    """Translates a Fortran string literal to a Python string"""
    actions = STRING_LEXER_ACTIONS
    return "".join(actions[cat](token) for (cat, token)
                   in tokenize_regex(STRING_LEXER_REGEX[tok[0]], tok[1:-1]))


CHANGE_D_TO_E = _maketrans('dD', 'eE')


def parse_float(tok):
    """Translates a Fortran real literal to a Python float"""
    return float(tok.translate(CHANGE_D_TO_E))


def parse_bool(tok):
    """Translates a Fortran boolean literal to a Python boolean"""
    return {'true': True, 'false': False}[tok.lower()]


def parse_radix(tok):
    """Parses a F03-style x'***' literal"""
    base = {'b': 2, 'o': 8, 'z': 16}[tok[0].lower()]
    return int(tok[2:-1], base)


def lex_buffer(buffer, form=None):
    """Perform lexical analysis for an opened free-form Fortran file."""
    # check for buffer
    if isinstance(buffer, _string_like_types):
        raise ValueError("Expect open file or other sequence of lines")
    if form is None:
        form, _ = common.guess_form(buffer.name)

    lexer_regex = get_lexer_regex()
    linecat_to_cat = LINECAT_TO_CAT
    lines_iter = splicer.get_splicer(form)

    fname = buffer.name
    for lineno, linecat, line in lines_iter(buffer):
        try:
            yield lineno, 0, linecat_to_cat[linecat], line
        except KeyError:
            try:
                for token_tuple in tokenize_regex(lexer_regex, line, lineno):
                    yield token_tuple
            except LexerError as e:
                e.fname = fname
                raise e

    # Make sure last line is terminated, then yield terminal token
    yield lineno+1, 0, CAT_EOS, '\n'
    yield lineno+1, 0, CAT_DOLLAR, '<$>'


def lex_snippet(fstring):
    """Perform lexical analysis of parts of a line"""
    return tuple(tokenize_regex(get_lexer_regex(), fstring)) \
           + ((CAT_DOLLAR, ''),)


def pprint(lexer, out, filename=None):
    """Make nicely formatted JSON output from lexer output"""
    from json.encoder import encode_basestring
    out.write('[\n')
    out.write('["lex_version", "1.0"],\n')
    out.write('["filename", %s],\n' % encode_basestring(filename))
    for cat, token in lexer:
        out.write('["%s",%s]' % (CAT_NAMES[cat], encode_basestring(token)))
        if cat == CAT_EOS or cat == CAT_PREPROC:
            out.write(',\n')
        elif cat == CAT_DOLLAR:
            out.write('\n]\n')
        else:
            out.write(', ')
