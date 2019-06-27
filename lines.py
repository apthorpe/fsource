#!/usr/bin/env python
"""
Line handling and preprocessing for free/fixed-form Fortran.

Uses regular expressions to split up a Fortran source file into a sequence
of logical lines together with line types.

Lexical analysis must deal with three ambiguities in the Fortran grammar:

 1. The 'FORMAT' statement is a bit of an oddball, as it allows tokens that
    are illegal everywhere else, e.g., 3I6 or ES13.2.  The lexer works
    around this by returning the format line as single token of category
    `CAT_FORMAT`.

Author: Markus Wallerberger
"""
from __future__ import print_function
import sys
import re

def _freeform_line_regex():
    """Discriminate line type"""
    ws = r"""[ \t]+"""
    endline = r"""(?:\n|\r\n?)"""
    anything = r"""[^\r\n]*"""
    comment = r"""(?:![^\r\n]*)"""
    lineno = r"""[0-9]{1,5}(?=[ \t])"""
    include = r"""include{ws}{anything}""".format(ws=ws, anything=anything)
    preproc = r"""\#{anything}""".format(anything=anything)
    atom = r"""(?: [^!&'"\r\n] | '(?:''|[^'\r\n])*' | "(?:""|[^"\r\n])*" )"""
    format = r"""{lineno}{ws}format{ws}\({anything}""".format(
                    ws=ws, anything=anything, lineno=lineno)
    truncstr = r"""(?: '(?:''|[^'\r\n])*
                     | "(?:""|[^"\r\n])*
                     )
            """
    line = r"""(?ix) ^[ \t]*
            (?: ( {preproc} {endline} )         # 1 preprocessor stmt
              | ( {include} {endline} )         # 2 include line
              | ( {format} {endline} )          # 3 format stmt
              | ( {atom}* ) (?:                  # 4 whole line part
                  ( {comment}? {endline} )           # 5 full line end
                | ( & [ \t]* {comment}? {endline} )  # 6 truncated line end
                | ( {truncstr} ) &[ \t]* {endline}   # 7 truncated string line end
                )
              ) $
            """.format(preproc=preproc, include=include, format=format,
                       anything=anything, atom=atom, truncstr=truncstr,
                       comment=comment, endline=endline)

    return re.compile(line)

FREE_PREPROC = 1
FREE_INCLUDE = 2
FREE_FORMAT = 3
FREE_WHOLE_PART = 4
FREE_FULL_END = 5
FREE_TRUNC_END = 6
FREE_TRUNC_STRING_END = 7

FREE_REGEX = _freeform_line_regex()

def _freeform_contd_regex():
    """Discriminate line type"""
    ws = r"""[ \t]+"""
    anything = r"""[^\r\n]+"""
    endline = r"""(?:\n|\r\n?)"""
    comment = r"""(?:![^\r\n]*)"""
    line = r"""(?x) ^[ \t]*
            (?:
                ( {comment} {endline} )              # 1 comment line (ignored)
              | &? [ \t]* ( {anything} {endline} )   # 2 spill
              ) $
            """.format(anything=anything, comment=comment, endline=endline)
    return re.compile(line)

CONTD_COMMENT = 1
CONTD_SPILL = 2

FF_CONTD_REGEX = _freeform_contd_regex()


LINECAT_NORMAL = 1
LINECAT_INCLUDE = 2
LINECAT_FORMAT = 3
LINECAT_PREPROC = 4

LINECAT_NAMES = (None, 'line', 'include', 'format', 'preproc', 'comment')

def free_form_lines(buffer):
    """Mend lines in free-form Fortran file"""
    # Iterate through lines of the file.  Fortran allows to split tokens
    # across lines, which is why we build up the whole line before giving
    # it to the tokenizer.
    stub = ''
    trunc_str = ''

    for line in buffer:
        # Handle truncated lines
        if stub:
            if trunc_str:
                line = trunc_str + line
                trunc_str = ''
            else:
                match = FF_CONTD_REGEX.match(line)
                if match.lastindex == CONTD_COMMENT:
                    continue
                line = match.group(2)

        # Now parse current (potentially preprocessed) line
        match = FREE_REGEX.match(line)
        discr = match.lastindex
        if discr == FREE_FULL_END:
            stub += match.group(FREE_WHOLE_PART) + match.group(FREE_FULL_END)
            yield LINECAT_NORMAL, stub
            stub = ''
        elif discr >= FREE_TRUNC_END:
            stub += match.group(FREE_WHOLE_PART)
            if discr == FREE_TRUNC_STRING_END:
                trunc_str = match.group(FREE_TRUNC_STRING_END)
        elif discr == FREE_PREPROC:
            trunc_str += match.group(FREE_PREPROC)
            if trunc_str[-1] == '\\':
                trunc_str = trunc_str[:-1]
                stub = trunc_str
            else:
                yield LINECAT_PREPROC, trunc_str
                stub = ''
                trunc_str = ''
        elif discr == FREE_FORMAT:
            yield LINECAT_FORMAT, line
        else:
            yield LINECAT_INCLUDE, line

    if stub or trunc_str:
        raise RuntimeError("line continuation marker followed by end of file")


def _fixedform_line_regex():
    line = """(?ix) ^
        (?: [cC*!](.*)                                      # 1: comment
            | [ ]{5}[^ 0] (.*)                              # 2: continuation
            | [ ]{6} [ \t]* (\#.*)                          # 3: preprocessor
            | [ ]{6} [ \t]* (include[ \t].*)                # 4: include line
            | ( [ ][\d ]{4}[ ] [ \t]* format[ \t]*\(.* )    # 5: format line
            | ( [ ][\d ]{4}[ ] [ \t]* .* | )                # 6: normal line
            ) $
            """
    return re.compile(line)

FIXED_COMMENT = 1
FIXED_CONTD = 2
FIXED_PREPROC = 3
FIXED_INCLUDE = 4
FIXED_FORMAT = 5
FIXED_OTHER = 6

FIXED_REGEX = _fixedform_line_regex()

def fixed_form_lines(buffer, margin=72):
    """Handle line continuation in fixed form fortran"""

    # The continuation markers at fixed-form lines are at the *following*
    # line, so we need to store the previous current line
    cat = None
    stub = None

    for line in buffer:
        line = line[:margin].rstrip()
        match = FIXED_REGEX.match(line)
        discr = match.lastindex

        if discr == FIXED_CONTD:
            if not stub:
                raise RuntimeError("No valid line to continue:\n" + line)
            stub += match.group(FIXED_CONTD)
            continue
        else:
            if stub and discr != FIXED_COMMENT:
                yield cat, stub
                stub = None

        if discr == FIXED_OTHER:
            cat = LINECAT_NORMAL
            stub = match.group(FIXED_OTHER)
        elif discr == FIXED_COMMENT:
            yield LINECAT_NORMAL, "!" + match.group(FIXED_COMMENT)
        elif discr == FIXED_FORMAT:
            cat = LINECAT_FORMAT
            stub = match.group(FIXED_FORMAT)
        elif discr == FIXED_INCLUDE:
            yield LINECAT_INCLUDE, match.group(FIXED_INCLUDE)
        elif discr == FIXED_PREPROC:
            yield LINECAT_PREPROC, match.group(FIXED_PREPROC)
        else:
            raise RuntimeError("Invalid token")

    # Handle last line
    if stub is not None:
        yield cat, stub


def get_lines(form='free'):
    if form == 'free':
        return free_form_lines
    elif form == 'fixed':
        return fixed_form_lines
    else:
        raise ValueError("form must be either 'free' or 'fixed'")

if __name__ == '__main__':
    import sys
    import argparse

    parser = argparse.ArgumentParser(description='Preproc for free-form Fortran')
    parser.add_argument('files', metavar='FILE', type=str, nargs='+',
                        help='files to lex')
    parser.add_argument('--fixed-form', dest='form', action='store_const',
                        const='fixed', default='free', help='Fixed form input')
    parser.add_argument('--free-form', dest='form', action='store_const',
                        const='free', help='Free form input')
    parser.add_argument('--no-output', dest='output', action='store_false',
                        default=True,
                        help='perform lexing but do not print result')
    args = parser.parse_args()
    lines = get_lines(args.form)

    for fname in args.files:
        contents = open(fname)
        if args.output:
            for cat, line in lines(contents):
                print("%s: %s" % (LINECAT_NAMES[cat], line), end='')
        else:
            for _ in lines(contents): pass