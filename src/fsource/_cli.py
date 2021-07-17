"""
Command line interface (CLI) for fsource.

Copyright 2019 Markus Wallerberger
Released under the GNU Lesser General Public License, Version 3 only.
See LICENSE.txt for permissions on usage, modification and distribution
"""
from __future__ import print_function
import argparse
import errno
import time
import sys
import json
import textwrap

import re

from . import __version__, __version_tuple__
from . import splicer
from . import lexer
from . import parser
from . import analyzer
from . import common


class Stopwatch:
    """Keeps track of elapsed time since construction"""
    def __init__(self, mytime=time.time):
        """Initialize object and start stopwatch"""
        self.time = mytime
        self.previous = mytime()
        self.initial = self.previous

    def click(self):
        """Start new lap and return time of current lap"""
        elapsed = self.time() - self.previous
        self.previous += elapsed
        return elapsed

    def total(self):
        """Return total elapsed time"""
        return self.time() - self.initial


def get_parser():
    """Return argument parser"""
    command_summary = textwrap.dedent("""\
        Fortran static analyis tool

        available commands:
          parse         construct abstract syntax tree for declarations
          lex           split source into tokens
          splice        split source into sequence of logical lines
          wrap          generate C wrappers for Fortran declarations
          assess        evaluate legacy-ness of source files
        """)

    p = argparse.ArgumentParser(
            prog='fsource',
            formatter_class=argparse.RawDescriptionHelpFormatter,
            description=command_summary,
            usage="fsource COMMAND FILE [FILE ...]"
            )
    p.add_argument('command', metavar='COMMAND',
                   choices=('splice', 'lex', 'parse', 'wrap', 'assess'),
                   help=argparse.SUPPRESS)
    p.add_argument('files', metavar='FILE', type=str, nargs='+',
                   help="Fortran file(s) to process")
    p.add_argument('--version', action='version',
                   version='%(prog)s ' + __version__)
    p.add_argument('--fixed-form', dest='form', action='store_const',
                   const='fixed', default=None, help='force fixed form input')
    p.add_argument('--free-form', dest='form', action='store_const',
                   const='free', help='force free form input')
    p.add_argument('--time', dest='output', action='store_const',
                   const='time', default='json',
                   help='process files but print timings only')
    p.add_argument('--dry-run', dest='output', action='store_const',
                   const='no', help='do not output anything')
    return p


def get_form(fname, force_form):
    """Discern source form (fixed/free) from parameter and file name"""
    if force_form is None:
        is_free, _ = common.guess_form(fname)
        return is_free
    else:
        return force_form


def cmd_splice(args):
    """splice subcommand handler"""
    try:
        for fname in args.files:
            form = get_form(fname, args.form)
            lines = splicer.get_splicer(form)
            contents = open(fname)
            if args.output == 'json':
                for lineno, cat, line in lines(contents):
                    print("%d: %s: %s"
                          % (lineno+1, splicer.LINECAT_NAMES[cat], line), end='')
            else:
                for _ in lines(contents): pass
    except common.ParsingError as e:
        sys.stdout.flush()
        sys.stderr.write("\n" + e.errmsg())


def pprint_lex(mylexer, out, filename=None):
    """Make nicely formatted JSON output from lexer output"""
    encode_string = json.encoder.encode_basestring

    out.write('[\n')
    out.write('["lex_version", %s],\n' %
              ", ".join('"%s"' % v for v in __version_tuple__))
    out.write('["filename", %s],\n' % encode_string(filename))
    for _, _, cat, token in mylexer:
        out.write('["%s",%s]' % (lexer.CAT_NAMES[cat], encode_string(token)))
        if cat == lexer.CAT_EOS or cat == lexer.CAT_PREPROC:
            out.write(',\n')
        elif cat == lexer.CAT_DOLLAR:
            out.write('\n]\n')
        else:
            out.write(', ')


def cmd_lex(args):
    """lex subcommand handler"""
    try:
        for fname in args.files:
            form = get_form(fname, args.form)
            mylexer = lexer.lex_buffer(open(fname), form)
            if args.output == 'json':
                pprint_lex(mylexer, sys.stdout, fname)
            else:
                for _ in mylexer: pass
    except common.ParsingError as e:
        sys.stdout.flush()
        sys.stderr.write("\n\n" + e.errmsg())


def pprint_parser(ast, out, level=0):
    """Make nicely formatted JSON output from parser output"""
    encode_basestring = json.encoder.encode_basestring
    block_elems = {
        'compilation_unit',
        'subroutine_decl',
        'function_decl',
        'interface_body',
        'entity_decl',
        'entity_list',
        'component_block',
        'declaration_block',
        'contained_block',
        'execution_block',
        'type_bound_procedures',
        'common_stmt',
        'common_block',
        'block_data_decl',
        }
    repl = {
        True: 'true',
        False: 'false',
        None: 'null'
        }

    if isinstance(ast, tuple):
        out.write("[" + encode_basestring(ast[0]))
        if ast[0] in block_elems:
            for elem in ast[1:]:
                out.write(",\n" + "    " * (level + 1))
                pprint_parser(elem, out, level + 1)
            out.write("\n" + "    " * level + "]")
        else:
            for elem in ast[1:]:
                out.write(", ")
                pprint_parser(elem, out, level)
            out.write("]")
    else:
        try:
            val = repl[ast]
        except KeyError:
            val = encode_basestring(ast)
        out.write(val)


def cmd_parse(args):
    """parse subcommand handler"""
    try:
        for fname in args.files:
            form = get_form(fname, args.form)
            slexer = lexer.lex_buffer(open(fname), form)
            tokens = parser.TokenStream(slexer, fname=fname)
            ast = parser.compilation_unit(tokens)
            if args.output == 'json':
                pprint_parser(ast, sys.stdout)
                print()
    except common.ParsingError as e:
        sys.stdout.flush()
        sys.stderr.write("\n\n" + e.errmsg())


def cmd_wrap(args):
    """wrapping subcommand handler"""
    def get_asr(fname):
        form = get_form(fname, args.form)
        slexer = lexer.lex_buffer(open(fname), form)
        tokens = parser.TokenStream(slexer, fname=fname)
        ast = parser.compilation_unit(tokens)
        asr = analyzer.TRANSFORMER(ast)
        asr.imbue()
        return asr

    try:
        asr_list = [get_asr(fname) for fname in args.files]
        ns = analyzer.Namespace.union(*(asr.namespace for asr in asr_list))
        # Broadcast the namespace back
        for asr in asr_list:
            asr.namespace.inherit(ns)
        # wrapping configuration
        config = analyzer.Config()
        # Next, generate wrappers
        for asr in asr_list:
            asr.resolve()
            decls = asr.cdecl(config).get()
            if args.output != 'time':
                print (decls)
    except common.ParsingError as e:
        sys.stdout.flush()
        sys.stderr.write("\n\n" + e.errmsg())
        sys.exit(1)


def cmd_assess(args):
    """Evaluate 'legacy'-ness of source files"""
    for fname in args.files:
        try:
            fmt_by_ext, is_preproc_by_ext = common.guess_form(fname)
            ffmt = fmt_by_ext
            if is_preproc_by_ext:
                fpre = 'preprocessed'
            else:
                fpre = 'final'
        except ValueError as e:
            ffmt = 'unknown'
            fpre = 'unknown'

        print("Via guess_form:   {0:s} is {1:s} format and {2:s}".format(fname, ffmt, fpre))

        line_ct = 0
        label_ct = 0
        max_llen = 0

        ccmt_ct = 0
        pat_ccmt = re.compile(r"^C(?ai)")

        excmt_ct = 0
        pat_excmt = re.compile(r"^\s*!(?a)")

        tab_ct = 0
        pat_tab = re.compile(r"\t(?a)")

        lcont_ct = 0
        pat_lcont = re.compile(r"^\s{5}[1-9](?ai)")

        odd_lcont_ct = 0
        pat_odd_lcont = re.compile(r"^\s{5}[^ 1-9](?ai)")

        goto_ct = 0
        pat_goto = re.compile(r"GO\s*TO(?ai)")

        has_while = False
        pat_while = re.compile(r"do\s+while(?ai)")

        has_end_do = False
        pat_end_do = re.compile(r"end\s*do(?ai)")

        contents = open(fname, "U")
        for line in contents:
            # print('line is [{0:s}]'.format(line))
            line_ct += 1

            if pat_tab.search(line):
                tab_ct += 1

            if pat_lcont.search(line):
                lcont_ct += 1
            elif pat_odd_lcont.search(line):
                odd_lcont_ct += 1

            llen = len(line)
            max_llen = max(llen, max_llen)
            # uline = str(line).upper()

            # print('uc(first char) is {0:s}'.format(uline[0]))

            if pat_ccmt.search(line):
                ccmt_ct += 1
            else:
                # print('not a comment [{0:s}]'.format(line))
                if pat_excmt.search(line):
                    excmt_ct += 1
                else:
                    if pat_goto.search(line):
                        goto_ct += 1

                    if pat_while.search(line):
                        has_while = True

                    if pat_end_do.search(line):
                        has_end_do = True

            if llen > 6:
                try:
                    if (int(line[0:4]) > 0) or (int(line[1:4]) > 0):
                        label_ct += 1
                except ValueError:
                    pass

        # line_endings = contents.newlines()
        # print("{0} unique line endings".format(len(line_endings)))
        print("{0} lines".format(line_ct))
        print("{0} legacy comments".format(ccmt_ct))
        if excmt_ct > 0:
            print("{0} modern comments".format(excmt_ct))
        print("{0} legacy labels".format(label_ct))
        print("{0} GOTOs".format(goto_ct))
        print("{0} valid legacy continuations".format(lcont_ct))
        print("{0} non-standard legacy continuations".format(odd_lcont_ct))
        if tab_ct > 0:
            print("{0} tabs (\\t) found".format(tab_ct))
        print("Max line length is {0}".format(max_llen))
        if has_end_do:
            print("Found END DO")
        if has_while:
            print("Found DO WHILE")

        print()


def main():
    """main entry point"""
    p = get_parser()
    args = p.parse_args()
    rabbit = Stopwatch()
    try:
        if args.command == 'assess':
            cmd_assess(args)
        elif args.command == 'splice':
            cmd_splice(args)
        elif args.command == 'lex':
            cmd_lex(args)
        elif args.command == 'parse':
            cmd_parse(args)
        elif args.command == 'wrap':
            cmd_wrap(args)
        if args.output == 'time':
            sys.stderr.write("Elapsed: %g sec\n" % rabbit.total())
    except IOError as e:
        if e.errno != errno.EPIPE:
            raise

if __name__ == '__main__':
    main()
