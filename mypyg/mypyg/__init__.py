import re

from pygments.lexer import Lexer, RegexLexer, bygroups, include, do_insertions
from pygments.token import Text, Comment, Operator, Keyword, Name, \
    String, Number, Punctuation, Literal, Generic

class AtomoLexer(RegexLexer):
    """
    An Atomo lexer.
    """
    name = 'Atomo'
    aliases = ['atomo', 'atomo']
    filenames = ['*.atomo']
    mimetypes = ['text/x-atomo']

    reserved = ['operator', 'macro', 'for-macro', 'this']
    ascii = ['NUL','SOH','[SE]TX','EOT','ENQ','ACK',
             'BEL','BS','HT','LF','VT','FF','CR','S[OI]','DLE',
             'DC[1-4]','NAK','SYN','ETB','CAN',
             'EM','SUB','ESC','[FGRU]S','SP','DEL']

    valid_name = r'[:a-zA-Z0-9!%&*+,/<=>?@^_~|-]'
    operator_name = r'[:!#%&*+.\\/<=>?@^|~-]'

    tokens = {
        'root': [
            # Whitespace:
            (r'\s+', Text),
            #(r'--\s*|.*$', Comment.Doc),
            (r'--(?![!#$%&*+./<=>?@\^|_~]).*?$', Comment.Single),
            (r'{-', Comment.Multiline, 'comment'),
            #  Identifiers
            (r'\b(%s)\b(?!%s)' % ('|'.join(reserved), operator_name), Keyword.Reserved),
            (r'[_a-z]' + valid_name + '*:', Name.Function),
            (r'[_a-z]' + valid_name + '*', Name),
            (r'[A-Z]' + valid_name + '*', Keyword.Type),
            #  Operators
            (r'(?![@$~])' + operator_name + '+', Operator),
            #  Numbers
            (r'\d+[eE][+-]?\d+', Number.Float),
            (r'\d+\.\d+([eE][+-]?\d+)?', Number.Float),
            (r'0[oO][0-7]+', Number.Oct),
            (r'0[xX][\da-fA-F]+', Number.Hex),
            (r'\d+', Number.Integer),
            #  Character/String Literals
            (r'\$', String.Char, 'character'),
            (r'"', String, 'string'),
            #  Boolean
            (r"(True|False)", Keyword.Constant),
            #  Quoting
            (r"'" + valid_name + '+', String.Symbol),
            (r"'", String.Symbol),
            (r"`" + valid_name + '+', String.Symbol),
            (r"`", String.Symbol),
            (r"~" + valid_name + '+', String.Interpol),
            (r"~", String.Interpol),
            #  Partickes
            (r"@" + valid_name + '+', String.Interpol),
            (r"@", String.Interpol),
            #  Punctuation
            (r'[][(),;{}|]', Punctuation),
            ],
        'comment': [
            # Multiline Comments
            (r'[^-{}]+', Comment.Multiline),
            (r'{-', Comment.Multiline, '#push'),
            (r'-}', Comment.Multiline, '#pop'),
            (r'[-{}]', Comment.Multiline),
            ],
        'character': [
            (r"[^\\]", String.Char, '#pop'),
            (r"\\[^\s]+", String.Escape, '#pop'),
            ],
        'string': [
            (r'[^\\"]+', String),
            (r"\\", String.Escape, 'escape'),
            ('"', String, '#pop'),
            ],
        'escape': [
            (r'[abfnrtv"\'&\\]', String.Escape, '#pop'),
            (r'\^[][A-Z@\^_]', String.Escape, '#pop'),
            ('|'.join(ascii), String.Escape, '#pop'),
            (r'o[0-7]+', String.Escape, '#pop'),
            (r'x[\da-fA-F]+', String.Escape, '#pop'),
            (r'\d+', String.Escape, '#pop'),
            (r'\s+\\', String.Escape, '#pop'),
            ],
        }
