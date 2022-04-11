# -*- encoding: utf8 -*-
# ------------------------------------------------------------------------------
# main.py
# Created on 04/04/2022
#
# The MIT License
#
#
# Copyright 2022 Jorge A. Gomes
# 
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
# 
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.
# ------------------------------------------------------------------------------

# region IMPORTS

from jsom.lib import JSOM
from typing import Optional, Any, Callable, TypeVar, Generic

# endregion (imports)
# ---------------------------------------------------------
# region EXPORTS


__all__ = [
    'BaseToken',
    'Error',
    'F',
    'Lexer',
    'Source',
    'SourceLn',
    'TokenStream',
    'UnexpectedCharError',
]


# endregion (exports)
# ---------------------------------------------------------
# region CONSTANTS & ENUMS

T = TypeVar('T')
F = Callable[[...], "BaseToken"]

# endregion (constants)
# ---------------------------------------------------------
# region CLASSES


class Error:

    def __init__(self, message: str, *args, **kwargs):
        self.message: str = message
        self.args: Any = args
        self.kwargs: Any = kwargs


class UnexpectedCharError(Error):
    pass


class Source:

    @classmethod
    def load(cls, filename: str) -> "Source":
        with open(filename, 'r', encoding='utf-8') as code:
            return cls(filename, code.read())

    def __init__(self, filename: str, code: Optional[str] = None):
        self.filename: str = filename
        self.code: Optional[str] = code

    def __len__(self) -> int:
        return self.code.__len__()

    def __getitem__(self, key) -> Optional[str]:
        if self.loaded and 0 <= key < len(self.code):
            return self.code[key]
        return None

    @property
    def loaded(self) -> bool:
        return self.code is not None


class SourceLn:

    def __init__(self, source: Source, index: int, line_index: int, line: int, column: int):
        self.index: int = index
        self.line_index: int = line_index
        self.line: int = line
        self.column: int = column
        self.source: Source = source

    def __str__(self):
        return f"Ln {self.line}:{self.column}"

    def advance(self, char: str) -> "SourceLn":
        self.index += 1
        if char == '\n':
            self.line_index = self.index
            self.line += 1
            self.column = 1
        else:
            self.column += 1

        return self

    def copy(self) -> "SourceLn":
        return SourceLn(self.source, self.index, self.line_index, self.line, self.column)


class BaseToken:

    def __init__(self, source: Source, ln: SourceLn, kind: str, value: str | slice):
        self.source: Source = source
        self.ln: SourceLn = ln
        self.kind: str = kind
        self.value: str | slice = value


class Lexer:

    def __init__(self, config: JSOM, source: Source):
        self._config: JSOM = config
        self._source: Source = source
        self._pos: SourceLn = SourceLn(source, -1, -1, 1, 1)
        self._char: str = ''

    @property
    def source(self) -> Source:
        return self._source

    def _kind_subkind(self, kind: str, subkind: str | None) -> str | None:
        delim = self._config.get('subkind_delimiter')
        if isinstance(delim, str) and subkind is not None and kind != subkind:
            return delim.join((kind, subkind))
        return kind

    def _advance(self):
        """Advances the lexer to the next character

        :return: None
        :raises RuntimeError: if character index is out of bounds
        """
        self._pos.advance(self._char)
        self._char = self._source[self._pos.index]
        if self._pos.index > len(self._source.code):
            raise RuntimeError()

    def _scan_string(self, fn: F) -> BaseToken:
        """Scans and returns a STRING type of token

        :param fn: A callable to create the STRING token
        :return: The token returned after `fn` gets called
        """
        quote: str = self._char
        conf = self._config
        start: SourceLn = self._pos.copy()
        end: SourceLn = start
        self._advance()

        while self._char is not None:
            if self._char == conf.string.escape.mark:
                self._advance()
                self._advance()
            elif self._char == quote:
                self._advance()
                end = self._pos.copy()
                break
            else:
                self._advance()

        return fn(source=self._source, pos=start, kind="STRING", value=slice(start.index, end.index))

    def _scan_number(self, fn: F) -> tuple[BaseToken | None, Error | None]:
        """Scans and returns a NUMBER type of token

        :param fn: A callable to create the NUMBER token
        :return: The token returned after `fn` gets called or an error
        """
        conf = self._config
        start = self._pos.copy()
        base = 10
        basename = "DECIMAL"
        digits: str = '0123456789'
        has_decimal: bool = False
        has_other_base: bool = False
        idx: int = 0
        number: str = ''
        suffix: str = ''
        subkind: str | None

        while self._char is not None:
            if idx == 0 and self._char == '0':
                idx += 1
                number += self._char
                self._advance()
                if self._char.lower() in conf.number.integer.base_chars.lower():
                    digits = conf.number.integer.base[self._char].digits
                    basename = conf.number.integer.base[self._char].name
                    base = len(digits)
                    has_other_base = True
                    idx += 1
                    number += self._char
                    self._advance()
                    continue

            if self._char.lower() in digits.lower():
                idx += 1
                number += self._char
                self._advance()

            elif self._char == conf.number.separator:
                idx += 1
                self._advance()

            elif self._char == '.':
                if has_other_base or has_decimal:
                    return None, UnexpectedCharError(self._char, pos=self._pos.copy())
                has_decimal = True
                basename = "FLOAT"
                idx += 1
                number += self._char
                self._advance()

            elif self._char.isalpha() and not has_other_base:
                if has_other_base:
                    return None, UnexpectedCharError("Not base 10", pos=self._pos.copy())
                idx += 1
                suffix += self._char
                self._advance()

            elif self._char in conf.whitespace.chars:
                break

            elif self._char in conf.delimiter.chars:
                break

            elif self._char in conf.operator.chars:
                break

            else:
                return None, UnexpectedCharError(self._char, pos=self._pos.copy())

        kind = self._kind_subkind("NUMBER", basename)
        end = self._pos.index
        return fn(source=self._source, number=number, suffix=suffix, kind=kind, base=base,
                  is_real=has_decimal, pos=start, value=slice(start.index, end)), None

    def _scan_operator(self, fn: F) -> tuple[BaseToken | None, Error | None]:
        """Scans and returns a OPERATOR type of token

        :param fn: A callable to create the OPERATOR token
        :return: The token returned after `fn` gets called or an error
        """
        conf = self._config
        start = self._pos.copy()
        operator: str = ''
        kind: str = "OPERATOR"
        subkind: str | None

        while self._char is not None:
            if self._char in conf.operator.chars:

                operator += self._char
                self._advance()

                if len(operator) > conf.operator.max_length:
                    return None, UnexpectedCharError(self._char, pos=self._pos.copy())

            else:
                break

        if operator == conf.comment.doc_line:
            end = self._scan_comment().index
            kind = "DOC"
            subkind = "BRIEF"

        elif operator == conf.comment.doc_block.begin:
            end = self._scan_comment(conf.comment.doc_block.end).index
            kind = "DOC"
            subkind = "LONG"

        elif operator == conf.comment.line:
            end = self._scan_comment().index
            kind = "COMMENT"
            subkind = "BRIEF"

        elif operator == conf.comment.block.begin:
            end = self._scan_comment(conf.comment.block.end).index
            kind = "COMMENT"
            subkind = "LONG"
        else:
            subkind = conf.tokens.get(operator, "OPERATOR")
            end = self._pos.index

        kind = self._kind_subkind(kind, subkind)

        token = fn(source=self._source, pos=start, kind=kind, value=slice(start.index, end))
        if kind == "COMMENT" or (kind == "DOC" and not conf.comment.include_doc):
            token = None
        return token, None

    def _scan_comment(self, end: str | None = None) -> SourceLn:
        """Scans a COMMENT type of token

        :param end: An optional ending marker to match for block comments or None for line comments
        :return: The position in the source code where the comment ends
        """
        comment: str = ''
        while self._char is not None:
            if self._char == '\n':
                break

            comment += self._char
            self._advance()

            if end is not None:
                if comment.endswith(end):
                    break

        return self._pos.copy()

    def _scan_word(self, fn: F) -> tuple[BaseToken | None, Error | None]:
        """Scans and returns a WORD type of token

        :param fn: A callable to create the WORD token
        :return: The token returned after `fn` gets called or an error
        """
        conf = self._config
        start = self._pos.copy()
        word: str = ''
        subkind: str | None = None

        while self._char is not None:
            if not self._char.isalnum() or self._char == '_':
                break
            word += self._char
            self._advance()

            if len(word) > conf.identifier.max_length:
                return None, UnexpectedCharError("Too long", pos=self._pos.copy())

        kind = "WORD"
        if word in conf.keyword.reserved:
            subkind = "RESERVED"
        else:
            for key_kind in conf.keyword.kind:
                if key_kind in conf.keyword and word in conf.keyword[key_kind]:
                    subkind = key_kind
        kind = self._kind_subkind(kind, subkind)

        return fn(source=self._source, pos=start, kind=kind.upper(), value=slice(start.index, self._pos.index)), None

    def gen_tokens(self, factory: F) -> tuple[list[BaseToken], Optional[Error]]:
        """Scans the source code and returns a list of tokens

        :param factory: A callable to create the tokens
        :return: The list of tokens or an error
        """
        tokens: list[BaseToken] = []
        token: BaseToken
        source = self._source
        conf = self._config

        while self._char is not None:
            if self._char in conf.whitespace.chars:
                start = self._pos.copy()
                while self._char is not None and self._char in conf.whitespace.chars:
                    self._advance()
                if conf.whitespace.include:
                    tokens.append(factory(source=self._source, pos=start, kind="WS",
                                          value=slice(start.index, self._pos.index)))
                continue

            if self._char == conf.string.quote.single:
                tokens.append(self._scan_string(factory))

            elif self._char == conf.string.quote.double:
                tokens.append(self._scan_string(factory))

            elif self._char in "0123456789":
                token, error = self._scan_number(factory)
                if error:
                    return [], error
                tokens.append(token)

            elif self._char in conf.delimiter.chars:
                kind = self._kind_subkind("DELIM", conf.tokens.get(self._char))
                tokens.append(factory(source=self._source, kind=kind, pos=self._pos.copy(),
                                      value=slice(self._pos.index, self._pos.index + 1)))
                self._advance()

            elif self._char in conf.operator.chars:
                token, error = self._scan_operator(factory)
                if error:
                    return [], error
                if token:
                    tokens.append(token)

            elif self._char.isalpha():
                token, error = self._scan_word(factory)
                if error:
                    return [], error
                tokens.append(token)

            else:
                error = UnexpectedCharError(self._char, pos=self._pos.copy())
                return [], error

        tokens.append(factory(source=source, pos=self._pos.copy(), kind="EOF", value=""))
        return tokens, None


class TokenStream(Generic[T]):

    def __init__(self, tokens: list[BaseToken], config: JSOM, **handlers):
        self._config: JSOM = config
        self._tokens: list[BaseToken | T] = tokens
        self._index: int = -1
        self._expect_error: Callable[[str, SourceLn, str, str], None] | None = handlers.get('expect_error_callback')

    @property
    def config(self) -> JSOM:
        return self._config

    @property
    def source(self) -> Source:
        return self._tokens[0].source

    @property
    def filename(self) -> str:
        return self.source.filename

    @property
    def token(self) -> BaseToken | T:
        if not self.is_eof:
            return self._tokens[self._index]
        return self._tokens[-1]

    @property
    def is_eof(self) -> bool:
        return self._index >= len(self._tokens)

    def next(self) -> bool:
        if not self.is_eof:
            self._index += 1
            return True
        return False

    def begin(self):
        if self._index < 0:
            self.next()

    @staticmethod
    def _join(*values: str) -> str:
        n = len(values)
        if n == 0:
            return 'no value'
        elif n == 1:
            return values[0]
        elif n == 2:
            return f"{values[0]} or {values[1]}"
        else:
            a = ', '.join(values[:-1])
            b = values[-1]
            return f"{a} or {b}"

    @staticmethod
    def _locate(token: BaseToken| T) -> str:
        src = token.source.filename
        ln = token.ln
        return f"{src}, {ln}"

    # region EXPECT_*

    def expect_token(self, value: str) -> None:
        if self.token.value == value:
            self.next()
        elif self._expect_error:
            self._expect_error(self._locate(self.token), self.token.ln, self.token.value, value)

    def expect_kind(self, kind: str) -> None:
        if self.token.kind.startswith(kind):
            self.next()
        elif self._expect_error:
            self._expect_error(self._locate(self.token), self.token.ln, self.token.kind, kind)

    def expect_subkind(self, subkind: str) -> None:
        if self.token.kind.endswith(subkind):
            self.next()
        elif self._expect_error:
            self._expect_error(self._locate(self.token), self.token.ln, self.token.kind, subkind)

    def expect_any_token(self, *values: str) -> None:
        if self.token.value in values:
            self.next()
        elif self._expect_error:
            self._expect_error(self._locate(self.token), self.token.ln, self.token.value, self._join(*values))

    def expect_any_kind(self, *kinds: str) -> None:
        for kind in kinds:
            if self.token.kind.startswith(kind):
                self.next()
                return
        if self._expect_error:
            self._expect_error(self._locate(self.token), self.token.ln, self.token.kind, self._join(*kinds))

    def expect_any_subkind(self, *subkinds: str) -> None:
        for subkind in subkinds:
            if self.token.kind.startswith(subkind):
                self.next()
                return
        if self._expect_error:
            self._expect_error(self._locate(self.token), self.token.ln, self.token.kind, self._join(*subkinds))

    # endregion (match_*)

    # region MATCH_*

    def match_token(self, value: str) -> bool:
        if self.token.value == value:
            self.next()
            return True
        return False

    def match_kind(self, kind: str) -> bool:
        if self.token.kind.startswith(kind):
            self.next()
            return True
        return False

    def match_subkind(self, subkind: str) -> bool:
        if self.token.kind.endswith(subkind):
            self.next()
            return True
        return False

    def match_any_token(self, *values: str) -> bool:
        if self.token.value in values:
            self.next()
            return True
        return False

    def match_any_kind(self, *kinds: str) -> bool:
        for kind in kinds:
            if self.token.kind.startswith(kind):
                self.next()
                return True
        return False

    def match_any_subkind(self, *subkinds: str) -> bool:
        for subkind in subkinds:
            if self.token.kind.endswith(subkind):
                self.next()
                return True
        return False

    # endregion (match_*)

    # region IS_*

    def is_token(self, value: str) -> bool:
        return self.token.value == value

    def is_kind(self, kind: str) -> bool:
        return self.token.kind.startswith(kind)

    def is_subkind(self, subkind: str) -> bool:
        return self.token.kind.endswith(subkind)

    def is_any_token(self, *values: str) -> bool:
        return self.token.value in values

    def is_any_kind(self, *kinds: str) -> bool:
        for kind in kinds:
            if self.token.kind.startswith(kind):
                return True
        return False

    def is_any_subkind(self, *subkinds: str) -> bool:
        for subkind in subkinds:
            if self.token.kind.endswith(subkind):
                return True
        return False

    # endregion

# endregion (classes)
# ---------------------------------------------------------
