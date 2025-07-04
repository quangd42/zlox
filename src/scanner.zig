const std = @import("std");

pub const Scanner = @This();

source: []const u8,
start: usize,
current: usize,
line: usize,

pub fn init(source: []const u8) Scanner {
    return .{
        .source = source,
        .start = 0,
        .current = 0,
        .line = 1,
    };
}

pub fn scanToken(s: *Scanner) Token {
    s.consumeWhitespace();
    s.start = s.current;
    if (s.isAtEnd()) return s.makeToken(.EOF);

    const char = s.source[s.current];
    s.current += 1;

    if (isAlpha(char)) return s.makeIdentToken();
    if (isDigit(char)) return s.makeNumberToken();

    return switch (char) {
        // Single-character tokens.
        '(' => s.makeToken(.LEFT_PAREN),
        ')' => s.makeToken(.RIGHT_PAREN),
        '{' => s.makeToken(.LEFT_BRACE),
        '}' => s.makeToken(.RIGHT_BRACE),
        ',' => s.makeToken(.COMMA),
        '.' => s.makeToken(.DOT),
        '-' => s.makeToken(.MINUS),
        '+' => s.makeToken(.PLUS),
        ';' => s.makeToken(.SEMICOLON),
        '/' => s.makeToken(.SLASH),
        '*' => s.makeToken(.STAR),
        // One or two character tokens.
        '!' => s.makeToken(if (!s.match('=')) .BANG else .BANG_EQUAL),
        '=' => s.makeToken(if (!s.match('=')) .EQUAL else .EQUAL_EQUAL),
        '>' => s.makeToken(if (!s.match('=')) .GREATER else .GREATER_EQUAL),
        '<' => s.makeToken(if (!s.match('=')) .LESS else .LESS_EQUAL),

        '"' => s.makeStringToken(),
        else => s.errorToken("Unexpected character."),
    };
}

fn peek(s: *Scanner) u8 {
    return s.source[s.current];
}

fn peekNext(s: *Scanner) u8 {
    if (s.current + 1 >= s.source.len) return '0' - 1;
    return s.source[s.current + 1];
}

fn match(s: *Scanner, expected: u8) bool {
    if (s.isAtEnd()) return false;
    if (s.source[s.current] != expected) return false;
    s.current += 1;
    return true;
}

fn isAtEnd(s: *Scanner) bool {
    return s.current >= s.source.len;
}

fn consumeWhitespace(s: *Scanner) void {
    while (!s.isAtEnd()) {
        const char = s.source[s.current];
        switch (char) {
            ' ', '\t', '\r' => s.current += 1,
            '\n' => {
                s.line += 1;
                s.current += 1;
            },
            '/' => {
                if (s.peekNext() == '/') {
                    while (!s.isAtEnd()) : (s.current += 1) {
                        if (s.source[s.current] == '\n') break;
                    }
                } else return;
            },
            else => return,
        }
    }
}

fn makeIdentToken(s: *Scanner) Token {
    while (!s.isAtEnd()) {
        const c = s.source[s.current];
        if (isAlpha(c) or isDigit(c)) s.current += 1 else break;
    }

    const lexeme = s.source[s.start..s.current];
    const tokType = Keywords.get(lexeme) orelse .IDENTIFIER;
    return s.makeToken(tokType);
}

fn makeNumberToken(s: *Scanner) Token {
    while (!s.isAtEnd()) {
        if (isDigit(s.peek())) {
            s.current += 1;
            continue;
        }
        if (s.peek() == '.' and isDigit(s.peekNext())) {
            s.current += 1;
            while (!s.isAtEnd() and isDigit(s.peek())) {
                s.current += 1;
            }
        }
        break;
    }
    return s.makeToken(.NUMBER);
}

fn makeStringToken(s: *Scanner) Token {
    while (true) : (s.current += 1) {
        if (s.isAtEnd()) return s.errorToken("Unterminated string.");
        if (s.peek() == '\n') s.line += 1;
        if (s.peek() == '"') break;
    }
    s.current += 1; // consume closing '"'
    return s.makeToken(.STRING);
}

fn makeToken(s: *Scanner, tokType: TokenType) Token {
    return .{
        .type = tokType,
        .lexeme = s.source[s.start..s.current],
        .line = s.line,
    };
}

fn errorToken(s: *Scanner, msg: []const u8) Token {
    return .{
        .type = .ERROR,
        .lexeme = msg,
        .line = s.line,
    };
}

pub const Token = struct {
    type: TokenType,
    lexeme: []const u8,
    line: usize,
};

pub const TokenType = enum {
    // zig fmt: off
    // Single-character tokens.
    LEFT_PAREN, RIGHT_PAREN,
    LEFT_BRACE, RIGHT_BRACE,
    COMMA, DOT, MINUS, PLUS,
    SEMICOLON, SLASH, STAR,
    // One or two character tokens.
    BANG, BANG_EQUAL,
    EQUAL, EQUAL_EQUAL,
    GREATER, GREATER_EQUAL,
    LESS, LESS_EQUAL,
    // Literals.
    IDENTIFIER, STRING, NUMBER,
    // Keywords.
    AND, CLASS, ELSE, FALSE,
    FOR, FUN, IF, NIL, OR,
    PRINT, RETURN, SUPER, THIS,
    TRUE, VAR, WHILE,

    ERROR, EOF,
    // zig fmt: on
};

const TokenStringMap = std.StaticStringMap(TokenType);
const Keywords = TokenStringMap.initComptime(.{
    .{ "and", .AND },
    .{ "class", .CLASS },
    .{ "else", .ELSE },
    .{ "false", .FALSE },
    .{ "for", .FOR },
    .{ "fun", .FUN },
    .{ "if", .IF },
    .{ "nil", .NIL },
    .{ "or", .OR },
    .{ "print", .PRINT },
    .{ "return", .RETURN },
    .{ "super", .SUPER },
    .{ "this", .THIS },
    .{ "true", .TRUE },
    .{ "var", .VAR },
    .{ "while", .WHILE },
});

fn isAlpha(c: u8) bool {
    return (c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z') or c == '_';
}

fn isDigit(c: u8) bool {
    return c >= '0' and c <= '9';
}
