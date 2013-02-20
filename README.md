W.I.P Blink schema parsing and C++ runtime
==========================================

See [Blink](http://blinkprotocol.org/)

Eventually, a code generator for the various serialization formats could be
produced.

# Schema EBNF

    schema      ::= nsDecl? def*
    nsDecl      ::= "namespace" name
    def         ::= define | groupDef
    define      ::= name "=" (enum | type)
    groupDef    ::= name ("/" id)? (":" qName)? ("->" fields)?
    fields      ::= field ("," field)+
    field       ::= type name "?"?
    type        ::= single | sequence
    single      ::= ref | time | number | string | "bool" | "object"
    sequence    ::= single "[" "]"
    string      ::= "string" ("(" [^)\n] ")"?
    ref         ::= qName | qName "*"
    number      ::= "i8" | "u8" | "i16" | "u16" | "i32" | "u32" | "i64" | "u64" |
                    "f64" | "decimal"
    time        ::= "date" | "timeOfDay" | "nanotime" | "millitime"
    enum        ::= "|" sym | sym ("|" sym)+
    sym         ::= name ("/" val)?
    val         ::= int | hexNum
    id          ::= uInt | hexNum
    qName       ::= name | cName
    name        ::= (ncName - keyword) | "\" ncName
    keyword     ::= "i8" | "u8" | "i16" | "u16" | "i32" |
                    "u32" | "i64" | "u64" | "f64" |
                    "decimal" | "date" | "timeOfDay" |
                    "nanotime" | "millitime" | "bool" |
                    "string" | "object" | "namespace" |
                    "type" | "schema"
    cname       ::= ncName ":" ncName
    ncName      ::= [_a-zA-Z] [_a-zA-Z0-9]*
    hexNum      ::= "0x" [0-9a-fA-F]+
    int         ::= "-"? uInt
    uInt        ::= [0-9]+

