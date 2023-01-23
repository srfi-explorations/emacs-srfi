;;; srfi-data.el --- Scheme Requests for Implementation database -*- lexical-binding: t -*-
;;
;; Copyright 1998-2020 srfi.schemers.org
;; SPDX-License-Identifier: MIT
;;
;;; Commentary:
;;
;; Automatically converted from srfi-data.scm.
;;
;;; Code:

(defconst srfi-data
  [
   ;; SRFI 0:
   1999 final "Feature-based conditional expansion construct"
   ;; SRFI 1:
   1999 final "List Library"
   ;; SRFI 2:
   1999 final "AND-LET*: an AND with local bindings, a guarded LET* special form"
   ;; SRFI 3:
   1999 withdrawn "List-Set Library"
   ;; SRFI 4:
   1999 final "Homogeneous numeric vector datatypes"
   ;; SRFI 5:
   1999 final "A compatible let form with signatures and rest arguments"
   ;; SRFI 6:
   1999 final "Basic String Ports"
   ;; SRFI 7:
   1999 final "Feature-based program configuration language"
   ;; SRFI 8:
   1999 final "receive: Binding to multiple values"
   ;; SRFI 9:
   1999 final "Defining Record Types"
   ;; SRFI 10:
   2000 final "#, external form"
   ;; SRFI 11:
   2000 final "Syntax for receiving multiple values"
   ;; SRFI 12:
   2000 withdrawn "Exception Handling"
   ;; SRFI 13:
   2000 final "String Libraries"
   ;; SRFI 14:
   2000 final "Character-set Library"
   ;; SRFI 15:
   2000 withdrawn "Syntax for dynamic scoping"
   ;; SRFI 16:
   2000 final "Syntax for procedures of variable arity"
   ;; SRFI 17:
   2000 final "Generalized set!"
   ;; SRFI 18:
   2001 final "Multithreading support"
   ;; SRFI 19:
   2000 final "Time Data Types and Procedures"
   ;; SRFI 20:
   2001 withdrawn "Simple object system"
   ;; SRFI 21:
   2001 final "Real-time multithreading support"
   ;; SRFI 22:
   2002 final "Running Scheme Scripts on Unix"
   ;; SRFI 23:
   2001 final "Error reporting mechanism"
   ;; SRFI 24:
   2002 withdrawn "Define-syntax in local lexical scopes"
   ;; SRFI 25:
   2002 final "Multi-dimensional Array Primitives"
   ;; SRFI 26:
   2002 final "Notation for Specializing Parameters without Currying"
   ;; SRFI 27:
   2002 final "Sources of Random Bits"
   ;; SRFI 28:
   2002 final "Basic Format Strings"
   ;; SRFI 29:
   2002 final "Localization"
   ;; SRFI 30:
   2002 final "Nested Multi-line Comments"
   ;; SRFI 31:
   2002 final "A special form `rec' for recursive evaluation"
   ;; SRFI 32:
   2003 withdrawn "Sort Libraries"
   ;; SRFI 33:
   2003 withdrawn "Integer Bitwise-operation Library"
   ;; SRFI 34:
   2002 final "Exception Handling for Programs"
   ;; SRFI 35:
   2002 final "Conditions"
   ;; SRFI 36:
   2002 final "I/O Conditions"
   ;; SRFI 37:
   2003 final "args-fold: a program argument processor"
   ;; SRFI 38:
   2003 final "External Representation for Data With Shared Structure"
   ;; SRFI 39:
   2003 final "Parameter objects"
   ;; SRFI 40:
   2017 withdrawn "A Library of Streams"
   ;; SRFI 41:
   2008 final "Streams"
   ;; SRFI 42:
   2003 final "Eager Comprehensions"
   ;; SRFI 43:
   2004 final "Vector library"
   ;; SRFI 44:
   2004 final "Collections"
   ;; SRFI 45:
   2004 final "Primitives for Expressing Iterative Lazy Algorithms"
   ;; SRFI 46:
   2005 final "Basic Syntax-rules Extensions"
   ;; SRFI 47:
   2004 final "Array"
   ;; SRFI 48:
   2004 final "Intermediate Format Strings"
   ;; SRFI 49:
   2005 final "Indentation-sensitive syntax"
   ;; SRFI 50:
   2005 withdrawn "Mixing Scheme and C"
   ;; SRFI 51:
   2004 final "Handling rest list"
   ;; SRFI 52:
   2004 withdrawn "Permitting and Supporting Extended Character Sets"
   ;; SRFI 53:
   2004 withdrawn "Syntactic computations with computation-rules"
   ;; SRFI 54:
   2004 final "Formatting"
   ;; SRFI 55:
   2004 final "require-extension"
   ;; SRFI 56:
   2005 withdrawn "Binary I/O"
   ;; SRFI 57:
   2005 final "Records"
   ;; SRFI 58:
   2005 final "Array Notation"
   ;; SRFI 59:
   2005 final "Vicinity"
   ;; SRFI 60:
   2005 final "Integers as Bits"
   ;; SRFI 61:
   2005 final "A more general cond clause"
   ;; SRFI 62:
   2005 final "S-expression comments"
   ;; SRFI 63:
   2005 final "Homogeneous and Heterogeneous Arrays"
   ;; SRFI 64:
   2006 final "A Scheme API for test suites"
   ;; SRFI 65:
   2005 withdrawn "define-immutable: A Syntax to Define Identifiers With Immutable Values"
   ;; SRFI 66:
   2005 final "Octet Vectors"
   ;; SRFI 67:
   2005 final "Compare Procedures"
   ;; SRFI 68:
   2005 withdrawn "Comprehensive I/O"
   ;; SRFI 69:
   2005 final "Basic hash tables"
   ;; SRFI 70:
   2005 final "Numbers"
   ;; SRFI 71:
   2005 final "Extended LET-syntax for multiple values"
   ;; SRFI 72:
   2005 final "Hygienic macros"
   ;; SRFI 73:
   2005 withdrawn "Exact Infinities"
   ;; SRFI 74:
   2005 final "Octet-Addressed Binary Blocks"
   ;; SRFI 75:
   2006 withdrawn "R6RS Unicode data"
   ;; SRFI 76:
   2006 withdrawn "R6RS Records"
   ;; SRFI 77:
   2006 withdrawn "Preliminary Proposal for R6RS Arithmetic"
   ;; SRFI 78:
   2006 final "Lightweight testing"
   ;; SRFI 79:
   2006 withdrawn "Primitive I/O"
   ;; SRFI 80:
   2006 withdrawn "Stream I/O"
   ;; SRFI 81:
   2006 withdrawn "Port I/O"
   ;; SRFI 82:
   2006 withdrawn "Stream Ports"
   ;; SRFI 83:
   2006 withdrawn "R6RS Library Syntax"
   ;; SRFI 84:
   2006 withdrawn "Universal Identifiers"
   ;; SRFI 85:
   2006 withdrawn "Recursive Equivalence Predicates"
   ;; SRFI 86:
   2006 final "MU and NU simulating VALUES & CALL-WITH-VALUES, and their related LET-syntax"
   ;; SRFI 87:
   2006 final "=> in case clauses"
   ;; SRFI 88:
   2007 final "Keyword objects"
   ;; SRFI 89:
   2007 final "Optional positional and named parameters"
   ;; SRFI 90:
   2007 final "Extensible hash table constructor"
   ;; SRFI 91:
   2007 withdrawn "Extended ports"
   ;; SRFI 92:
   2007 withdrawn "ALAMBDA and ALAMBDA*"
   ;; SRFI 93:
   2006 withdrawn "R6RS Syntax-Case Macros"
   ;; SRFI 94:
   2007 final "Type-Restricted Numerical Functions"
   ;; SRFI 95:
   2007 final "Sorting and Merging"
   ;; SRFI 96:
   2008 final "SLIB Prerequisites"
   ;; SRFI 97:
   2008 final "SRFI Libraries"
   ;; SRFI 98:
   2008 final "An interface to access environment variables"
   ;; SRFI 99:
   2009 final "ERR5RS Records"
   ;; SRFI 100:
   2010 final "define-lambda-object"
   ;; SRFI 101:
   2013 final "Purely Functional Random-Access Pairs and Lists"
   ;; SRFI 102:
   2013 withdrawn "Procedure Arity Inspection"
   ;; SRFI 103:
   2013 withdrawn "Library Files"
   ;; SRFI 104:
   2010 withdrawn "Library Files Utilities"
   ;; SRFI 105:
   2012 final "Curly-infix-expressions"
   ;; SRFI 106:
   2013 final "Basic socket interface"
   ;; SRFI 107:
   2013 final "XML reader syntax"
   ;; SRFI 108:
   2013 final "Named quasi-literal constructors"
   ;; SRFI 109:
   2013 final "Extended string quasi-literals"
   ;; SRFI 110:
   2013 final "Sweet-expressions (t-expressions)"
   ;; SRFI 111:
   2013 final "Boxes"
   ;; SRFI 112:
   2013 final "Environment Inquiry"
   ;; SRFI 113:
   2014 final "Sets and bags"
   ;; SRFI 114:
   2017 withdrawn "Comparators"
   ;; SRFI 115:
   2014 final "Scheme Regular Expressions"
   ;; SRFI 116:
   2014 final "Immutable List Library"
   ;; SRFI 117:
   2015 final "Queues based on lists"
   ;; SRFI 118:
   2015 final "Simple adjustable-size strings"
   ;; SRFI 119:
   2015 final "wisp: simpler indentation-sensitive scheme"
   ;; SRFI 120:
   2015 final "Timer APIs"
   ;; SRFI 121:
   2019 withdrawn "Generators"
   ;; SRFI 122:
   2022 withdrawn "Nonempty Intervals and Generalized Arrays"
   ;; SRFI 123:
   2015 final "Generic accessor and modifier operators"
   ;; SRFI 124:
   2015 final "Ephemerons"
   ;; SRFI 125:
   2015 final "Intermediate hash tables"
   ;; SRFI 126:
   2016 final "R6RS-based hashtables"
   ;; SRFI 127:
   2016 final "Lazy Sequences"
   ;; SRFI 128:
   2016 final "Comparators (reduced)"
   ;; SRFI 129:
   2016 final "Titlecase procedures"
   ;; SRFI 130:
   2016 final "Cursor-based string library"
   ;; SRFI 131:
   2016 final "ERR5RS Record Syntax (reduced)"
   ;; SRFI 132:
   2016 final "Sort Libraries"
   ;; SRFI 133:
   2016 final "Vector Library (R7RS-compatible)"
   ;; SRFI 134:
   2016 final "Immutable Deques"
   ;; SRFI 135:
   2016 final "Immutable Texts"
   ;; SRFI 136:
   2016 final "Extensible record types"
   ;; SRFI 137:
   2016 final "Minimal Unique Types"
   ;; SRFI 138:
   2016 final "Compiling Scheme programs to executables"
   ;; SRFI 139:
   2016 final "Syntax parameters"
   ;; SRFI 140:
   2017 final "Immutable Strings"
   ;; SRFI 141:
   2016 final "Integer division"
   ;; SRFI 142:
   2017 withdrawn "Bitwise Operations"
   ;; SRFI 143:
   2017 final "Fixnums"
   ;; SRFI 144:
   2017 final "Flonums"
   ;; SRFI 145:
   2017 final "Assumptions"
   ;; SRFI 146:
   2018 final "Mappings"
   ;; SRFI 147:
   2017 final "Custom macro transformers"
   ;; SRFI 148:
   2017 final "Eager syntax-rules"
   ;; SRFI 149:
   2017 final "Basic Syntax-rules Template Extensions"
   ;; SRFI 150:
   2018 final "Hygienic ERR5RS Record Syntax (reduced)"
   ;; SRFI 151:
   2017 final "Bitwise Operations"
   ;; SRFI 152:
   2017 final "String Library (reduced)"
   ;; SRFI 153:
   2018 withdrawn "Ordered Sets"
   ;; SRFI 154:
   2018 final "First-class dynamic extents"
   ;; SRFI 155:
   2018 final "Promises"
   ;; SRFI 156:
   2017 final "Syntactic combiners for binary predicates"
   ;; SRFI 157:
   2018 final "Continuation marks"
   ;; SRFI 158:
   2017 final "Generators and Accumulators"
   ;; SRFI 159:
   2020 withdrawn "Combinator Formatting"
   ;; SRFI 160:
   2019 final "Homogeneous numeric vector libraries"
   ;; SRFI 161:
   2019 final "Unifiable Boxes"
   ;; SRFI 162:
   2019 final "Comparators sublibrary"
   ;; SRFI 163:
   2019 final "Enhanced array literals"
   ;; SRFI 164:
   2019 final "Enhanced multi-dimensional Arrays"
   ;; SRFI 165:
   2019 final "The Environment Monad"
   ;; SRFI 166:
   2020 final "Monadic Formatting"
   ;; SRFI 167:
   2019 final "Ordered Key Value Store"
   ;; SRFI 168:
   2019 final "Generic Tuple Store Database"
   ;; SRFI 169:
   2019 final "Underscores in numbers"
   ;; SRFI 170:
   2020 final "POSIX API"
   ;; SRFI 171:
   2019 final "Transducers"
   ;; SRFI 172:
   2019 final "Two Safer Subsets of R7RS"
   ;; SRFI 173:
   2019 final "Hooks"
   ;; SRFI 174:
   2019 final "POSIX Timespecs"
   ;; SRFI 175:
   2019 final "ASCII character library"
   ;; SRFI 176:
   2020 final "Version flag"
   ;; SRFI 177:
   2020 withdrawn "Portable keyword arguments"
   ;; SRFI 178:
   2020 final "Bitvector library"
   ;; SRFI 179:
   2020 final "Nonempty Intervals and Generalized Arrays (Updated)"
   ;; SRFI 180:
   2020 final "JSON"
   ;; SRFI 181:
   2020 final "Custom ports (including transcoded ports)"
   ;; SRFI 182:
   2020 withdrawn "ADBMAL, ALET, and ALET*"
   ;; SRFI 183:
   2020 withdrawn "Another format procedure, Fox"
   ;; SRFI 184:
   2020 withdrawn "define-record-lambda"
   ;; SRFI 185:
   2020 final "Linear adjustable-length strings"
   ;; SRFI 186:
   2020 withdrawn "Transcoders and transcoded ports"
   ;; SRFI 187:
   2020 withdrawn "ALAMBDA and ADEFINE"
   ;; SRFI 188:
   2020 final "Splicing binding constructs for syntactic keywords"
   ;; SRFI 189:
   2020 final "Maybe and Either: optional container types"
   ;; SRFI 190:
   2020 final "Coroutine Generators"
   ;; SRFI 191:
   2020 withdrawn "Procedure Arity Inspection"
   ;; SRFI 192:
   2020 final "Port Positioning"
   ;; SRFI 193:
   2020 final "Command line"
   ;; SRFI 194:
   2020 final "Random data generators"
   ;; SRFI 195:
   2020 final "Multiple-value boxes"
   ;; SRFI 196:
   2020 final "Range Objects"
   ;; SRFI 197:
   2020 final "Pipeline Operators"
   ;; SRFI 198:
   2020 withdrawn "Foreign Interface Status"
   ;; SRFI 199:
   2020 withdrawn "POSIX errno manipulation"
   ;; SRFI 200:
   2022 withdrawn "Pattern Matching"
   ;; SRFI 201:
   2021 final "Syntactic Extensions to the Core Scheme Bindings"
   ;; SRFI 202:
   2020 final "Pattern-matching Variant of the and-let* Form that Supports Multiple Values"
   ;; SRFI 203:
   2020 final "A Simple Picture Language in the Style of SICP"
   ;; SRFI 204:
   2022 withdrawn "Wright-Cartwright-Shinn Pattern Matcher"
   ;; SRFI 205:
   2022 withdrawn "POSIX Terminal Fundamentals"
   ;; SRFI 206:
   2020 final "Auxiliary Syntax Keywords"
   ;; SRFI 207:
   2020 final "String-notated bytevectors"
   ;; SRFI 208:
   2021 final "NaN procedures"
   ;; SRFI 209:
   2020 final "Enums and Enum Sets"
   ;; SRFI 210:
   2021 final "Procedures and Syntax for Multiple Values"
   ;; SRFI 211:
   2022 final "Scheme Macro Libraries"
   ;; SRFI 212:
   2021 final "Aliases"
   ;; SRFI 213:
   2021 final "Identifier Properties"
   ;; SRFI 214:
   2021 final "Flexvectors"
   ;; SRFI 215:
   2021 final "Central Log Exchange"
   ;; SRFI 216:
   2020 final "SICP Prerequisites (Portable)"
   ;; SRFI 217:
   2021 final "Integer Sets"
   ;; SRFI 218:
   2021 withdrawn "Unicode Numerals"
   ;; SRFI 219:
   2021 final "Define higher-order lambda"
   ;; SRFI 220:
   2021 withdrawn "Line directives"
   ;; SRFI 221:
   2021 final "Generator/accumulator sub-library"
   ;; SRFI 222:
   2021 final "Compound Objects"
   ;; SRFI 223:
   2021 final "Generalized binary search procedures"
   ;; SRFI 224:
   2021 final "Integer Mappings"
   ;; SRFI 225:
   2022 final "Dictionaries"
   ;; SRFI 226:
   nil draft "Control Features"
   ;; SRFI 227:
   2021 final "Optional Arguments"
   ;; SRFI 228:
   2022 final "Composing Comparators"
   ;; SRFI 229:
   2021 final "Tagged Procedures"
   ;; SRFI 230:
   2021 final "Atomic Operations"
   ;; SRFI 231:
   2022 final "Intervals and Generalized Arrays"
   ;; SRFI 232:
   2022 final "Flexible curried procedures"
   ;; SRFI 233:
   2022 final "INI files"
   ;; SRFI 234:
   nil draft "Topological Sorting"
   ;; SRFI 235:
   nil draft "Combinators"
   ;; SRFI 236:
   2022 final "Evaluating expressions in an unspecified order"
   ;; SRFI 237:
   nil draft "R6RS Records (refined)"
   ;; SRFI 238:
   2023 final "Codesets"
   ;; SRFI 239:
   2023 final "Destructuring Lists"
   ;; SRFI 240:
   nil draft "Reconciled Records"
   ;; SRFI 241:
   nil draft "Match — Simple Pattern-Matching Syntax to Express Catamorphisms on Scheme Data"
   ;; SRFI 242:
   nil draft "The CFG Language"
   ;; SRFI 243:
   nil draft "Unreadable Objects"
   ;; SRFI 244:
   nil draft "Multiple-value Definitions"
   ]
  "Table of all known SRFI documents.")

(defconst srfi-data-keywords
  '(
    ("Algorithm"
     32 95 132 234)
    ("Assignment"
     17)
    ("Binding"
     2 5 8 11 15 39 61 65 71 89 139 177 201 202 210 213
     219 227 244)
    ("Comparison"
     67 85 114 128 162 228)
    ("Concurrency"
     18 21 230)
    ("Continuations"
     157 226)
    ("Control Flow"
     2 12 23 42 61 87 202 226 236 242)
    ("Data Structure"
     1 3 4 9 13 14 19 25 33 40 41 43 44 45 47 52
     57 60 63 66 69 70 74 76 84 86 90 99 100 101 111 113
     115 116 117 118 121 122 124 125 126 127 130 131 133 134 135 136
     137 140 142 146 150 151 152 153 155 158 160 161 164 171 174 175
     178 179 182 184 185 189 195 196 208 209 214 217 221 222 224 225
     229 231 237 240)
    ("Error Handling"
     23 198 199 238)
    ("Exceptions"
     12 34 35 36)
    ("Features"
     0 7 55 96)
    ("I/O"
     6 28 29 36 38 48 54 56 59 68 79 80 81 82 91 106
     159 166 167 168 180 181 183 186 192 233)
    ("Internationalization"
     75 129 218)
    ("Introspection"
     102 191 238)
    ("Lazy Evaluation"
     65 155)
    ("Miscellaneous"
     20 31 92 120 123 154 165 172 173 187 223 235)
    ("Modules"
     83 97)
    ("Multiple-Value Returns"
     8 11 51 71 86 182 189 195 210 244)
    ("Numbers"
     4 60 63 70 73 77 94 122 141 143 144 151 169 179 208 231)
    ("Operating System"
     6 22 37 50 98 103 104 112 138 170 176 193 198 199 205 215
     238)
    ("Optimization"
     145)
    ("Parameters"
     39 139)
    ("Pattern Matching"
     16 200 201 202 204 241)
    ("R6RS process"
     75 76 77 83 93)
    ("R7RS Large"
     1 14 41 101 111 113 115 116 117 124 125 127 132 133 134 135
     141 143 144 146 151 158 159 160)
    ("R7RS Large: Red Edition"
     1 14 41 101 111 113 116 117 121 124 125 127 132 133 134 135)
    ("R7RS Large: Tangerine Edition"
     115 141 143 144 146 151 158 159 160)
    ("Randomness"
     27 194)
    ("Reader Syntax"
     10 30 49 58 62 88 105 107 108 109 110 119 163 169 207 220
     243)
    ("SICP"
     203 216)
    ("Superseded"
     40 114 121 122 142 159 186)
    ("Syntax"
     24 26 46 53 72 93 147 148 149 156 188 190 197 201 206 211
     212 213 219 232 236 239 241)
    ("Testing"
     64 78)
    ("Type Checking"
     92 187)
    )
  "Table of SRFI numbers by keyword.")

(provide 'srfi-data)

;;; srfi-data.el ends here
