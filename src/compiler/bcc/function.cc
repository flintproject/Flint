/* C++ code produced by gperf version 3.0.4 */
/* Command-line: gperf function.txt  */
/* Computed positions: -k'1,3-4,6,$' */

#if !((' ' == 32) && ('!' == 33) && ('"' == 34) && ('#' == 35) \
      && ('%' == 37) && ('&' == 38) && ('\'' == 39) && ('(' == 40) \
      && (')' == 41) && ('*' == 42) && ('+' == 43) && (',' == 44) \
      && ('-' == 45) && ('.' == 46) && ('/' == 47) && ('0' == 48) \
      && ('1' == 49) && ('2' == 50) && ('3' == 51) && ('4' == 52) \
      && ('5' == 53) && ('6' == 54) && ('7' == 55) && ('8' == 56) \
      && ('9' == 57) && (':' == 58) && (';' == 59) && ('<' == 60) \
      && ('=' == 61) && ('>' == 62) && ('?' == 63) && ('A' == 65) \
      && ('B' == 66) && ('C' == 67) && ('D' == 68) && ('E' == 69) \
      && ('F' == 70) && ('G' == 71) && ('H' == 72) && ('I' == 73) \
      && ('J' == 74) && ('K' == 75) && ('L' == 76) && ('M' == 77) \
      && ('N' == 78) && ('O' == 79) && ('P' == 80) && ('Q' == 81) \
      && ('R' == 82) && ('S' == 83) && ('T' == 84) && ('U' == 85) \
      && ('V' == 86) && ('W' == 87) && ('X' == 88) && ('Y' == 89) \
      && ('Z' == 90) && ('[' == 91) && ('\\' == 92) && (']' == 93) \
      && ('^' == 94) && ('_' == 95) && ('a' == 97) && ('b' == 98) \
      && ('c' == 99) && ('d' == 100) && ('e' == 101) && ('f' == 102) \
      && ('g' == 103) && ('h' == 104) && ('i' == 105) && ('j' == 106) \
      && ('k' == 107) && ('l' == 108) && ('m' == 109) && ('n' == 110) \
      && ('o' == 111) && ('p' == 112) && ('q' == 113) && ('r' == 114) \
      && ('s' == 115) && ('t' == 116) && ('u' == 117) && ('v' == 118) \
      && ('w' == 119) && ('x' == 120) && ('y' == 121) && ('z' == 122) \
      && ('{' == 123) && ('|' == 124) && ('}' == 125) && ('~' == 126))
/* The character set is not based on ISO-646.  */
#error "gperf generated tables don't work with this execution character set. Please report a bug to <bug-gnu-gperf@gnu.org>."
#endif

#line 8 "function.txt"
struct FunctionEntry { const char *name; Token::Type type; };
/* maximum key range = 201, duplicates = 0 */

class FunctionHash
{
private:
  static inline unsigned int hash (const char *str, unsigned int len);
public:
  static const struct FunctionEntry *in_word_set (const char *str, unsigned int len);
};

inline unsigned int
FunctionHash::hash (register const char *str, register unsigned int len)
{
  static const unsigned char asso_values[] =
    {
      203, 203, 203, 203, 203, 203, 203, 203, 203, 203,
      203, 203, 203, 203, 203, 203, 203, 203, 203, 203,
      203, 203, 203, 203, 203, 203, 203, 203, 203, 203,
      203, 203, 203, 203, 203, 203, 203, 203, 203, 203,
      203, 203, 203, 203, 203, 203, 203, 203,   0,   0,
       60,  55, 203, 203, 203, 203, 203, 203, 203, 203,
      203, 203, 203, 203, 203, 203, 203, 203, 203, 203,
      203, 203, 203, 203, 203, 203, 203, 203, 203, 203,
      203, 203, 203, 203, 203, 203, 203, 203, 203, 203,
      203, 203, 203, 203, 203, 203, 203,   0,  30,  10,
        0,   0,  25,  75,   0,  50,  35, 203,  45,  15,
       10,  30,  35,   0,  40,   0,   5,  15,  55,   5,
        5,   5, 203, 203, 203, 203, 203, 203, 203, 203,
      203, 203, 203, 203, 203, 203, 203, 203, 203, 203,
      203, 203, 203, 203, 203, 203, 203, 203, 203, 203,
      203, 203, 203, 203, 203, 203, 203, 203, 203, 203,
      203, 203, 203, 203, 203, 203, 203, 203, 203, 203,
      203, 203, 203, 203, 203, 203, 203, 203, 203, 203,
      203, 203, 203, 203, 203, 203, 203, 203, 203, 203,
      203, 203, 203, 203, 203, 203, 203, 203, 203, 203,
      203, 203, 203, 203, 203, 203, 203, 203, 203, 203,
      203, 203, 203, 203, 203, 203, 203, 203, 203, 203,
      203, 203, 203, 203, 203, 203, 203, 203, 203, 203,
      203, 203, 203, 203, 203, 203, 203, 203, 203, 203,
      203, 203, 203, 203, 203, 203, 203, 203, 203, 203,
      203, 203, 203, 203, 203, 203, 203
    };
  register int hval = len;

  switch (hval)
    {
      default:
        hval += asso_values[(unsigned char)str[5]+1];
      /*FALLTHROUGH*/
      case 5:
      case 4:
        hval += asso_values[(unsigned char)str[3]];
      /*FALLTHROUGH*/
      case 3:
        hval += asso_values[(unsigned char)str[2]+1];
      /*FALLTHROUGH*/
      case 2:
      case 1:
        hval += asso_values[(unsigned char)str[0]];
        break;
    }
  return hval + asso_values[(unsigned char)str[len - 1]];
}

const struct FunctionEntry *
FunctionHash::in_word_set (register const char *str, register unsigned int len)
{
  enum
    {
      TOTAL_KEYWORDS = 75,
      MIN_WORD_LENGTH = 2,
      MAX_WORD_LENGTH = 13,
      MIN_HASH_VALUE = 2,
      MAX_HASH_VALUE = 202
    };

  static const struct FunctionEntry wordlist[] =
    {
      {"",Token::Type::kUnspecified},
      {"",Token::Type::kUnspecified},
#line 35 "function.txt"
      {"eq",Token::Type::kEq},
      {"",Token::Type::kUnspecified},
#line 72 "function.txt"
      {"sech",Token::Type::kSech},
      {"",Token::Type::kUnspecified},
      {"",Token::Type::kUnspecified},
#line 19 "function.txt"
      {"arcsech",Token::Type::kArcsech},
#line 10 "function.txt"
      {"abs",Token::Type::kAbs},
#line 69 "function.txt"
      {"save",Token::Type::kSave},
      {"",Token::Type::kUnspecified},
      {"",Token::Type::kUnspecified},
      {"",Token::Type::kUnspecified},
#line 71 "function.txt"
      {"sec",Token::Type::kSec},
#line 31 "function.txt"
      {"csch",Token::Type::kCsch},
      {"",Token::Type::kUnspecified},
#line 18 "function.txt"
      {"arcsec",Token::Type::kArcsec},
#line 17 "function.txt"
      {"arccsch",Token::Type::kArccsch},
#line 26 "function.txt"
      {"cos",Token::Type::kCos},
#line 27 "function.txt"
      {"cosh",Token::Type::kCosh},
#line 81 "function.txt"
      {"times",Token::Type::kTimes},
#line 12 "function.txt"
      {"arccos",Token::Type::kArccos},
#line 13 "function.txt"
      {"arccosh",Token::Type::kArccosh},
#line 30 "function.txt"
      {"csc",Token::Type::kCsc},
#line 58 "function.txt"
      {"move",Token::Type::kMove},
#line 36 "function.txt"
      {"eulergamma",Token::Type::kEulergamma},
#line 16 "function.txt"
      {"arccsc",Token::Type::kArccsc},
      {"",Token::Type::kUnspecified},
#line 54 "function.txt"
      {"max",Token::Type::kMax},
#line 29 "function.txt"
      {"coth",Token::Type::kCoth},
#line 32 "function.txt"
      {"deref",Token::Type::kDeref},
      {"",Token::Type::kUnspecified},
#line 15 "function.txt"
      {"arccoth",Token::Type::kArccoth},
#line 28 "function.txt"
      {"cot",Token::Type::kCot},
#line 77 "function.txt"
      {"sinh",Token::Type::kSinh},
      {"",Token::Type::kUnspecified},
#line 14 "function.txt"
      {"arccot",Token::Type::kArccot},
#line 21 "function.txt"
      {"arcsinh",Token::Type::kArcsinh},
#line 37 "function.txt"
      {"exp",Token::Type::kExp},
#line 80 "function.txt"
      {"tanh",Token::Type::kTanh},
      {"",Token::Type::kUnspecified},
#line 33 "function.txt"
      {"determinant",Token::Type::kDeterminant},
#line 23 "function.txt"
      {"arctanh",Token::Type::kArctanh},
#line 76 "function.txt"
      {"sin",Token::Type::kSin},
#line 59 "function.txt"
      {"mult",Token::Type::kMult},
#line 40 "function.txt"
      {"false",Token::Type::kFalse},
#line 20 "function.txt"
      {"arcsin",Token::Type::kArcsin},
#line 46 "function.txt"
      {"ld",Token::Type::kLd},
#line 79 "function.txt"
      {"tan",Token::Type::kTan},
      {"",Token::Type::kUnspecified},
#line 52 "function.txt"
      {"log10",Token::Type::kLog10},
#line 22 "function.txt"
      {"arctan",Token::Type::kArctan},
#line 53 "function.txt"
      {"lt",Token::Type::kLt},
#line 60 "function.txt"
      {"neq",Token::Type::kNeq},
#line 82 "function.txt"
      {"transpose",Token::Type::kTranspose},
      {"",Token::Type::kUnspecified},
      {"",Token::Type::kUnspecified},
#line 48 "function.txt"
      {"ln",Token::Type::kLn},
#line 55 "function.txt"
      {"min",Token::Type::kMin},
      {"",Token::Type::kUnspecified},
#line 11 "function.txt"
      {"alloc",Token::Type::kAlloc},
      {"",Token::Type::kUnspecified},
#line 61 "function.txt"
      {"outerproduct",Token::Type::kOuterproduct},
#line 67 "function.txt"
      {"ret",Token::Type::kRet},
#line 83 "function.txt"
      {"true",Token::Type::kTrue},
#line 56 "function.txt"
      {"minus",Token::Type::kMinus},
      {"",Token::Type::kUnspecified},
#line 38 "function.txt"
      {"exponentiale",Token::Type::kExponentiale},
#line 66 "function.txt"
      {"rem",Token::Type::kRem},
      {"",Token::Type::kUnspecified},
      {"",Token::Type::kUnspecified},
#line 75 "function.txt"
      {"selrow",Token::Type::kSelrow},
#line 24 "function.txt"
      {"br",Token::Type::kBr},
#line 44 "function.txt"
      {"jmp",Token::Type::kJmp},
      {"",Token::Type::kUnspecified},
      {"",Token::Type::kUnspecified},
      {"",Token::Type::kUnspecified},
#line 45 "function.txt"
      {"lb",Token::Type::kLb},
#line 84 "function.txt"
      {"vectorproduct",Token::Type::kVectorproduct},
#line 49 "function.txt"
      {"load",Token::Type::kLoad},
#line 78 "function.txt"
      {"store",Token::Type::kStore},
      {"",Token::Type::kUnspecified},
#line 43 "function.txt"
      {"gt",Token::Type::kGt},
      {"",Token::Type::kUnspecified},
#line 39 "function.txt"
      {"factorial",Token::Type::kFactorial},
#line 64 "function.txt"
      {"power",Token::Type::kPower},
#line 34 "function.txt"
      {"divide",Token::Type::kDivide},
#line 62 "function.txt"
      {"pi",Token::Type::kPi},
#line 47 "function.txt"
      {"leq",Token::Type::kLeq},
#line 68 "function.txt"
      {"root",Token::Type::kRoot},
      {"",Token::Type::kUnspecified},
      {"",Token::Type::kUnspecified},
#line 74 "function.txt"
      {"select3",Token::Type::kSelect3},
#line 70 "function.txt"
      {"scalarproduct",Token::Type::kScalarproduct},
#line 63 "function.txt"
      {"plus",Token::Type::kPlus},
      {"",Token::Type::kUnspecified},
      {"",Token::Type::kUnspecified},
#line 73 "function.txt"
      {"select2",Token::Type::kSelect2},
      {"",Token::Type::kUnspecified},
      {"",Token::Type::kUnspecified},
      {"",Token::Type::kUnspecified},
      {"",Token::Type::kUnspecified},
      {"",Token::Type::kUnspecified},
      {"",Token::Type::kUnspecified},
      {"",Token::Type::kUnspecified},
      {"",Token::Type::kUnspecified},
      {"",Token::Type::kUnspecified},
      {"",Token::Type::kUnspecified},
      {"",Token::Type::kUnspecified},
      {"",Token::Type::kUnspecified},
      {"",Token::Type::kUnspecified},
      {"",Token::Type::kUnspecified},
      {"",Token::Type::kUnspecified},
      {"",Token::Type::kUnspecified},
      {"",Token::Type::kUnspecified},
      {"",Token::Type::kUnspecified},
      {"",Token::Type::kUnspecified},
      {"",Token::Type::kUnspecified},
#line 42 "function.txt"
      {"geq",Token::Type::kGeq},
      {"",Token::Type::kUnspecified},
      {"",Token::Type::kUnspecified},
      {"",Token::Type::kUnspecified},
      {"",Token::Type::kUnspecified},
#line 51 "function.txt"
      {"log",Token::Type::kLog},
      {"",Token::Type::kUnspecified},
      {"",Token::Type::kUnspecified},
      {"",Token::Type::kUnspecified},
      {"",Token::Type::kUnspecified},
      {"",Token::Type::kUnspecified},
      {"",Token::Type::kUnspecified},
#line 50 "function.txt"
      {"loadi",Token::Type::kLoadi},
      {"",Token::Type::kUnspecified},
      {"",Token::Type::kUnspecified},
      {"",Token::Type::kUnspecified},
      {"",Token::Type::kUnspecified},
#line 41 "function.txt"
      {"floor",Token::Type::kFloor},
      {"",Token::Type::kUnspecified},
      {"",Token::Type::kUnspecified},
      {"",Token::Type::kUnspecified},
      {"",Token::Type::kUnspecified},
      {"",Token::Type::kUnspecified},
      {"",Token::Type::kUnspecified},
      {"",Token::Type::kUnspecified},
      {"",Token::Type::kUnspecified},
      {"",Token::Type::kUnspecified},
      {"",Token::Type::kUnspecified},
      {"",Token::Type::kUnspecified},
      {"",Token::Type::kUnspecified},
      {"",Token::Type::kUnspecified},
      {"",Token::Type::kUnspecified},
      {"",Token::Type::kUnspecified},
      {"",Token::Type::kUnspecified},
      {"",Token::Type::kUnspecified},
      {"",Token::Type::kUnspecified},
      {"",Token::Type::kUnspecified},
      {"",Token::Type::kUnspecified},
      {"",Token::Type::kUnspecified},
      {"",Token::Type::kUnspecified},
      {"",Token::Type::kUnspecified},
      {"",Token::Type::kUnspecified},
#line 65 "function.txt"
      {"refer",Token::Type::kRefer},
      {"",Token::Type::kUnspecified},
      {"",Token::Type::kUnspecified},
      {"",Token::Type::kUnspecified},
#line 57 "function.txt"
      {"mmul",Token::Type::kMmul},
      {"",Token::Type::kUnspecified},
      {"",Token::Type::kUnspecified},
      {"",Token::Type::kUnspecified},
      {"",Token::Type::kUnspecified},
      {"",Token::Type::kUnspecified},
      {"",Token::Type::kUnspecified},
      {"",Token::Type::kUnspecified},
      {"",Token::Type::kUnspecified},
      {"",Token::Type::kUnspecified},
      {"",Token::Type::kUnspecified},
      {"",Token::Type::kUnspecified},
      {"",Token::Type::kUnspecified},
      {"",Token::Type::kUnspecified},
      {"",Token::Type::kUnspecified},
      {"",Token::Type::kUnspecified},
      {"",Token::Type::kUnspecified},
      {"",Token::Type::kUnspecified},
      {"",Token::Type::kUnspecified},
      {"",Token::Type::kUnspecified},
      {"",Token::Type::kUnspecified},
      {"",Token::Type::kUnspecified},
      {"",Token::Type::kUnspecified},
      {"",Token::Type::kUnspecified},
      {"",Token::Type::kUnspecified},
      {"",Token::Type::kUnspecified},
      {"",Token::Type::kUnspecified},
      {"",Token::Type::kUnspecified},
      {"",Token::Type::kUnspecified},
      {"",Token::Type::kUnspecified},
      {"",Token::Type::kUnspecified},
      {"",Token::Type::kUnspecified},
      {"",Token::Type::kUnspecified},
      {"",Token::Type::kUnspecified},
      {"",Token::Type::kUnspecified},
      {"",Token::Type::kUnspecified},
      {"",Token::Type::kUnspecified},
      {"",Token::Type::kUnspecified},
#line 25 "function.txt"
      {"ceiling",Token::Type::kCeiling}
    };

  if (len <= MAX_WORD_LENGTH && len >= MIN_WORD_LENGTH)
    {
      register int key = hash (str, len);

      if (key <= MAX_HASH_VALUE && key >= 0)
        {
          register const char *s = wordlist[key].name;

          if (*str == *s && !strncmp (str + 1, s + 1, len - 1) && s[len] == '\0')
            return &wordlist[key];
        }
    }
  return 0;
}
#line 85 "function.txt"

