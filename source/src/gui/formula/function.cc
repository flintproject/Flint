/* C++ code produced by gperf version 3.1 */
/* Command-line: gperf function.txt  */
/* Computed positions: -k'1,3-4,6' */

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
#error "gperf generated tables don't work with this execution character set. Please report a bug to <bug-gperf@gnu.org>."
#endif

#line 8 "function.txt"
struct FunctionEntry { const char *name; Tree::Op op; };
/* maximum key range = 85, duplicates = 0 */

class FunctionHash
{
private:
  static inline unsigned int hash (const char *str, size_t len);
public:
  static const struct FunctionEntry *in_word_set (const char *str, size_t len);
};

inline unsigned int
FunctionHash::hash (const char *str, size_t len)
{
  static const unsigned char asso_values[] =
    {
      88, 88, 88, 88, 88, 88, 88, 88, 88, 88,
      88, 88, 88, 88, 88, 88, 88, 88, 88, 88,
      88, 88, 88, 88, 88, 88, 88, 88, 88, 88,
      88, 88, 88, 88, 88, 88, 88, 88, 88, 88,
      88, 88, 88, 88, 88, 88, 88, 88, 88, 88,
      88, 88, 88, 88, 88, 88, 88, 88, 88, 88,
      88, 88, 88, 88, 88, 88, 88, 88, 88, 88,
      88, 88, 88, 88, 88, 88, 88, 88, 88, 88,
      88, 88, 88, 88, 88, 88, 88, 88, 88, 88,
      88, 88, 88, 88, 88, 88,  0, 25,  0, 15,
       5, 20, 10,  0,  0, 88,  0, 88, 60, 40,
       5,  0,  0, 15, 35,  5,  0, 15, 35,  0,
       0,  5, 88, 88, 88, 88, 88, 88, 88, 88,
      88, 88, 88, 88, 88, 88, 88, 88, 88, 88,
      88, 88, 88, 88, 88, 88, 88, 88, 88, 88,
      88, 88, 88, 88, 88, 88, 88, 88, 88, 88,
      88, 88, 88, 88, 88, 88, 88, 88, 88, 88,
      88, 88, 88, 88, 88, 88, 88, 88, 88, 88,
      88, 88, 88, 88, 88, 88, 88, 88, 88, 88,
      88, 88, 88, 88, 88, 88, 88, 88, 88, 88,
      88, 88, 88, 88, 88, 88, 88, 88, 88, 88,
      88, 88, 88, 88, 88, 88, 88, 88, 88, 88,
      88, 88, 88, 88, 88, 88, 88, 88, 88, 88,
      88, 88, 88, 88, 88, 88, 88, 88, 88, 88,
      88, 88, 88, 88, 88, 88, 88, 88, 88, 88,
      88, 88, 88, 88, 88, 88, 88
    };
  unsigned int hval = len;

  switch (hval)
    {
      default:
        hval += asso_values[static_cast<unsigned char>(str[5]+1)];
      /*FALLTHROUGH*/
      case 5:
      case 4:
        hval += asso_values[static_cast<unsigned char>(str[3])];
      /*FALLTHROUGH*/
      case 3:
        hval += asso_values[static_cast<unsigned char>(str[2]+1)];
      /*FALLTHROUGH*/
      case 2:
      case 1:
        hval += asso_values[static_cast<unsigned char>(str[0])];
        break;
    }
  return hval;
}

const struct FunctionEntry *
FunctionHash::in_word_set (const char *str, size_t len)
{
  enum
    {
      TOTAL_KEYWORDS = 42,
      MIN_WORD_LENGTH = 2,
      MAX_WORD_LENGTH = 19,
      MIN_HASH_VALUE = 3,
      MAX_HASH_VALUE = 87
    };

  static const struct FunctionEntry wordlist[] =
    {
      {"",Tree::Op::kCi}, {"",Tree::Op::kCi},
      {"",Tree::Op::kCi},
#line 43 "function.txt"
      {"tan",Tree::Op::kTan},
#line 44 "function.txt"
      {"tanh",Tree::Op::kTanh},
      {"",Tree::Op::kCi}, {"",Tree::Op::kCi},
      {"",Tree::Op::kCi},
#line 41 "function.txt"
      {"sin",Tree::Op::kSin},
#line 42 "function.txt"
      {"sinh",Tree::Op::kSinh},
      {"",Tree::Op::kCi}, {"",Tree::Op::kCi},
      {"",Tree::Op::kCi},
#line 39 "function.txt"
      {"sec",Tree::Op::kSec},
#line 40 "function.txt"
      {"sech",Tree::Op::kSech},
#line 32 "function.txt"
      {"floor",Tree::Op::kFloor},
      {"",Tree::Op::kCi}, {"",Tree::Op::kCi},
#line 24 "function.txt"
      {"cos",Tree::Op::kCos},
#line 25 "function.txt"
      {"cosh",Tree::Op::kCosh},
#line 49 "function.txt"
      {"poisson_variate",Tree::Op::kPoissonVariate},
      {"",Tree::Op::kCi}, {"",Tree::Op::kCi},
#line 28 "function.txt"
      {"csc",Tree::Op::kCsc},
#line 29 "function.txt"
      {"csch",Tree::Op::kCsch},
#line 37 "function.txt"
      {"power",Tree::Op::kPower},
      {"",Tree::Op::kCi}, {"",Tree::Op::kCi},
#line 10 "function.txt"
      {"abs",Tree::Op::kAbs},
#line 31 "function.txt"
      {"factorial",Tree::Op::kFactorial},
      {"",Tree::Op::kCi}, {"",Tree::Op::kCi},
      {"",Tree::Op::kCi},
#line 26 "function.txt"
      {"cot",Tree::Op::kCot},
#line 27 "function.txt"
      {"coth",Tree::Op::kCoth},
      {"",Tree::Op::kCi},
#line 21 "function.txt"
      {"arctan",Tree::Op::kArctan},
#line 22 "function.txt"
      {"arctanh",Tree::Op::kArctanh},
#line 30 "function.txt"
      {"exp",Tree::Op::kExp},
#line 38 "function.txt"
      {"root",Tree::Op::kRoot},
      {"",Tree::Op::kCi},
#line 19 "function.txt"
      {"arcsin",Tree::Op::kArcsin},
#line 20 "function.txt"
      {"arcsinh",Tree::Op::kArcsinh},
#line 36 "function.txt"
      {"min",Tree::Op::kMin},
      {"",Tree::Op::kCi},
#line 50 "function.txt"
      {"uniform_variate",Tree::Op::kUniformVariate},
#line 17 "function.txt"
      {"arcsec",Tree::Op::kArcsec},
#line 18 "function.txt"
      {"arcsech",Tree::Op::kArcsech},
#line 35 "function.txt"
      {"max",Tree::Op::kMax},
      {"",Tree::Op::kCi}, {"",Tree::Op::kCi},
#line 11 "function.txt"
      {"arccos",Tree::Op::kArccos},
#line 12 "function.txt"
      {"arccosh",Tree::Op::kArccosh},
#line 47 "function.txt"
      {"gauss_variate",Tree::Op::kGaussVariate},
      {"",Tree::Op::kCi},
#line 51 "function.txt"
      {"weibull_variate",Tree::Op::kWeibullVariate},
#line 15 "function.txt"
      {"arccsc",Tree::Op::kArccsc},
#line 16 "function.txt"
      {"arccsch",Tree::Op::kArccsch},
#line 46 "function.txt"
      {"gamma_variate",Tree::Op::kGammaVariate},
      {"",Tree::Op::kCi}, {"",Tree::Op::kCi},
      {"",Tree::Op::kCi},
#line 33 "function.txt"
      {"ln",Tree::Op::kLn},
#line 34 "function.txt"
      {"log",Tree::Op::kLog},
#line 45 "function.txt"
      {"exponential_variate",Tree::Op::kExponentialVariate},
      {"",Tree::Op::kCi},
#line 13 "function.txt"
      {"arccot",Tree::Op::kArccot},
#line 14 "function.txt"
      {"arccoth",Tree::Op::kArccoth},
      {"",Tree::Op::kCi}, {"",Tree::Op::kCi},
      {"",Tree::Op::kCi}, {"",Tree::Op::kCi},
      {"",Tree::Op::kCi}, {"",Tree::Op::kCi},
      {"",Tree::Op::kCi}, {"",Tree::Op::kCi},
      {"",Tree::Op::kCi}, {"",Tree::Op::kCi},
      {"",Tree::Op::kCi}, {"",Tree::Op::kCi},
      {"",Tree::Op::kCi}, {"",Tree::Op::kCi},
#line 23 "function.txt"
      {"ceiling",Tree::Op::kCeiling},
      {"",Tree::Op::kCi}, {"",Tree::Op::kCi},
      {"",Tree::Op::kCi}, {"",Tree::Op::kCi},
#line 48 "function.txt"
      {"lognormal_variate",Tree::Op::kLognormalVariate}
    };

  if (len <= MAX_WORD_LENGTH && len >= MIN_WORD_LENGTH)
    {
      unsigned int key = hash (str, len);

      if (key <= MAX_HASH_VALUE)
        {
          const char *s = wordlist[key].name;

          if (*str == *s && !strncmp (str + 1, s + 1, len - 1) && s[len] == '\0')
            return &wordlist[key];
        }
    }
  return 0;
}
#line 52 "function.txt"

