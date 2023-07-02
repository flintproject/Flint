/* C++ code produced by gperf version 3.1 */
/* Command-line: gperf procedure.txt  */
/* Computed positions: -k'4' */

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

#line 8 "procedure.txt"
struct ProcedureEntry { const char *name; Token::Type type; };
/* maximum key range = 23, duplicates = 0 */

class ProcedureHash
{
private:
  static inline unsigned int hash (const char *str, size_t len);
public:
  static const struct ProcedureEntry *in_word_set (const char *str, size_t len);
};

inline unsigned int
ProcedureHash::hash (const char *str, size_t len)
{
  static const unsigned char asso_values[] =
    {
      26, 26, 26, 26, 26, 26, 26, 26, 26, 26,
      26, 26, 26, 26, 26, 26, 26, 26, 26, 26,
      26, 26, 26, 26, 26, 26, 26, 26, 26, 26,
      26, 26, 26, 26, 26, 26, 26, 26, 26, 26,
      26, 26, 26, 26, 26, 26, 26, 26, 26, 26,
      26, 26, 26, 26, 26, 26, 26, 26, 26, 26,
      26, 26, 26, 26, 26, 26, 26, 26, 26, 26,
      26, 26, 26, 26, 26, 26, 26, 26, 26, 26,
      26, 26, 26, 26, 26, 26, 26, 26, 26, 26,
      26, 26, 26, 26, 26, 26, 26, 26, 10, 26,
      26, 26,  5, 26, 26, 26, 26, 26, 26,  5,
       0,  0, 26, 26, 26,  0, 26, 26, 26, 26,
      26, 26, 26, 26, 26, 26, 26, 26, 26, 26,
      26, 26, 26, 26, 26, 26, 26, 26, 26, 26,
      26, 26, 26, 26, 26, 26, 26, 26, 26, 26,
      26, 26, 26, 26, 26, 26, 26, 26, 26, 26,
      26, 26, 26, 26, 26, 26, 26, 26, 26, 26,
      26, 26, 26, 26, 26, 26, 26, 26, 26, 26,
      26, 26, 26, 26, 26, 26, 26, 26, 26, 26,
      26, 26, 26, 26, 26, 26, 26, 26, 26, 26,
      26, 26, 26, 26, 26, 26, 26, 26, 26, 26,
      26, 26, 26, 26, 26, 26, 26, 26, 26, 26,
      26, 26, 26, 26, 26, 26, 26, 26, 26, 26,
      26, 26, 26, 26, 26, 26, 26, 26, 26, 26,
      26, 26, 26, 26, 26, 26, 26, 26, 26, 26,
      26, 26, 26, 26, 26, 26
    };
  unsigned int hval = len;

  switch (hval)
    {
      default:
        hval += asso_values[static_cast<unsigned char>(str[3])];
      /*FALLTHROUGH*/
      case 3:
        break;
    }
  return hval;
}

const struct ProcedureEntry *
ProcedureHash::in_word_set (const char *str, size_t len)
{
  enum
    {
      TOTAL_KEYWORDS = 8,
      MIN_WORD_LENGTH = 3,
      MAX_WORD_LENGTH = 19,
      MIN_HASH_VALUE = 3,
      MAX_HASH_VALUE = 25
    };

  static const struct ProcedureEntry wordlist[] =
    {
      {"",Token::Type::kUnspecified},
      {"",Token::Type::kUnspecified},
      {"",Token::Type::kUnspecified},
#line 10 "procedure.txt"
      {"Mod",Token::Type::kMod},
      {"",Token::Type::kUnspecified},
      {"",Token::Type::kUnspecified},
      {"",Token::Type::kUnspecified},
      {"",Token::Type::kUnspecified},
      {"",Token::Type::kUnspecified},
      {"",Token::Type::kUnspecified},
      {"",Token::Type::kUnspecified},
      {"",Token::Type::kUnspecified},
      {"",Token::Type::kUnspecified},
#line 13 "procedure.txt"
      {"gauss_variate",Token::Type::kGaussVariate},
      {"",Token::Type::kUnspecified},
#line 15 "procedure.txt"
      {"poisson_variate",Token::Type::kPoissonVariate},
      {"",Token::Type::kUnspecified},
#line 14 "procedure.txt"
      {"lognormal_variate",Token::Type::kLognormalVariate},
#line 12 "procedure.txt"
      {"gamma_variate",Token::Type::kGammaVariate},
#line 11 "procedure.txt"
      {"exponential_variate",Token::Type::kExponentialVariate},
#line 16 "procedure.txt"
      {"uniform_variate",Token::Type::kUniformVariate},
      {"",Token::Type::kUnspecified},
      {"",Token::Type::kUnspecified},
      {"",Token::Type::kUnspecified},
      {"",Token::Type::kUnspecified},
#line 17 "procedure.txt"
      {"weibull_variate",Token::Type::kWeibullVariate}
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
#line 18 "procedure.txt"

