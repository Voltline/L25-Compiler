// A Bison parser, made by GNU Bison 3.8.2.

// Skeleton implementation for Bison LALR(1) parsers in C++

// Copyright (C) 2002-2015, 2018-2021 Free Software Foundation, Inc.

// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.

// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <https://www.gnu.org/licenses/>.

// As a special exception, you may create a larger work that contains
// part or all of the Bison parser skeleton and distribute that work
// under terms of your choice, so long as that work isn't itself a
// parser generator using the skeleton or a modified version thereof
// as a parser skeleton.  Alternatively, if you modify or redistribute
// the parser skeleton itself, you may (at your option) remove this
// special exception, which will cause the skeleton and the resulting
// Bison output files to be licensed under the GNU General Public
// License without this special exception.

// This special exception was added by the Free Software Foundation in
// version 2.2 of Bison.

// DO NOT RELY ON FEATURES THAT ARE NOT DOCUMENTED in the manual,
// especially those whose name start with YY_ or yy_.  They are
// private implementation details that can be changed or removed.



// First part of user prologue.
#line 3 "parser.y"

#include<stdio.h>
#include<stdlib.h>
#include<malloc.h>
#include<memory.h>
#include<string.h>


#define txmax 100     /* 符号表容量 */
#define al 10         /* 标识符的最大长度 */

/* 符号表中的类型 */
enum object {
    constant,
    variable,
    procedure,
};

/* 符号表结构 */
struct tablestruct
{
   char name[al];     /* 名字 */
   enum object kind;  /* 类型：const，var或procedure */
};
struct tablestruct table[txmax]; /* 符号表 */


int tx;         /* 符号表当前尾指针 */
char id[al];

FILE* fin;      /* 输入源文件 */
FILE* foutput;  /* 输出出错示意（如有错） */
char fname[al];
int err;
extern int line;

int yydebug = 0;

void init();
void enter(enum object k);
int position(char *s);
void redirectInput(FILE* fin);
int yyerror(char *s);
int yylex(void);

#line 87 "parser.tab.cc"


#include "parser.tab.hh"




#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> // FIXME: INFRINGES ON USER NAME SPACE.
#   define YY_(msgid) dgettext ("bison-runtime", msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(msgid) msgid
# endif
#endif


// Whether we are compiled with exception support.
#ifndef YY_EXCEPTIONS
# if defined __GNUC__ && !defined __EXCEPTIONS
#  define YY_EXCEPTIONS 0
# else
#  define YY_EXCEPTIONS 1
# endif
#endif



// Enable debugging if requested.
#if YYDEBUG

// A pseudo ostream that takes yydebug_ into account.
# define YYCDEBUG if (yydebug_) (*yycdebug_)

# define YY_SYMBOL_PRINT(Title, Symbol)         \
  do {                                          \
    if (yydebug_)                               \
    {                                           \
      *yycdebug_ << Title << ' ';               \
      yy_print_ (*yycdebug_, Symbol);           \
      *yycdebug_ << '\n';                       \
    }                                           \
  } while (false)

# define YY_REDUCE_PRINT(Rule)          \
  do {                                  \
    if (yydebug_)                       \
      yy_reduce_print_ (Rule);          \
  } while (false)

# define YY_STACK_PRINT()               \
  do {                                  \
    if (yydebug_)                       \
      yy_stack_print_ ();                \
  } while (false)

#else // !YYDEBUG

# define YYCDEBUG if (false) std::cerr
# define YY_SYMBOL_PRINT(Title, Symbol)  YY_USE (Symbol)
# define YY_REDUCE_PRINT(Rule)           static_cast<void> (0)
# define YY_STACK_PRINT()                static_cast<void> (0)

#endif // !YYDEBUG

#define yyerrok         (yyerrstatus_ = 0)
#define yyclearin       (yyla.clear ())

#define YYACCEPT        goto yyacceptlab
#define YYABORT         goto yyabortlab
#define YYERROR         goto yyerrorlab
#define YYRECOVERING()  (!!yyerrstatus_)

namespace yy {
#line 165 "parser.tab.cc"

  /// Build a parser object.
  parser::parser ()
#if YYDEBUG
    : yydebug_ (false),
      yycdebug_ (&std::cerr)
#else

#endif
  {}

  parser::~parser ()
  {}

  parser::syntax_error::~syntax_error () YY_NOEXCEPT YY_NOTHROW
  {}

  /*---------.
  | symbol.  |
  `---------*/

  // basic_symbol.
  template <typename Base>
  parser::basic_symbol<Base>::basic_symbol (const basic_symbol& that)
    : Base (that)
    , value (that.value)
  {}


  /// Constructor for valueless symbols.
  template <typename Base>
  parser::basic_symbol<Base>::basic_symbol (typename Base::kind_type t)
    : Base (t)
    , value ()
  {}

  template <typename Base>
  parser::basic_symbol<Base>::basic_symbol (typename Base::kind_type t, YY_RVREF (value_type) v)
    : Base (t)
    , value (YY_MOVE (v))
  {}


  template <typename Base>
  parser::symbol_kind_type
  parser::basic_symbol<Base>::type_get () const YY_NOEXCEPT
  {
    return this->kind ();
  }


  template <typename Base>
  bool
  parser::basic_symbol<Base>::empty () const YY_NOEXCEPT
  {
    return this->kind () == symbol_kind::S_YYEMPTY;
  }

  template <typename Base>
  void
  parser::basic_symbol<Base>::move (basic_symbol& s)
  {
    super_type::move (s);
    value = YY_MOVE (s.value);
  }

  // by_kind.
  parser::by_kind::by_kind () YY_NOEXCEPT
    : kind_ (symbol_kind::S_YYEMPTY)
  {}

#if 201103L <= YY_CPLUSPLUS
  parser::by_kind::by_kind (by_kind&& that) YY_NOEXCEPT
    : kind_ (that.kind_)
  {
    that.clear ();
  }
#endif

  parser::by_kind::by_kind (const by_kind& that) YY_NOEXCEPT
    : kind_ (that.kind_)
  {}

  parser::by_kind::by_kind (token_kind_type t) YY_NOEXCEPT
    : kind_ (yytranslate_ (t))
  {}



  void
  parser::by_kind::clear () YY_NOEXCEPT
  {
    kind_ = symbol_kind::S_YYEMPTY;
  }

  void
  parser::by_kind::move (by_kind& that)
  {
    kind_ = that.kind_;
    that.clear ();
  }

  parser::symbol_kind_type
  parser::by_kind::kind () const YY_NOEXCEPT
  {
    return kind_;
  }


  parser::symbol_kind_type
  parser::by_kind::type_get () const YY_NOEXCEPT
  {
    return this->kind ();
  }



  // by_state.
  parser::by_state::by_state () YY_NOEXCEPT
    : state (empty_state)
  {}

  parser::by_state::by_state (const by_state& that) YY_NOEXCEPT
    : state (that.state)
  {}

  void
  parser::by_state::clear () YY_NOEXCEPT
  {
    state = empty_state;
  }

  void
  parser::by_state::move (by_state& that)
  {
    state = that.state;
    that.clear ();
  }

  parser::by_state::by_state (state_type s) YY_NOEXCEPT
    : state (s)
  {}

  parser::symbol_kind_type
  parser::by_state::kind () const YY_NOEXCEPT
  {
    if (state == empty_state)
      return symbol_kind::S_YYEMPTY;
    else
      return YY_CAST (symbol_kind_type, yystos_[+state]);
  }

  parser::stack_symbol_type::stack_symbol_type ()
  {}

  parser::stack_symbol_type::stack_symbol_type (YY_RVREF (stack_symbol_type) that)
    : super_type (YY_MOVE (that.state), YY_MOVE (that.value))
  {
#if 201103L <= YY_CPLUSPLUS
    // that is emptied.
    that.state = empty_state;
#endif
  }

  parser::stack_symbol_type::stack_symbol_type (state_type s, YY_MOVE_REF (symbol_type) that)
    : super_type (s, YY_MOVE (that.value))
  {
    // that is emptied.
    that.kind_ = symbol_kind::S_YYEMPTY;
  }

#if YY_CPLUSPLUS < 201103L
  parser::stack_symbol_type&
  parser::stack_symbol_type::operator= (const stack_symbol_type& that)
  {
    state = that.state;
    value = that.value;
    return *this;
  }

  parser::stack_symbol_type&
  parser::stack_symbol_type::operator= (stack_symbol_type& that)
  {
    state = that.state;
    value = that.value;
    // that is emptied.
    that.state = empty_state;
    return *this;
  }
#endif

  template <typename Base>
  void
  parser::yy_destroy_ (const char* yymsg, basic_symbol<Base>& yysym) const
  {
    if (yymsg)
      YY_SYMBOL_PRINT (yymsg, yysym);

    // User destructor.
    YY_USE (yysym.kind ());
  }

#if YYDEBUG
  template <typename Base>
  void
  parser::yy_print_ (std::ostream& yyo, const basic_symbol<Base>& yysym) const
  {
    std::ostream& yyoutput = yyo;
    YY_USE (yyoutput);
    if (yysym.empty ())
      yyo << "empty symbol";
    else
      {
        symbol_kind_type yykind = yysym.kind ();
        yyo << (yykind < YYNTOKENS ? "token" : "nterm")
            << ' ' << yysym.name () << " (";
        YY_USE (yykind);
        yyo << ')';
      }
  }
#endif

  void
  parser::yypush_ (const char* m, YY_MOVE_REF (stack_symbol_type) sym)
  {
    if (m)
      YY_SYMBOL_PRINT (m, sym);
    yystack_.push (YY_MOVE (sym));
  }

  void
  parser::yypush_ (const char* m, state_type s, YY_MOVE_REF (symbol_type) sym)
  {
#if 201103L <= YY_CPLUSPLUS
    yypush_ (m, stack_symbol_type (s, std::move (sym)));
#else
    stack_symbol_type ss (s, sym);
    yypush_ (m, ss);
#endif
  }

  void
  parser::yypop_ (int n) YY_NOEXCEPT
  {
    yystack_.pop (n);
  }

#if YYDEBUG
  std::ostream&
  parser::debug_stream () const
  {
    return *yycdebug_;
  }

  void
  parser::set_debug_stream (std::ostream& o)
  {
    yycdebug_ = &o;
  }


  parser::debug_level_type
  parser::debug_level () const
  {
    return yydebug_;
  }

  void
  parser::set_debug_level (debug_level_type l)
  {
    yydebug_ = l;
  }
#endif // YYDEBUG

  parser::state_type
  parser::yy_lr_goto_state_ (state_type yystate, int yysym)
  {
    int yyr = yypgoto_[yysym - YYNTOKENS] + yystate;
    if (0 <= yyr && yyr <= yylast_ && yycheck_[yyr] == yystate)
      return yytable_[yyr];
    else
      return yydefgoto_[yysym - YYNTOKENS];
  }

  bool
  parser::yy_pact_value_is_default_ (int yyvalue) YY_NOEXCEPT
  {
    return yyvalue == yypact_ninf_;
  }

  bool
  parser::yy_table_value_is_error_ (int yyvalue) YY_NOEXCEPT
  {
    return yyvalue == yytable_ninf_;
  }

  int
  parser::operator() ()
  {
    return parse ();
  }

  int
  parser::parse ()
  {
    int yyn;
    /// Length of the RHS of the rule being reduced.
    int yylen = 0;

    // Error handling.
    int yynerrs_ = 0;
    int yyerrstatus_ = 0;

    /// The lookahead symbol.
    symbol_type yyla;

    /// The return value of parse ().
    int yyresult;

#if YY_EXCEPTIONS
    try
#endif // YY_EXCEPTIONS
      {
    YYCDEBUG << "Starting parse\n";


    /* Initialize the stack.  The initial state will be set in
       yynewstate, since the latter expects the semantical and the
       location values to have been already stored, initialize these
       stacks with a primary value.  */
    yystack_.clear ();
    yypush_ (YY_NULLPTR, 0, YY_MOVE (yyla));

  /*-----------------------------------------------.
  | yynewstate -- push a new symbol on the stack.  |
  `-----------------------------------------------*/
  yynewstate:
    YYCDEBUG << "Entering state " << int (yystack_[0].state) << '\n';
    YY_STACK_PRINT ();

    // Accept?
    if (yystack_[0].state == yyfinal_)
      YYACCEPT;

    goto yybackup;


  /*-----------.
  | yybackup.  |
  `-----------*/
  yybackup:
    // Try to take a decision without lookahead.
    yyn = yypact_[+yystack_[0].state];
    if (yy_pact_value_is_default_ (yyn))
      goto yydefault;

    // Read a lookahead token.
    if (yyla.empty ())
      {
        YYCDEBUG << "Reading a token\n";
#if YY_EXCEPTIONS
        try
#endif // YY_EXCEPTIONS
          {
            yyla.kind_ = yytranslate_ (yylex (&yyla.value));
          }
#if YY_EXCEPTIONS
        catch (const syntax_error& yyexc)
          {
            YYCDEBUG << "Caught exception: " << yyexc.what() << '\n';
            error (yyexc);
            goto yyerrlab1;
          }
#endif // YY_EXCEPTIONS
      }
    YY_SYMBOL_PRINT ("Next token is", yyla);

    if (yyla.kind () == symbol_kind::S_YYerror)
    {
      // The scanner already issued an error message, process directly
      // to error recovery.  But do not keep the error token as
      // lookahead, it is too special and may lead us to an endless
      // loop in error recovery. */
      yyla.kind_ = symbol_kind::S_YYUNDEF;
      goto yyerrlab1;
    }

    /* If the proper action on seeing token YYLA.TYPE is to reduce or
       to detect an error, take that action.  */
    yyn += yyla.kind ();
    if (yyn < 0 || yylast_ < yyn || yycheck_[yyn] != yyla.kind ())
      {
        goto yydefault;
      }

    // Reduce or error.
    yyn = yytable_[yyn];
    if (yyn <= 0)
      {
        if (yy_table_value_is_error_ (yyn))
          goto yyerrlab;
        yyn = -yyn;
        goto yyreduce;
      }

    // Count tokens shifted since error; after three, turn off error status.
    if (yyerrstatus_)
      --yyerrstatus_;

    // Shift the lookahead token.
    yypush_ ("Shifting", state_type (yyn), YY_MOVE (yyla));
    goto yynewstate;


  /*-----------------------------------------------------------.
  | yydefault -- do the default action for the current state.  |
  `-----------------------------------------------------------*/
  yydefault:
    yyn = yydefact_[+yystack_[0].state];
    if (yyn == 0)
      goto yyerrlab;
    goto yyreduce;


  /*-----------------------------.
  | yyreduce -- do a reduction.  |
  `-----------------------------*/
  yyreduce:
    yylen = yyr2_[yyn];
    {
      stack_symbol_type yylhs;
      yylhs.state = yy_lr_goto_state_ (yystack_[yylen].state, yyr1_[yyn]);
      /* If YYLEN is nonzero, implement the default value of the
         action: '$$ = $1'.  Otherwise, use the top of the stack.

         Otherwise, the following line sets YYLHS.VALUE to garbage.
         This behavior is undocumented and Bison users should not rely
         upon it.  */
      if (yylen)
        yylhs.value = yystack_[yylen - 1].value;
      else
        yylhs.value = yystack_[0].value;


      // Perform the reduction.
      YY_REDUCE_PRINT (yyn);
#if YY_EXCEPTIONS
      try
#endif // YY_EXCEPTIONS
        {
          switch (yyn)
            {
  case 8: // constdef: IDENT EQL NUMBER
#line 85 "parser.y"
            {
               strcpy(id,(yystack_[2].value.ident));
               enter(constant);
            }
#line 624 "parser.tab.cc"
    break;

  case 13: // vardef: IDENT
#line 98 "parser.y"
            {
              strcpy(id, (yystack_[0].value.ident));
              enter(variable);
            }
#line 633 "parser.tab.cc"
    break;

  case 16: // procdecl: PROCSYM IDENT SEMICOLON
#line 109 "parser.y"
               {
                 strcpy(id, (yystack_[1].value.ident));
	         enter(procedure);
               }
#line 642 "parser.tab.cc"
    break;

  case 28: // assignmentstm: ident BECOMES expression
#line 124 "parser.y"
               {
                 if ((yystack_[2].value.number) == 0)
                       yyerror("Symbol does not exist");
                 else
                    {
                       if (table[(yystack_[2].value.number)].kind != variable)
                           yyerror("Symbol should be a variable");
                    }
               }
#line 656 "parser.tab.cc"
    break;

  case 29: // callstm: CALLSYM ident
#line 136 "parser.y"
             {
                 if ((yystack_[0].value.number) == 0)
                       yyerror("Symbol does not exist");
                 else
                    {
                       if (table[(yystack_[0].value.number)].kind != procedure)
                           yyerror("Symbol should be a procedure");
                    }
              }
#line 670 "parser.tab.cc"
    break;

  case 40: // readvar: ident
#line 173 "parser.y"
        {}
#line 676 "parser.tab.cc"
    break;

  case 59: // factor: ident
#line 207 "parser.y"
               { if ((yystack_[0].value.number) == 0)
                       yyerror("Symbol does not exist");
                 else
                    {
                       if (table[(yystack_[0].value.number)].kind == procedure)
                           yyerror("Symbol should not be a procedure");
                    }
                }
#line 689 "parser.tab.cc"
    break;

  case 60: // factor: NUMBER
#line 215 "parser.y"
                {}
#line 695 "parser.tab.cc"
    break;

  case 62: // ident: IDENT
#line 219 "parser.y"
         {
           (yylhs.value.number) = position ((yystack_[0].value.ident));
         }
#line 703 "parser.tab.cc"
    break;


#line 707 "parser.tab.cc"

            default:
              break;
            }
        }
#if YY_EXCEPTIONS
      catch (const syntax_error& yyexc)
        {
          YYCDEBUG << "Caught exception: " << yyexc.what() << '\n';
          error (yyexc);
          YYERROR;
        }
#endif // YY_EXCEPTIONS
      YY_SYMBOL_PRINT ("-> $$ =", yylhs);
      yypop_ (yylen);
      yylen = 0;

      // Shift the result of the reduction.
      yypush_ (YY_NULLPTR, YY_MOVE (yylhs));
    }
    goto yynewstate;


  /*--------------------------------------.
  | yyerrlab -- here on detecting error.  |
  `--------------------------------------*/
  yyerrlab:
    // If not already recovering from an error, report this error.
    if (!yyerrstatus_)
      {
        ++yynerrs_;
        std::string msg = YY_("syntax error");
        error (YY_MOVE (msg));
      }


    if (yyerrstatus_ == 3)
      {
        /* If just tried and failed to reuse lookahead token after an
           error, discard it.  */

        // Return failure if at end of input.
        if (yyla.kind () == symbol_kind::S_YYEOF)
          YYABORT;
        else if (!yyla.empty ())
          {
            yy_destroy_ ("Error: discarding", yyla);
            yyla.clear ();
          }
      }

    // Else will try to reuse lookahead token after shifting the error token.
    goto yyerrlab1;


  /*---------------------------------------------------.
  | yyerrorlab -- error raised explicitly by YYERROR.  |
  `---------------------------------------------------*/
  yyerrorlab:
    /* Pacify compilers when the user code never invokes YYERROR and
       the label yyerrorlab therefore never appears in user code.  */
    if (false)
      YYERROR;

    /* Do not reclaim the symbols of the rule whose action triggered
       this YYERROR.  */
    yypop_ (yylen);
    yylen = 0;
    YY_STACK_PRINT ();
    goto yyerrlab1;


  /*-------------------------------------------------------------.
  | yyerrlab1 -- common code for both syntax error and YYERROR.  |
  `-------------------------------------------------------------*/
  yyerrlab1:
    yyerrstatus_ = 3;   // Each real token shifted decrements this.
    // Pop stack until we find a state that shifts the error token.
    for (;;)
      {
        yyn = yypact_[+yystack_[0].state];
        if (!yy_pact_value_is_default_ (yyn))
          {
            yyn += symbol_kind::S_YYerror;
            if (0 <= yyn && yyn <= yylast_
                && yycheck_[yyn] == symbol_kind::S_YYerror)
              {
                yyn = yytable_[yyn];
                if (0 < yyn)
                  break;
              }
          }

        // Pop the current state because it cannot handle the error token.
        if (yystack_.size () == 1)
          YYABORT;

        yy_destroy_ ("Error: popping", yystack_[0]);
        yypop_ ();
        YY_STACK_PRINT ();
      }
    {
      stack_symbol_type error_token;


      // Shift the error token.
      error_token.state = state_type (yyn);
      yypush_ ("Shifting", YY_MOVE (error_token));
    }
    goto yynewstate;


  /*-------------------------------------.
  | yyacceptlab -- YYACCEPT comes here.  |
  `-------------------------------------*/
  yyacceptlab:
    yyresult = 0;
    goto yyreturn;


  /*-----------------------------------.
  | yyabortlab -- YYABORT comes here.  |
  `-----------------------------------*/
  yyabortlab:
    yyresult = 1;
    goto yyreturn;


  /*-----------------------------------------------------.
  | yyreturn -- parsing is finished, return the result.  |
  `-----------------------------------------------------*/
  yyreturn:
    if (!yyla.empty ())
      yy_destroy_ ("Cleanup: discarding lookahead", yyla);

    /* Do not reclaim the symbols of the rule whose action triggered
       this YYABORT or YYACCEPT.  */
    yypop_ (yylen);
    YY_STACK_PRINT ();
    while (1 < yystack_.size ())
      {
        yy_destroy_ ("Cleanup: popping", yystack_[0]);
        yypop_ ();
      }

    return yyresult;
  }
#if YY_EXCEPTIONS
    catch (...)
      {
        YYCDEBUG << "Exception caught: cleaning lookahead and stack\n";
        // Do not try to display the values of the reclaimed symbols,
        // as their printers might throw an exception.
        if (!yyla.empty ())
          yy_destroy_ (YY_NULLPTR, yyla);

        while (1 < yystack_.size ())
          {
            yy_destroy_ (YY_NULLPTR, yystack_[0]);
            yypop_ ();
          }
        throw;
      }
#endif // YY_EXCEPTIONS
  }

  void
  parser::error (const syntax_error& yyexc)
  {
    error (yyexc.what ());
  }

#if YYDEBUG || 0
  const char *
  parser::symbol_name (symbol_kind_type yysymbol)
  {
    return yytname_[yysymbol];
  }
#endif // #if YYDEBUG || 0









  const signed char parser::yypact_ninf_ = -49;

  const signed char parser::yytable_ninf_ = -1;

  const signed char
  parser::yypact_[] =
  {
      -2,   -28,    22,    27,    59,    45,   -14,   -49,   -49,   -49,
      55,   -49,    61,   -28,   -49,   -49,    17,   -49,     7,   -49,
     -49,    55,   -49,    28,    68,    53,    70,    76,    53,    78,
      28,    53,   -49,    -2,   -49,   -49,   -49,   -49,   -49,   -49,
     -49,   -49,   -49,   -49,    88,   -49,   -49,     0,   -49,    44,
      21,    21,    44,   -49,    95,    62,    12,   -49,   -49,    75,
      68,   105,    44,   101,   102,    80,   -49,    44,   -49,    28,
      77,    12,    12,    29,    28,    44,    44,    44,    44,    21,
      21,    44,    44,    21,    21,   -49,    31,   -49,   -49,    28,
      69,    77,    53,    28,   -49,    77,   -49,   -49,   -49,    77,
      77,    77,    77,    12,    12,    77,    77,   -49,   -49,   -49,
      68,   -49,   -49,    44,   -49,    99,   -49,    77,    28,   -49
  };

  const signed char
  parser::yydefact_[] =
  {
       5,     0,     0,     0,    10,     0,     0,     6,     1,     2,
       0,    15,     0,     0,     4,    13,     0,    11,    27,     8,
       7,     0,     9,    27,     0,     0,     0,     0,     0,     0,
      27,     0,    62,     5,     3,    18,    19,    20,    21,    22,
      25,    26,    23,    24,     0,    12,    31,     0,    29,     0,
       0,     0,     0,    60,     0,     0,    53,    56,    59,     0,
       0,     0,     0,     0,     0,     0,    14,     0,    30,    27,
      44,    51,    52,     0,    27,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    16,     0,    38,    40,    27,
       0,    42,     0,    27,    17,    28,    32,    61,    33,    47,
      48,    49,    50,    54,    55,    45,    46,    57,    58,    37,
       0,    34,    41,     0,    35,     0,    39,    43,    27,    36
  };

  const signed char
  parser::yypgoto_[] =
  {
     -49,   -49,    83,   -49,   -49,   104,   -49,   -49,    97,   -49,
     -49,   -49,   -21,   -49,   -49,   -49,   -49,   -49,   -49,   -49,
     -49,   -49,   -49,     9,   -49,   -49,   -15,   -48,   -13,    11,
     -18
  };

  const signed char
  parser::yydefgoto_[] =
  {
       0,     2,     3,     4,     6,     7,    11,    16,    17,    18,
      33,    66,    34,    35,    36,    37,    47,    38,    39,    40,
      41,    42,    86,    87,    43,    90,    54,    55,    56,    57,
      58
  };

  const signed char
  parser::yytable_[] =
  {
      44,    70,    46,     1,    73,    44,    48,    68,     5,    63,
      23,    24,    44,    61,    91,    25,    64,    26,    13,    95,
      27,    14,     8,    28,    29,    30,    31,    99,   100,   101,
     102,    23,    24,   105,   106,    69,    25,    71,    72,    83,
      84,    27,    88,    32,    28,    29,    30,    31,    96,    21,
      52,    44,    22,    98,    79,    80,    44,    32,    53,    97,
       9,   109,    49,   110,    32,   117,   103,   104,   111,    50,
      51,    44,   115,    52,    10,    44,    12,   114,    50,    51,
      32,    53,    52,    75,    76,    77,    78,    79,    80,    32,
      53,    15,    88,    81,   107,   108,    82,   119,    19,   112,
      44,   113,    79,    80,    32,    60,    59,    62,    67,    74,
      85,    89,    92,   118,    93,    94,    65,    20,    45,   116
  };

  const signed char
  parser::yycheck_[] =
  {
      18,    49,    23,     5,    52,    23,    24,     7,    36,    30,
       3,     4,    30,    28,    62,     8,    31,    10,    32,    67,
      13,    35,     0,    16,    17,    18,    19,    75,    76,    77,
      78,     3,     4,    81,    82,    35,     8,    50,    51,    27,
      28,    13,    60,    36,    16,    17,    18,    19,    69,    32,
      29,    69,    35,    74,    25,    26,    74,    36,    37,    30,
      33,    30,     9,    32,    36,   113,    79,    80,    89,    25,
      26,    89,    93,    29,    15,    93,    31,    92,    25,    26,
      36,    37,    29,    21,    22,    23,    24,    25,    26,    36,
      37,    36,   110,    31,    83,    84,    34,   118,    37,    30,
     118,    32,    25,    26,    36,    29,    36,    29,    20,    14,
      35,     6,    11,    14,    12,    35,    33,    13,    21,   110
  };

  const signed char
  parser::yystos_[] =
  {
       0,     5,    39,    40,    41,    36,    42,    43,     0,    33,
      15,    44,    31,    32,    35,    36,    45,    46,    47,    37,
      43,    32,    35,     3,     4,     8,    10,    13,    16,    17,
      18,    19,    36,    48,    50,    51,    52,    53,    55,    56,
      57,    58,    59,    62,    68,    46,    50,    54,    68,     9,
      25,    26,    29,    37,    64,    65,    66,    67,    68,    36,
      29,    64,    29,    50,    64,    40,    49,    20,     7,    35,
      65,    66,    66,    65,    14,    21,    22,    23,    24,    25,
      26,    31,    34,    27,    28,    35,    60,    61,    68,     6,
      63,    65,    11,    12,    35,    65,    50,    30,    50,    65,
      65,    65,    65,    66,    66,    65,    65,    67,    67,    30,
      32,    50,    30,    32,    64,    50,    61,    65,    14,    50
  };

  const signed char
  parser::yyr1_[] =
  {
       0,    38,    39,    40,    41,    41,    42,    42,    43,    44,
      44,    45,    45,    46,    47,    47,    48,    49,    50,    50,
      50,    50,    50,    50,    50,    50,    50,    50,    51,    52,
      53,    54,    54,    55,    56,    57,    58,    59,    60,    60,
      61,    62,    63,    63,    64,    64,    64,    64,    64,    64,
      64,    65,    65,    65,    65,    65,    66,    66,    66,    67,
      67,    67,    68
  };

  const signed char
  parser::yyr2_[] =
  {
       0,     2,     2,     4,     3,     0,     1,     3,     3,     3,
       0,     1,     3,     1,     3,     0,     3,     2,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     0,     3,     2,
       3,     1,     3,     4,     4,     4,     6,     4,     1,     3,
       1,     4,     1,     3,     2,     3,     3,     3,     3,     3,
       3,     2,     2,     1,     3,     3,     1,     3,     3,     1,
       1,     3,     1
  };


#if YYDEBUG
  // YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
  // First, the terminals, then, starting at \a YYNTOKENS, nonterminals.
  const char*
  const parser::yytname_[] =
  {
  "\"end of file\"", "error", "\"invalid token\"", "BEGINSYM", "CALLSYM",
  "CONSTSYM", "DOSYM", "ENDSYM", "IFSYM", "ODDSYM", "PROCSYM", "UNTILSYM",
  "WITHSYM", "READSYM", "THENSYM", "VARSYM", "WHILESYM", "WRITESYM",
  "REPEATSYM", "FORSYM", "BECOMES", "LSS", "LEQ", "GTR", "GEQ", "PLUS",
  "MINUS", "TIMES", "SLASH", "LPAREN", "RPAREN", "EQL", "COMMA", "PERIOD",
  "NEQ", "SEMICOLON", "IDENT", "NUMBER", "$accept", "program", "block",
  "constdecl", "constlist", "constdef", "vardecl", "varlist", "vardef",
  "procdecls", "procdecl", "procbody", "statement", "assignmentstm",
  "callstm", "compoundstm", "statements", "ifstm", "whilestm", "repeatstm",
  "forstm", "readstm", "readvarlist", "readvar", "writestm",
  "writeexplist", "condition", "expression", "term", "factor", "ident", YY_NULLPTR
  };
#endif


#if YYDEBUG
  const unsigned char
  parser::yyrline_[] =
  {
       0,    72,    72,    75,    78,    78,    81,    81,    84,    91,
      91,    94,    94,    97,   105,   105,   108,   116,   119,   119,
     119,   119,   120,   120,   120,   120,   120,   120,   123,   135,
     148,   151,   151,   154,   157,   160,   163,   166,   169,   169,
     172,   176,   179,   179,   182,   183,   184,   185,   186,   187,
     188,   192,   193,   194,   195,   196,   200,   201,   202,   206,
     215,   216,   218
  };

  void
  parser::yy_stack_print_ () const
  {
    *yycdebug_ << "Stack now";
    for (stack_type::const_iterator
           i = yystack_.begin (),
           i_end = yystack_.end ();
         i != i_end; ++i)
      *yycdebug_ << ' ' << int (i->state);
    *yycdebug_ << '\n';
  }

  void
  parser::yy_reduce_print_ (int yyrule) const
  {
    int yylno = yyrline_[yyrule];
    int yynrhs = yyr2_[yyrule];
    // Print the symbols being reduced, and their result.
    *yycdebug_ << "Reducing stack by rule " << yyrule - 1
               << " (line " << yylno << "):\n";
    // The symbols being reduced.
    for (int yyi = 0; yyi < yynrhs; yyi++)
      YY_SYMBOL_PRINT ("   $" << yyi + 1 << " =",
                       yystack_[(yynrhs) - (yyi + 1)]);
  }
#endif // YYDEBUG

  parser::symbol_kind_type
  parser::yytranslate_ (int t) YY_NOEXCEPT
  {
    // YYTRANSLATE[TOKEN-NUM] -- Symbol number corresponding to
    // TOKEN-NUM as returned by yylex.
    static
    const signed char
    translate_table[] =
    {
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37
    };
    // Last valid token kind.
    const int code_max = 292;

    if (t <= 0)
      return symbol_kind::S_YYEOF;
    else if (t <= code_max)
      return static_cast <symbol_kind_type> (translate_table[t]);
    else
      return symbol_kind::S_YYUNDEF;
  }

} // yy
#line 1141 "parser.tab.cc"

#line 228 "parser.y"

int yyerror(char *s)
{
	err = err + 1;
        printf("%s in line %d\n", s, line);
	fprintf(foutput, "%s in line %d\n", s, line);
	return 0;
}

void init()
{
	tx = 0;
        err = 0;
}

void enter(enum object k)
{
	tx = tx + 1;
	strcpy(table[tx].name, id);
	table[tx].kind = k;
}

int position(char *s)
{
	int i;
	strcpy(table[0].name, s);
	i = tx;
	while(strcmp(table[i].name, s) != 0)
		i--;
	return i;
}

int main(void)
{
	printf("Input pl/0 file?   ");
	scanf("%s", fname);		/* 输入文件名 */

	if ((fin = fopen(fname, "r")) == NULL)
	{
		printf("Can't open the input file!\n");
		exit(1);
	}
	if ((foutput = fopen("foutput.txt", "w")) == NULL)
        {
		printf("Can't open the output file!\n");
		exit(1);
	}

	redirectInput(fin);
	init();
    yyparse();
	if(err == 0)
	{
		printf("\n===Parsing success!===\n");
		fprintf(foutput, "\n===Parsing success!===\n");
	}
        else
	{
		printf("%d errors in PL/0 program\n", err);
		fprintf(foutput, "%d errors in PL/0 program\n", err);
	}
        fclose(foutput);
	fclose(fin);
	return 0;
}



