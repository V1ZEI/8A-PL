/* A Bison parser, made by GNU Bison 3.5.1.  */

/* Bison interface for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015, 2018-2020 Free Software Foundation,
   Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* Undocumented macros, especially those whose name start with YY_,
   are private implementation details.  Do not rely on them.  */

#ifndef YY_YY_Y_TAB_H_INCLUDED
# define YY_YY_Y_TAB_H_INCLUDED
/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif
#if YYDEBUG
extern int yydebug;
#endif

/* Token type.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    KEY = 258,
    AND = 259,
    OR = 260,
    NOT = 261,
    RELOP = 262,
    ID = 263,
    VAL = 264,
    SC = 265,
    CM = 266,
    PL = 267,
    MI = 268,
    MUL = 269,
    DIV = 270,
    POW = 271,
    OP = 272,
    CL = 273,
    AO = 274,
    AC = 275,
    EQ = 276,
    BO = 277,
    BC = 278,
    UNR = 279,
    TRUE = 280,
    FALSE = 281,
    IF = 282,
    ELSE = 283,
    WHILE = 284
  };
#endif
/* Tokens.  */
#define KEY 258
#define AND 259
#define OR 260
#define NOT 261
#define RELOP 262
#define ID 263
#define VAL 264
#define SC 265
#define CM 266
#define PL 267
#define MI 268
#define MUL 269
#define DIV 270
#define POW 271
#define OP 272
#define CL 273
#define AO 274
#define AC 275
#define EQ 276
#define BO 277
#define BC 278
#define UNR 279
#define TRUE 280
#define FALSE 281
#define IF 282
#define ELSE 283
#define WHILE 284

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
union YYSTYPE
{
#line 68 "6.y"

	typedef struct node NODE;
	string *str;
	NODE *node;


#line 122 "y.tab.h"

};
typedef union YYSTYPE YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE yylval;

int yyparse (void);

#endif /* !YY_YY_Y_TAB_H_INCLUDED  */
