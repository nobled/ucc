#include "ucl.h"
#include "ast.h"
#include "expr.h"

/**
 * This module parses the C expression.
 */

/**
 * The token's corresponding unary or binary opearator.
 * Some tokens represent both unary and binary operator,
 * e.g. * or +.
 */
static struct tokenOp TokenOps[] = 
{
#define TOKENOP(tok, bop, uop) {bop, uop},
#include "tokenop.h"
#undef  TOKENOP
};

// operators' precedence
static int Prec[] =
{
#define OPINFO(op, prec, name, func, opcode) prec,
#include "opinfo.h"
	0
#undef OPINFO
};

// expression for integer constant 0
AstExpression Constant0;

// operator names, mainly used for error reporting
char *OPNames[] = 
{
#define OPINFO(op, prec, name, func, opcode) name,
#include "opinfo.h"
	NULL
#undef OPINFO
};

/**
 *  primary-expression:
 *		ID
 *		constant
 *		string-literal
 *		( expression )
 */
static AstExpression ParsePrimaryExpression(void)
{
	AstExpression expr;


	switch (CurrentToken)
	{
	case TK_ID:

		CREATE_AST_NODE(expr, Expression);

		expr->op = OP_ID;
		expr->val = TokenValue;
		NEXT_TOKEN;

		return expr;

	/// Notice: Only when parsing constant and string literal,
	/// ty member in astExpression is used since from OP_CONST
	/// and OP_STR alone the expression's type can't be determined
	case TK_INTCONST:
	case TK_UINTCONST:
	case TK_LONGCONST:
	case TK_ULONGCONST:
	case TK_LLONGCONST:
	case TK_ULLONGCONST:
	case TK_FLOATCONST:
	case TK_DOUBLECONST:
	case TK_LDOUBLECONST:

		CREATE_AST_NODE(expr, Expression);

		if (CurrentToken >= TK_FLOATCONST)
			CurrentToken++;

		/// nasty, requires that both from TK_INTCONST to TK_LDOUBLECONST
		/// and from INT to LDOUBLE are consecutive
		expr->ty = T(INT + CurrentToken - TK_INTCONST);
		expr->op = OP_CONST;
		expr->val = TokenValue;
		NEXT_TOKEN;

		return expr;

	case TK_STRING:
	case TK_WIDESTRING:

		CREATE_AST_NODE(expr, Expression);

		expr->ty = ArrayOf(((String)TokenValue.p)->len + 1, CurrentToken == TK_STRING ? T(CHAR) : WCharType);
		expr->op = OP_STR;
		expr->val = TokenValue;
		NEXT_TOKEN;

		return expr;

	case TK_LPAREN:

		NEXT_TOKEN;
		expr = ParseExpression();
		Expect(TK_RPAREN);

		return expr;

	default:
		Error(&TokenCoord, "Expect identifier, string, constant or (");
		return Constant0;
	}
}

/**
 *  postfix-expression:
 *		primary-expression
 *		postfix-expression [ expression ]
 *		postfix-expression ( [argument-expression-list] )
 *		postfix-expression . identifier
 *		postfix-expression -> identifier
 *		postfix-expression ++
 *		postfix-expression --
 */
static AstExpression ParsePostfixExpression(void)
{
	AstExpression expr, p;

	expr = ParsePrimaryExpression();

	while (1)
	{
		switch (CurrentToken)
		{
		case TK_LBRACKET:

			CREATE_AST_NODE(p, Expression);

			p->op = OP_INDEX;
			p->kids[0] = expr;
			NEXT_TOKEN;
			p->kids[1] = ParseExpression();
			Expect(TK_RBRACKET);

			expr = p;
			break;

		case TK_LPAREN:

			CREATE_AST_NODE(p, Expression);

			p->op = OP_CALL;
			p->kids[0] = expr;
			NEXT_TOKEN;
			if (CurrentToken != TK_RPAREN)
			{
				AstNode *tail;

				/// function call expression's second kid is actually
				/// a list of expression instead of a single expression
				p->kids[1] = ParseAssignmentExpression();
				tail = &p->kids[1]->next;
				while (CurrentToken == TK_COMMA)
				{
					NEXT_TOKEN;
					*tail = (AstNode)ParseAssignmentExpression();
					tail = &(*tail)->next;
				}
			}
			Expect(TK_RPAREN);

			expr = p;
			break;

		case TK_DOT:
		case TK_POINTER:

			CREATE_AST_NODE(p, Expression);

			p->op = (CurrentToken == TK_DOT ? OP_MEMBER : OP_PTR_MEMBER);
			p->kids[0] = expr;
			NEXT_TOKEN;
			if (CurrentToken != TK_ID)
			{
				Error(&p->coord, "Expect identifier as struct or union member");
			}
			else
			{
				p->val = TokenValue;
				NEXT_TOKEN;
			}

			expr = p;
			break;

		case TK_INC:
		case TK_DEC:

			CREATE_AST_NODE(p, Expression);

			p->op = (CurrentToken == TK_INC) ? OP_POSTINC : OP_POSTDEC;
			p->kids[0] = expr;
			NEXT_TOKEN;

			expr = p;
			break;

		default:

			return expr;
		}
	}
}

/**
 *  unary-expression:
 *		postfix-expression
 *		unary-operator unary-expression
 *		( type-name ) unary-expression
 *		sizeof unary-expression
 *		sizeof ( type-name )
 *
 *  unary-operator:
 *		++ -- & * + - ! ~
 */
static AstExpression ParseUnaryExpression()
{
	AstExpression expr;
	int t;

	switch (CurrentToken)
	{
	case TK_INC:
	case TK_DEC:
	case TK_BITAND:
	case TK_MUL:
	case TK_ADD:
	case TK_SUB:
	case TK_NOT:
	case TK_COMP:

		CREATE_AST_NODE(expr, Expression);

		expr->op = UNARY_OP;
		NEXT_TOKEN;
		expr->kids[0] = ParseUnaryExpression();

		return expr;

	case TK_LPAREN:

		/// When current token is (, it may be a type cast expression
		/// or a primary expression, we need to look ahead one token,
		/// if next token is type name, the expression is treated as
		/// a type cast expression; otherwise a primary expresion
		BeginPeekToken();
		t = GetNextToken();
		if (IsTypeName(t))
		{
			EndPeekToken();

			CREATE_AST_NODE(expr, Expression);

			expr->op = OP_CAST;
			NEXT_TOKEN;
			expr->kids[0] =  (AstExpression)ParseTypeName();
			Expect(TK_RPAREN);
			expr->kids[1] = ParseUnaryExpression();

			return expr;
		}
		else
		{
			EndPeekToken();
			return ParsePostfixExpression();
		}
		break;

	case TK_SIZEOF:

		/// this case hase the same issue with TK_LPAREN case
		CREATE_AST_NODE(expr, Expression);

		expr->op = OP_SIZEOF;
		NEXT_TOKEN;
		if (CurrentToken == TK_LPAREN)
		{
			BeginPeekToken();
			t = GetNextToken();
			if (IsTypeName(t))
			{
				EndPeekToken();

				NEXT_TOKEN;
				/// In this case, the first kid is not an expression,
				/// but thanks to both type name and expression have a 
				/// kind member to discriminate them.
				expr->kids[0] = (AstExpression)ParseTypeName();
				Expect(TK_RPAREN);
			}
			else
			{
				EndPeekToken();
				expr->kids[0] = ParseUnaryExpression();
			}
		}
		else
		{
			expr->kids[0] = ParseUnaryExpression();
		}

		return expr;

	default:
		return ParsePostfixExpression();
	}
}

/**
 * Parse a binary expression, from logical-OR-expresssion to multiplicative-expression
 */
static AstExpression ParseBinaryExpression(int prec)
{
	AstExpression binExpr;
	AstExpression expr;
	int newPrec;

	expr = ParseUnaryExpression();
	/// while the following binary operater's precedence is higher than current
	/// binary operator's precedence, parses a higer precedence expression
	while (IsBinaryOP(CurrentToken) && (newPrec = Prec[BINARY_OP]) >= prec)
	{
		CREATE_AST_NODE(binExpr, Expression);

		binExpr->op = BINARY_OP;
		binExpr->kids[0] = expr;
		NEXT_TOKEN;
		binExpr->kids[1] = ParseBinaryExpression(newPrec + 1);

		expr = binExpr;
	}

	return expr;

}

/**
 *  conditional-expression:
 *      logical-OR-expression
 *      logical-OR-expression ? expression : conditional-expression
 */
static AstExpression ParseConditionalExpression(void)
{
	AstExpression expr;

	expr = ParseBinaryExpression(Prec[OP_OR]);
	if (CurrentToken == TK_QUESTION)
	{
		AstExpression condExpr;

		CREATE_AST_NODE(condExpr, Expression);

		condExpr->op = OP_QUESTION;
		condExpr->kids[0] = expr;
		NEXT_TOKEN;

		CREATE_AST_NODE(condExpr->kids[1], Expression);

		condExpr->kids[1]->op = OP_COLON;
		condExpr->kids[1]->kids[0] = ParseExpression();
		Expect(TK_COLON);
		condExpr->kids[1]->kids[1] = ParseConditionalExpression();

		return condExpr;
	}

	return expr;
}

/**
 *  assignment-expression:
 *      conditional-expression
 *      unary-expression assignment-operator assignment-expression
 *  assignment-operator:
 *      = *= /= %= += -= <<= >>= &= ^= |=
 *  There is a little twist here: the parser always treats the first nonterminal
 *  as a conditional expression.
 */
AstExpression ParseAssignmentExpression(void)
{
	AstExpression expr;

	expr = ParseConditionalExpression();
	if (CurrentToken >= TK_ASSIGN && CurrentToken <= TK_MOD_ASSIGN)
	{
		AstExpression asgnExpr;

		CREATE_AST_NODE(asgnExpr, Expression);

		asgnExpr->op = BINARY_OP;
		asgnExpr->kids[0] = expr;
		NEXT_TOKEN;
		asgnExpr->kids[1] = ParseAssignmentExpression();

		return asgnExpr;
	}

	return expr;
}

/**
 *  expression:
 *      assignment-expression
 *      expression , assignment-expression
 */
AstExpression ParseExpression(void)
{
	AstExpression expr, comaExpr;

	expr = ParseAssignmentExpression();
	while(CurrentToken == TK_COMMA)
	{
		CREATE_AST_NODE(comaExpr, Expression);

		comaExpr->op = OP_COMMA;
		comaExpr->kids[0] = expr;
		NEXT_TOKEN;
		comaExpr->kids[1] = ParseAssignmentExpression();

		expr = comaExpr;
	}

	return expr;
}

/**
 * Parse constant expression which is actually a conditional expression
 */ 
AstExpression ParseConstantExpression(void)
{
	return ParseConditionalExpression();
}


