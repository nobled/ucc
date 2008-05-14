#include "ucl.h"
#include "ast.h"
#include "decl.h"
#include "expr.h"
#include "stmt.h"

#define PushStatement(v, stmt)   INSERT_ITEM(v, stmt)
#define PopStatement(v)          (v->data[--v->len])
#define TopStatement(v)          TOP_ITEM(v)

static AstStatement CheckStatement(AstStatement stmt);

static Label TryAddLabel(char *id)
{
	Label p = CURRENTF->labels;

	while (p)
	{
		if (p->id == id)
			return p;
		p = p->next;
	}

	CALLOC(p);

	p->id = id;
	p->next = CURRENTF->labels;
	CURRENTF->labels = p;

	return p;
}

static void AddCase(AstSwitchStatement swtchStmt, AstCaseStatement caseStmt)
{
	AstCaseStatement p = swtchStmt->cases;
	AstCaseStatement *pprev = &swtchStmt->cases;
	int diff;

	while (p)
	{
		diff = caseStmt->expr->val.i[0] - p->expr->val.i[0];
		if (diff < 0)
			break;

		if (diff > 0)
		{
			pprev = &p->nextCase;
			p = p->nextCase;
		}
		else
		{
			Error(&caseStmt->coord, "Repeated constant in a switch statement");
			return;
		}
	}

	*pprev = caseStmt;
	caseStmt->nextCase = p;
}

static AstStatement CheckExpressionStatement(AstStatement stmt)
{
	AstExpressionStatement exprStmt = AsExpr(stmt);

	if (exprStmt->expr != NULL)
	{
		exprStmt->expr = CheckExpression(exprStmt->expr);
	}

	return stmt;
}

static AstStatement CheckLabelStatement(AstStatement stmt)
{
	AstLabelStatement labelStmt = AsLabel(stmt);

	labelStmt->label = TryAddLabel(labelStmt->id);
	if (labelStmt->label->defined)
	{
		Error(&stmt->coord, "Label name should be unique within function.");
	}
	labelStmt->label->defined = 1;
	labelStmt->label->coord = stmt->coord;
	labelStmt->stmt = CheckStatement(labelStmt->stmt);

	return stmt;
}

static AstStatement CheckCaseStatement(AstStatement stmt)
{
	AstCaseStatement caseStmt = AsCase(stmt);
	AstSwitchStatement swtchStmt;

	swtchStmt = (AstSwitchStatement)TopStatement(CURRENTF->swtches);
	if (swtchStmt == NULL)
	{
		Error(&stmt->coord, "A case label shall appear in a switch statement.");
		return stmt;
	}

	caseStmt->expr = CheckConstantExpression(caseStmt->expr);
	if (caseStmt->expr == NULL)
	{
		Error(&stmt->coord, "The case value must be integer constant.");
		return stmt;
	}

	caseStmt->stmt = CheckStatement(caseStmt->stmt);
	caseStmt->expr = FoldCast(swtchStmt->expr->ty, caseStmt->expr);
	AddCase(swtchStmt, caseStmt);

	return stmt;
}

static AstStatement CheckDefaultStatement(AstStatement stmt)
{
	AstDefaultStatement defStmt = AsDef(stmt);
	AstSwitchStatement swtchStmt;

	swtchStmt = (AstSwitchStatement)TopStatement(CURRENTF->swtches);
	if (swtchStmt == NULL)
	{
		Error(&stmt->coord, "A default label shall appear in a switch statement.");
		return stmt;
	}
	if (swtchStmt->defStmt != NULL)
	{
		Error(&stmt->coord, "There shall be only one default label in a switch statement.");
		return stmt;
	}

	defStmt->stmt = CheckStatement(defStmt->stmt);
	swtchStmt->defStmt = defStmt;

	return stmt;
}

static AstStatement CheckIfStatement(AstStatement stmt)
{
	AstIfStatement ifStmt = AsIf(stmt);

	ifStmt->expr = Adjust(CheckExpression(ifStmt->expr), 1);
	if (! IsScalarType(ifStmt->expr->ty))
	{
		Error(&stmt->coord, "The expression in if statement shall be scalar type.");
	}

	ifStmt->thenStmt = CheckStatement(ifStmt->thenStmt);
	if (ifStmt->elseStmt)
	{
		ifStmt->elseStmt = CheckStatement(ifStmt->elseStmt);
	}

	return stmt;
}

static AstStatement CheckSwitchStatement(AstStatement stmt)
{
	AstSwitchStatement swtchStmt = AsSwitch(stmt);

	PushStatement(CURRENTF->swtches,   stmt);
	PushStatement(CURRENTF->breakable, stmt);

	swtchStmt->expr = Adjust(CheckExpression(swtchStmt->expr), 1);
	if (! IsIntegType(swtchStmt->expr->ty))
	{
		Error(&stmt->coord, "The expression in a switch statement shall be integer type.");
		swtchStmt->expr->ty = T(INT);
	}
	if (swtchStmt->expr->ty->categ < INT)
	{                               
		swtchStmt->expr = Cast(T(INT), swtchStmt->expr);
	}
	swtchStmt->stmt = CheckStatement(swtchStmt->stmt);

	PopStatement(CURRENTF->swtches);
	PopStatement(CURRENTF->breakable);

	return stmt;
}

static AstStatement CheckLoopStatement(AstStatement stmt)
{
	AstLoopStatement loopStmt = AsLoop(stmt);

	PushStatement(CURRENTF->loops,    stmt);
	PushStatement(CURRENTF->breakable, stmt);

	loopStmt->expr = Adjust(CheckExpression(loopStmt->expr), 1);
	if (! IsScalarType(loopStmt->expr->ty))
	{
		Error(&stmt->coord, "The expression in do or while statement shall be scalar type.");
	}
	loopStmt->stmt = CheckStatement(loopStmt->stmt);
	
	PopStatement(CURRENTF->loops);
	PopStatement(CURRENTF->breakable);

	return stmt;
}

static AstStatement CheckForStatement(AstStatement stmt)
{
	AstForStatement forStmt = AsFor(stmt);

	PushStatement(CURRENTF->loops,     stmt);
	PushStatement(CURRENTF->breakable, stmt);

	if (forStmt->initExpr)
	{
		forStmt->initExpr = CheckExpression(forStmt->initExpr);
	}
	if (forStmt->expr)
	{
		forStmt->expr = Adjust(CheckExpression(forStmt->expr), 1);
		if (! IsScalarType(forStmt->expr->ty))
		{
			Error(&stmt->coord, "The second expression in for statement shall be scalar type.");
		}
	}
	if (forStmt->incrExpr)
	{
		forStmt->incrExpr = CheckExpression(forStmt->incrExpr);
	}
	forStmt->stmt = CheckStatement(forStmt->stmt);

	PopStatement(CURRENTF->loops);
	PopStatement(CURRENTF->breakable);

	return stmt;
}

static AstStatement CheckGotoStatement(AstStatement stmt)
{
	AstGotoStatement gotoStmt = AsGoto(stmt);

	if (gotoStmt->id != NULL)
	{
		gotoStmt->label = TryAddLabel(gotoStmt->id);
		gotoStmt->label->coord = gotoStmt->coord;
		gotoStmt->label->ref++;
	}

	return stmt;
}

static AstStatement CheckBreakStatement(AstStatement stmt)
{
	AstBreakStatement brkStmt = AsBreak(stmt);

	brkStmt->target = TopStatement(CURRENTF->breakable);
	if (brkStmt->target == NULL)
	{
		Error(&stmt->coord, "The break shall appear in a switch or loop");
	}

	return stmt;
}

static AstStatement CheckContinueStatement(AstStatement stmt)
{
	AstContinueStatement contStmt = AsCont(stmt);

	contStmt->target = (AstLoopStatement)TopStatement(CURRENTF->loops);
	if (contStmt->target == NULL)
	{
		Error(&stmt->coord, "The continue shall appear in a loop.");
	}

	return stmt;
}

static AstStatement CheckReturnStatement(AstStatement stmt)
{
	AstReturnStatement retStmt = AsRet(stmt);
	Type rty = FSYM->ty->bty;

	CURRENTF->hasReturn = 1;
	if (retStmt->expr)
	{
		retStmt->expr = Adjust(CheckExpression(retStmt->expr), 1);

		if (rty->categ == VOID)
		{
			Error(&stmt->coord, "Void function should not return value");
			return stmt;
		}

		if (! CanAssign(rty, retStmt->expr))
		{
			Error(&stmt->coord, "Incompatible return value");
			return stmt;
		}

		retStmt->expr = Cast(rty, retStmt->expr);

		return stmt;
	}

	if (rty->categ != VOID)
	{
		Warning(&stmt->coord, "The function should return a value.");
	}
	return stmt;
}

static AstStatement CheckLocalCompound(AstStatement stmt)
{
	AstStatement s;

	EnterScope();

	s = CheckCompoundStatement(stmt);

	ExitScope();

	return stmt;
}

static AstStatement (* StmtCheckers[])(AstStatement) = 
{
	CheckExpressionStatement,
	CheckLabelStatement,
	CheckCaseStatement,
	CheckDefaultStatement,
	CheckIfStatement,
	CheckSwitchStatement,
	CheckLoopStatement,
	CheckLoopStatement,
	CheckForStatement,
	CheckGotoStatement,
	CheckBreakStatement,
	CheckContinueStatement,
	CheckReturnStatement,
	CheckLocalCompound
};

static AstStatement CheckStatement(AstStatement stmt)
{
	return (* StmtCheckers[stmt->kind - NK_ExpressionStatement])(stmt);
}

AstStatement CheckCompoundStatement(AstStatement stmt)
{
	AstCompoundStatement compStmt = AsComp(stmt);
	AstNode p;

	compStmt->ilocals = CreateVector(1);
	p = compStmt->decls;
	while (p)
	{
		CheckLocalDeclaration((AstDeclaration)p, compStmt->ilocals);
		p = p->next;
	}
	p = compStmt->stmts;
	while (p)
	{
		CheckStatement((AstStatement)p);
		p = p->next;
	}

	return stmt;
}
