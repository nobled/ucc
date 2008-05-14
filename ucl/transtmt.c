#include "ucl.h"
#include "ast.h"
#include "stmt.h"
#include "expr.h"
#include "decl.h"
#include "gen.h"

static void TranslateStatement(AstStatement stmt);

static int HasSideEffect(AstExpression expr)
{
	if (expr == NULL)
		return 0;

	if (expr->op == OP_CALL    || expr->op >= OP_ASSIGN && expr->op <= OP_MOD_ASSIGN ||
	    expr->op == OP_PREINC  || expr->op == OP_PREDEC ||
	    expr->op == OP_POSTINC || expr->op == OP_POSTDEC)
		return 1;

	return HasSideEffect(expr->kids[0]) || HasSideEffect(expr->kids[1]);
}

static void TranslateExpressionStatement(AstStatement stmt)
{
	AstExpressionStatement exprStmt = AsExpr(stmt);

	if (exprStmt->expr != NULL)
	{
		TranslateExpression(exprStmt->expr);
	}
}

static void TranslateLabelStatement(AstStatement stmt)
{
	AstLabelStatement labelStmt = AsLabel(stmt);

	if (labelStmt->label->ref > 0)
	{
		if (labelStmt->label->respBB == NULL)
		{
			labelStmt->label->respBB = CreateBBlock();
		}
		StartBBlock(labelStmt->label->respBB);
	}
	TranslateStatement(labelStmt->stmt);
}

static void TranslateCaseStatement(AstStatement stmt)
{
	AstCaseStatement caseStmt = AsCase(stmt);

	StartBBlock(caseStmt->respBB);
	TranslateStatement(caseStmt->stmt);
}

static void TranslateDefaultStatement(AstStatement stmt)
{
	AstDefaultStatement defStmt = AsDef(stmt);

	StartBBlock(defStmt->respBB);
	TranslateStatement(defStmt->stmt);
}

static void TranslateIfStatement(AstStatement stmt)
{
	AstIfStatement ifStmt = AsIf(stmt);
	BBlock nextBB;
	BBlock trueBB;
	BBlock falseBB;

	nextBB = CreateBBlock();
	trueBB = CreateBBlock();

	if (ifStmt->elseStmt == NULL)
	{
		TranslateBranch(Not(ifStmt->expr), nextBB, trueBB);

		StartBBlock(trueBB);
		TranslateStatement(ifStmt->thenStmt);
	}
	else
	{
		falseBB = CreateBBlock();

		TranslateBranch(Not(ifStmt->expr), falseBB, trueBB);

		StartBBlock(trueBB);
		TranslateStatement(ifStmt->thenStmt);
		GenerateJump(nextBB);

		StartBBlock(falseBB);
		TranslateStatement(ifStmt->elseStmt);
	}

	StartBBlock(nextBB);
}

static void TranslateWhileStatement(AstStatement stmt)
{
	AstLoopStatement whileStmt = AsLoop(stmt);

	whileStmt->loopBB = CreateBBlock();
	whileStmt->contBB = CreateBBlock();
	whileStmt->nextBB = CreateBBlock();

	GenerateJump(whileStmt->contBB);

	StartBBlock(whileStmt->loopBB);
	TranslateStatement(whileStmt->stmt);

	StartBBlock(whileStmt->contBB);
	TranslateBranch(whileStmt->expr, whileStmt->loopBB, whileStmt->nextBB);

	StartBBlock(whileStmt->nextBB);
}

static void TranslateDoStatement(AstStatement stmt)
{
	AstLoopStatement doStmt = AsLoop(stmt);

	doStmt->loopBB = CreateBBlock();
	doStmt->contBB = CreateBBlock();
	doStmt->nextBB = CreateBBlock();

	StartBBlock(doStmt->loopBB);
	TranslateStatement(doStmt->stmt);

	StartBBlock(doStmt->contBB);
	TranslateBranch(doStmt->expr, doStmt->loopBB, doStmt->nextBB);

	StartBBlock(doStmt->nextBB);
}

static void TranslateForStatement(AstStatement stmt)
{
	AstForStatement forStmt = AsFor(stmt);

	forStmt->loopBB = CreateBBlock();
	forStmt->contBB = CreateBBlock();
	forStmt->testBB = CreateBBlock();
	forStmt->nextBB = CreateBBlock();

	if (forStmt->initExpr)
	{
		TranslateExpression(forStmt->initExpr);
	}
	GenerateJump(forStmt->testBB);

	StartBBlock(forStmt->loopBB);
	TranslateStatement(forStmt->stmt);

	StartBBlock(forStmt->contBB);
	if (forStmt->incrExpr)
	{
		TranslateExpression(forStmt->incrExpr);
	}

	StartBBlock(forStmt->testBB);
	if (forStmt->expr)
	{
		TranslateBranch(forStmt->expr, forStmt->loopBB, forStmt->nextBB);
	}
	else
	{
		GenerateJump(forStmt->loopBB);
	}

	StartBBlock(forStmt->nextBB);
}


static void TranslateGotoStatement(AstStatement stmt)
{
	AstGotoStatement gotoStmt = AsGoto(stmt);

	if (gotoStmt->label->respBB == NULL)
	{
		gotoStmt->label->respBB = CreateBBlock();
	}
	GenerateJump(gotoStmt->label->respBB);
}

static void TranslateBreakStatement(AstStatement stmt)
{
	AstBreakStatement brkStmt = AsBreak(stmt);

	if (brkStmt->target->kind == NK_SwitchStatement)
	{
		GenerateJump(AsSwitch(brkStmt->target)->nextBB);
	}
	else
	{
		GenerateJump(AsLoop(brkStmt->target)->nextBB);
	}
}

static void TranslateContinueStatement(AstStatement stmt)
{
	AstContinueStatement contStmt = AsCont(stmt);

	GenerateJump(contStmt->target->contBB);
}

static void TranslateReturnStatement(AstStatement stmt)
{
	AstReturnStatement retStmt = AsRet(stmt);

	if (retStmt->expr)
	{
		GenerateReturn(retStmt->expr->ty, TranslateExpression(retStmt->expr));
	}
	GenerateJump(FSYM->exitBB);
	StartBBlock(CreateBBlock());
}

static int MergeSwitchBucket(SwitchBucket *pBucket)
{
	SwitchBucket bucket = *pBucket;
	int count = 0;

	while (bucket->prev)
	{
		if ((bucket->prev->ncase + bucket->ncase + 1) * 2 <= (bucket->maxVal - bucket->prev->minVal))
			break;

		bucket->prev->ncase += bucket->ncase;
		bucket->prev->maxVal = bucket->maxVal;
		*bucket->prev->tail = bucket->cases;
		bucket->prev->tail = bucket->tail;
		bucket= bucket->prev;
		count++;
	}

	*pBucket = bucket;
	return count;
}

static void TranslateSwitchBuckets(SwitchBucket *bucketArray, int left, int right, 
                                   Symbol choice, BBlock currBB, BBlock defBB)
{
	int mid, len, i;
	AstCaseStatement p;
	BBlock lhalfBB, rhalfBB;
	BBlock *dstBBs;
	Symbol index;

	if (left > right)
		return;

	mid = (left + right) / 2;
	lhalfBB = (left > mid - 1) ?  defBB : CreateBBlock();
	rhalfBB = (mid + 1 > right) ? defBB : CreateBBlock();

	len = bucketArray[mid]->maxVal - bucketArray[mid]->minVal + 1;

	dstBBs = HeapAllocate(CurrentHeap, (len + 1)* sizeof(BBlock));
	for (i = 0; i < len; ++i)
		dstBBs[i] = defBB;
	dstBBs[len] = NULL;

	p = bucketArray[mid]->cases;
	while (p)
	{
		i = p->expr->val.i[0] - bucketArray[mid]->minVal;
		dstBBs[i] = p->respBB;
		p = p->nextCase;
	}

	if (currBB != NULL)
	{
		StartBBlock(currBB);
	}
	GenerateBranch(choice->ty, lhalfBB, JL, choice, IntConstant(bucketArray[mid]->minVal));

	StartBBlock(CreateBBlock());
	GenerateBranch(choice->ty, rhalfBB, JG, choice, IntConstant(bucketArray[mid]->maxVal));

	StartBBlock(CreateBBlock());

	if (len != 1)
	{
		index = CreateTemp(choice->ty);
		GenerateAssign(choice->ty, index, SUB, choice, IntConstant(bucketArray[mid]->minVal));
		GenerateIndirectJump(dstBBs, len, index);
	}
	else
	{
		GenerateJump(dstBBs[0]);
	}

	TranslateSwitchBuckets(bucketArray, left, mid - 1, choice, lhalfBB, defBB);
	TranslateSwitchBuckets(bucketArray, mid + 1, right, choice, rhalfBB, defBB);

}

static void TranslateSwitchStatement(AstStatement stmt)
{
	AstSwitchStatement swtchStmt = AsSwitch(stmt);
	AstCaseStatement p, q;
	SwitchBucket bucket, b;
	SwitchBucket *bucketArray;
	int i, val;
	Symbol sym;

	sym = TranslateExpression(swtchStmt->expr);

	bucket = b = NULL;
	p = swtchStmt->cases;
	while (p)
	{
		q = p;
		p = p->nextCase;

		q->respBB = CreateBBlock();
		val = q->expr->val.i[0];
		if (bucket && (bucket->ncase + 1) * 2 > (val - bucket->minVal))
		{
			bucket->ncase++;
			bucket->maxVal = val;
			*bucket->tail = q;
			bucket->tail = &(q->nextCase);
			swtchStmt->nbucket -= MergeSwitchBucket(&bucket);
		}
		else
		{
			ALLOC(b);

			b->cases = q;
			b->ncase = 1;
			b->minVal = b->maxVal = val;
			b->tail = &(q->nextCase);
			b->prev = bucket;
			bucket = b;
			swtchStmt->nbucket++;
		}
	}
	swtchStmt->buckets = bucket;

	bucketArray = HeapAllocate(CurrentHeap, swtchStmt->nbucket * sizeof(SwitchBucket));

	for (i = swtchStmt->nbucket - 1; i >= 0; i--)
	{
		bucketArray[i] = bucket;
		*bucket->tail = NULL;
		bucket = bucket->prev;
	}

	swtchStmt->defBB = CreateBBlock();
	if (swtchStmt->defStmt)
	{
		swtchStmt->defStmt->respBB = swtchStmt->defBB;
		swtchStmt->nextBB = CreateBBlock();
	}
	else
	{
		swtchStmt->nextBB = swtchStmt->defBB;
	}
	TranslateSwitchBuckets(bucketArray, 0, swtchStmt->nbucket - 1, sym, NULL, swtchStmt->defBB);
	TranslateStatement(swtchStmt->stmt);
	StartBBlock(swtchStmt->nextBB);
}

static void TranslateCompoundStatement(AstStatement stmt)
{
	AstCompoundStatement compStmt = AsComp(stmt);
	AstNode p = compStmt->stmts;
	Vector ilocals = compStmt->ilocals;
	Symbol v;
	int i;

	for (i = 0; i < LEN(ilocals); ++i)
	{
		InitData initd;
		Type ty;
		Symbol dst, src;
		int size;

		v = GET_ITEM(ilocals, i);
		initd = AsVar(v)->idata;
		size = 0;
		while (initd != NULL)
		{
			if (initd->offset != size)
			{
				dst = CreateOffset(T(UCHAR), v, size);
				GenerateClear(dst, initd->offset - size);
			}
			ty = initd->expr->ty;
			if (initd->expr->op == OP_STR)
			{
				String str = initd->expr->val.p;

				src = AddString(ArrayOf(str->len + 1, ty->bty), str);
			}
			else
			{
				src = TranslateExpression(initd->expr);
			}
			dst = CreateOffset(ty, v, initd->offset);
			GenerateMove(ty, dst, src);

			size = initd->offset + ty->size;
			initd = initd->next;
		}
		if (size < v->ty->size)
		{
			dst = CreateOffset(T(UCHAR), v, size);
			GenerateClear(dst, v->ty->size - size);
		}
	}

	while (p)
	{
		TranslateStatement((AstStatement)p);
		p = p->next;
	}

}

static void (* StmtTrans[])(AstStatement) = 
{
	TranslateExpressionStatement,
	TranslateLabelStatement,
	TranslateCaseStatement,
	TranslateDefaultStatement,
	TranslateIfStatement,
	TranslateSwitchStatement,
	TranslateWhileStatement,
	TranslateDoStatement,
	TranslateForStatement,
	TranslateGotoStatement,
	TranslateBreakStatement,
	TranslateContinueStatement,
	TranslateReturnStatement,
	TranslateCompoundStatement
};

static void TranslateStatement(AstStatement stmt)
{
	(* StmtTrans[stmt->kind - NK_ExpressionStatement])(stmt);
}

static void TranslateFunction(AstFunction func)
{
	BBlock bb;

	FSYM = func->fsym;
	if (! FSYM->defined)
		return;

	TempNum = 0;
	FSYM->entryBB = CreateBBlock();
	FSYM->exitBB = CreateBBlock();

	CurrentBB = FSYM->entryBB;
	TranslateStatement(func->stmt);
	StartBBlock(FSYM->exitBB);

	Optimize(FSYM);
	bb = FSYM->entryBB;
	while (bb != NULL)
	{
		if (bb != FSYM->entryBB && bb->ref != 0)
		{
			bb->sym = CreateLabel();
		}
		bb = bb->next;
	}
	
}

void Translate(AstTranslationUnit transUnit)
{
	AstNode p = transUnit->extDecls;

	while (p)
	{
		if (p->kind == NK_Function && ((AstFunction)p)->stmt)
		{
			TranslateFunction((AstFunction)p);
		}
		p = p->next;
	}
}
