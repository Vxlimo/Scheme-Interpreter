#ifndef PARSER
#define PARSER

// parser of myscheme

#include "Def.hpp"
#include "RE.hpp"
#include "expr.hpp"
#include "syntax.hpp"
#include <cstring>
#include <iostream>
#include <map>
#define mp make_pair
using std ::pair;
using std ::string;
using std ::vector;

extern std ::map<std ::string, ExprType> primitives;
extern std ::map<std ::string, ExprType> reserved_words;

/* fixnum, ex: 5 */
Expr Number ::parse(Assoc& env)
{
    return Expr(new Fixnum(this->n));
}

/* variable, ex: aa */
Expr Identifier ::parse(Assoc& env)
{
    return Expr(new Var(this->s));
}

/* true, ex: #t */
Expr TrueSyntax ::parse(Assoc& env)
{
    return Expr(new True());
}

/* false, ex: #f */
Expr FalseSyntax ::parse(Assoc& env)
{
    return Expr(new False());
}

/* list, ex: (balabala) */
Expr List ::parse(Assoc& env)
{
    if (stxs.empty()) { // empty list, ex: quote ()
        return Expr(new Quote(new List()));
    }
    Expr func = stxs[0].parse(env);

    switch (func->e_type) {

    /* let, ex: (let ([var expr]*) expr) */
    case E_LET: {
        if (stxs.size() != 3)
            throw RuntimeError("let: wrong number of args.");
        /* get var list */
        List* vars = dynamic_cast<List*>(stxs[1].get());
        if (vars == nullptr)
            throw RuntimeError("let: args[1] is not a list.");
        /* the body is based on new assoc env1 */
        Assoc env1 = env;
        /* try to build the var list */
        std::vector<std::pair<std::string, Expr>> bind;
        for (Syntax stx : vars->stxs) {
            List* assign = dynamic_cast<List*>(stx.get());
            if (assign == nullptr)
                throw RuntimeError("let: args[2] have something not a list.");
            Identifier* name = dynamic_cast<Identifier*>(assign->stxs[0].get());
            // TODO: what if var is not defined?
            if (name == nullptr)
                throw RuntimeError("let: args[2] have some var name invalid.");
            bind.push_back(std::make_pair(name->s, assign->stxs[1].parse(env)));
        }

        Expr body = stxs[2].parse(env1);
        return Expr(new Let(bind, body));
    }

    /* lambda, ex: (lambda (var*) expr) */
    case E_LAMBDA: {
    }

    /* never appeared? */
    case E_APPLY: {
        break;
    }

    /* letrec, ex: (letrec ([var expr]*) expr) */
    case E_LETREC: {
    }

    /* never appeared */
    case E_VAR: {
        break;
    }

    /* never appeared */
    case E_FIXNUM: {
        break;
    }

    /* if, ex: (if expr expr expr) */
    case E_IF: {
        if (stxs.size() != 4)
            throw RuntimeError("if: wrong number of args.");
        return Expr(new If(stxs[1].parse(env), stxs[2].parse(env), stxs[3].parse(env)));
    }

    /* never appeared */
    case E_TRUE: {
        break;
    }

    /* never appeared */
    case E_FALSE: {
        break;
    }

    /* begin, ex: (begin expr*) */
    case E_BEGIN: {
    }

    /* quote, ex: (quote datum) */
    case E_QUOTE: {
    }

    /* void, ex: (void) */
    case E_VOID: {
        if (stxs.size() != 1)
            throw RuntimeError("void: wrong number of args.");
        return Expr(new MakeVoid());
    }

    /* mul, ex: (* a b) */
    case E_MUL: {
        if (stxs.size() != 3)
            throw RuntimeError("*: wrong number of args.");
        return Expr(new Mult(stxs[1].parse(env), stxs[2].parse(env)));
    }

    /* plus, ex: (+ a b) */
    case E_PLUS: {
        if (stxs.size() != 3)
            throw RuntimeError("+: wrong number of args.");
        return Expr(new Plus(stxs[1].parse(env), stxs[2].parse(env)));
    }

    /* minus, ex: (- a b) */
    case E_MINUS: {
        if (stxs.size() != 3)
            throw RuntimeError("-: wrong number of args.");
        return Expr(new Minus(stxs[1].parse(env), stxs[2].parse(env)));
    }

    /* <, ex: (< a b) */
    case E_LT: {
        if (stxs.size() != 3)
            throw RuntimeError("<: wrong number of args.");
        return Expr(new Less(stxs[1].parse(env), stxs[2].parse(env)));
    }

    /* <=, ex: (<= a b) */
    case E_LE: {
        if (stxs.size() != 3)
            throw RuntimeError("<=: wrong number of args.");
        return Expr(new LessEq(stxs[1].parse(env), stxs[2].parse(env)));
    }

    /* =, ex: (= a b) */
    case E_EQ: {
        if (stxs.size() != 3)
            throw RuntimeError("=: wrong number of args.");
        return Expr(new Equal(stxs[1].parse(env), stxs[2].parse(env)));
    }

    /* >=, ex: (>= a b) */
    case E_GE: {
        if (stxs.size() != 3)
            throw RuntimeError(">=: wrong number of args.");
        return Expr(new GreaterEq(stxs[1].parse(env), stxs[2].parse(env)));
    }

    /* >, ex: (> a b) */
    case E_GT: {
        if (stxs.size() != 3)
            throw RuntimeError(">: wrong number of args.");
        return Expr(new Greater(stxs[1].parse(env), stxs[2].parse(env)));
    }

    /* cons, ex: (cons a b) */
    case E_CONS: {
    }

    /* not, ex: (not expr) */
    case E_NOT: {
    }

    /* car, ex: (car (a.b)) */
    case E_CAR: {
    }

    /* cdr, ex: (cdr (a.b)) */
    case E_CDR: {
    }

    /* eq?, ex: (eq? a b) */
    case E_EQQ: {
        if (stxs.size() != 3)
            throw RuntimeError("eq?: wrong number of args.");
        return Expr(new IsEq(stxs[1].parse(env), stxs[2].parse(env)));
    }

    /* boolean?, ex: (boolean? a) */
    case E_BOOLQ: {
        if (stxs.size() != 2)
            throw RuntimeError("boolean?: wrong number of args.");
        return Expr(new IsBoolean(stxs[1].parse(env)));
    }

    /* finnum?, ex: (finnum? a) */
    case E_INTQ: {
        if (stxs.size() != 2)
            throw RuntimeError("finnum?: wrong number of args.");
        return Expr(new IsFixnum(stxs[1].parse(env)));
    }

    /* null?, ex: (null? a) */
    case E_NULLQ: {
        if (stxs.size() != 2)
            throw RuntimeError("null?: wrong number of args.");
        return Expr(new IsNull(stxs[1].parse(env)));
    }

    /* pair?, ex: (pair? a) */
    case E_PAIRQ: {
        if (stxs.size() != 2)
            throw RuntimeError("pair?: wrong number of args.");
        return Expr(new IsPair(stxs[1].parse(env)));
    }

    /* procedure?, ex: (procedure? a) */
    case E_PROCQ: {
        if (stxs.size() != 2)
            throw RuntimeError("procedure?: wrong number of args.");
        return Expr(new IsProcedure(stxs[1].parse(env)));
    }

    /* symbol?, ex: (symbol? a) */
    case E_SYMBOLQ: {
        if (stxs.size() != 2)
            throw RuntimeError("symbol?: wrong number of args.");
        return Expr(new IsSymbol(stxs[1].parse(env)));
    }

    /* exit ,ex: (exit) */
    case E_EXIT: {
        if (stxs.size() != 1)
            throw RuntimeError("exit: wrong number of args.");
        return Expr(new Exit());
    }

    default: {
        break;
    }
    }
}

#endif