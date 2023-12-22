#ifndef PARSER
#define PARSER

// parser of myscheme

#include "Def.hpp"
#include "RE.hpp"
#include "expr.hpp"
#include "syntax.hpp"
#include "value.hpp"
#include <cstring>
#include <iostream>
#include <map>
#define mp make_pair
using std ::pair;
using std ::string;
using std ::vector;

extern std ::map<std ::string, ExprType> primitives;
extern std ::map<std ::string, ExprType> reserved_words;

Expr Syntax::parse(Assoc& env)
{
    return ptr->parse(env);
}

/* fixnum, ex: 5 */
Expr Number ::parse(Assoc& env)
{
    return Expr(new Fixnum(n));
}

/* variable, ex: aa */
Expr Identifier ::parse(Assoc& env)
{
    /* check whether it is a function or a variable */
    Value v = find(s, env);
    if (v.get() != nullptr)
        return Expr(new Var(s));

    /* GetType used for get the type of operation
     * idea from Wang Yuxuan */
    if (primitives.find(s) != primitives.end())
        return Expr(new GetType(primitives[s]));
    if (reserved_words.find(this->s) != reserved_words.end())
        return Expr(new GetType(reserved_words[s]));

    /* a new function or variable */
    return Expr(new Var(s));
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
    /* empty list, ex: quote () */
    if (stxs.empty()) {
        return Expr(new Quote(new List()));
    }

    Expr func = stxs[0].parse(env);
    /* func is E_... if and only if it is Exprbase */
    if (typeid(*func) != typeid(GetType)) {
        /* check whether the args is matched */
        Lambda* lambda = dynamic_cast<Lambda*>(func.get());
        if (stxs.size() != lambda->x.size() + 1)
            throw RuntimeError("wrong number of args.");

        Expr rator = func;
        std::vector<Expr> rand;
        for (int i = 1; i < stxs.size(); i++) {
            Assoc env1 = env;
            rand.push_back(stxs[i].parse(env1));
        }
        return Expr(new Apply(rator, rand));
    }

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
            if (name == nullptr)
                throw RuntimeError("let: args[2] have some var name invalid.");
            env1 = extend(name->s, Value(nullptr), env1);
            bind.push_back(std::make_pair(name->s, assign->stxs[1].parse(env)));
        }

        Expr body = stxs[2].parse(env1);
        return Expr(new Let(bind, body));
    }

    /* lambda, ex: (lambda (var*) expr) */
    case E_LAMBDA: {
        if (stxs.size() != 3)
            throw RuntimeError("let: wrong number of args.");

        /* get var list */
        List* vars = dynamic_cast<List*>(stxs[1].get());
        if (vars == nullptr)
            throw RuntimeError("let: args[1] is not a list.");

        /* the body is based on new assoc env1 */
        Assoc env1 = env;

        /* try to build the var list */
        std::vector<std::string> x;
        for (Syntax stx : vars->stxs) {
            Identifier* name = dynamic_cast<Identifier*>(stx.get());
            if (name == nullptr)
                throw RuntimeError("let: args[2] have some var name invalid.");
            env1 = extend(name->s, Value(nullptr), env1);
            x.push_back(name->s);
        }

        Expr body = stxs[2].parse(env1);
        return Expr(new Lambda(x, body));
    }

    /* never appeared */
    case E_APPLY: {
        goto RE;
        break;
    }

    /* letrec, ex: (letrec ([var expr]*) expr) */
    case E_LETREC: {
        if (stxs.size() != 3)
            throw RuntimeError("let: wrong number of args.");

        /* get var list */
        List* vars = dynamic_cast<List*>(stxs[1].get());
        if (vars == nullptr)
            throw RuntimeError("let: args[1] is not a list.");

        /* the body is based on new assoc env1, this time add vars first */
        Assoc env1 = env;

        /* try to build the var list */
        std::vector<std::pair<std::string, Expr>> bind;
        for (Syntax stx : vars->stxs) {
            List* assign = dynamic_cast<List*>(stx.get());
            if (assign == nullptr)
                throw RuntimeError("let: args[2] have something not a list.");
            Identifier* name = dynamic_cast<Identifier*>(assign->stxs[0].get());
            if (name == nullptr)
                throw RuntimeError("let: args[2] have some var name invalid.");
            env1 = extend(name->s, Value(nullptr), env1);
            bind.push_back(std::make_pair(name->s, Expr(nullptr)));
        }

        /* the values are based on env1(with vars) */
        for (int i = 0; i < bind.size(); i++) {
            Syntax stx = vars->stxs[i];
            List* assign = dynamic_cast<List*>(stx.get());
            bind[i].second = assign->stxs[1].parse(env1);
        }

        Expr body = stxs[2].parse(env1);
        return Expr(new Letrec(bind, body));
    }

    /* never appeared */
    case E_VAR: {
        goto RE;
        break;
    }

    /* never appeared */
    case E_FIXNUM: {
        goto RE;
        break;
    }

    /* if, ex: (if expr expr expr) */
    case E_IF: {
        if (stxs.size() != 4)
            throw RuntimeError("if: wrong number of args.");
        Assoc env1 = env, env2 = env, env3 = env;
        return Expr(new If(stxs[1].parse(env1), stxs[2].parse(env2), stxs[3].parse(env3)));
    }

    /* never appeared */
    case E_TRUE: {
        goto RE;
        break;
    }

    /* never appeared */
    case E_FALSE: {
        goto RE;
        break;
    }

    /* begin, ex: (begin expr*) */
    case E_BEGIN: {
        if (stxs.size() < 2)
            throw RuntimeError("begin: wrong number of args.");

        std::vector<Expr> exprs;
        for (int i = 1; i < stxs.size(); i++) {
            Assoc env1 = env;
            exprs.push_back(stxs[i].parse(env1));
        }
        return Expr(new Begin(exprs));
    }

    /* quote, ex: (quote datum) */
    case E_QUOTE: {
        if (stxs.size() != 2)
            throw RuntimeError("quote: wrong number of args.");

        return Expr(new Quote(stxs[1]));
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

        Assoc env1 = env, env2 = env;
        return Expr(new Mult(stxs[1].parse(env1), stxs[2].parse(env2)));
    }

    /* plus, ex: (+ a b) */
    case E_PLUS: {
        if (stxs.size() != 3)
            throw RuntimeError("+: wrong number of args.");

        Assoc env1 = env, env2 = env;
        return Expr(new Plus(stxs[1].parse(env1), stxs[2].parse(env2)));
    }

    /* minus, ex: (- a b) */
    case E_MINUS: {
        if (stxs.size() != 3)
            throw RuntimeError("-: wrong number of args.");

        Assoc env1 = env, env2 = env;
        return Expr(new Minus(stxs[1].parse(env1), stxs[2].parse(env2)));
    }

    /* <, ex: (< a b) */
    case E_LT: {
        if (stxs.size() != 3)
            throw RuntimeError("<: wrong number of args.");

        Assoc env1 = env, env2 = env;
        return Expr(new Less(stxs[1].parse(env1), stxs[2].parse(env2)));
    }

    /* <=, ex: (<= a b) */
    case E_LE: {
        if (stxs.size() != 3)
            throw RuntimeError("<=: wrong number of args.");

        Assoc env1 = env, env2 = env;
        return Expr(new LessEq(stxs[1].parse(env1), stxs[2].parse(env2)));
    }

    /* =, ex: (= a b) */
    case E_EQ: {
        if (stxs.size() != 3)
            throw RuntimeError("=: wrong number of args.");

        Assoc env1 = env, env2 = env;
        return Expr(new Equal(stxs[1].parse(env1), stxs[2].parse(env2)));
    }

    /* >=, ex: (>= a b) */
    case E_GE: {
        if (stxs.size() != 3)
            throw RuntimeError(">=: wrong number of args.");

        Assoc env1 = env, env2 = env;
        return Expr(new GreaterEq(stxs[1].parse(env1), stxs[2].parse(env2)));
    }

    /* >, ex: (> a b) */
    case E_GT: {
        if (stxs.size() != 3)
            throw RuntimeError(">: wrong number of args.");

        Assoc env1 = env, env2 = env;
        return Expr(new Greater(stxs[1].parse(env1), stxs[2].parse(env2)));
    }

    /* cons, ex: (cons a b) */
    case E_CONS: {
        if (stxs.size() != 3)
            throw RuntimeError("cons: wrong number of args.");

        Assoc env1 = env, env2 = env;
        return Expr(new Cons(stxs[1].parse(env1), stxs[2].parse(env2)));
    }

    /* not, ex: (not expr) */
    case E_NOT: {
        if (stxs.size() != 2)
            throw RuntimeError("not: wrong number of args.");

        Assoc env1 = env;
        return Expr(new Not(stxs[1].parse(env1)));
    }

    /* car, ex: (car (a.b)) */
    case E_CAR: {
        if (stxs.size() != 2)
            throw RuntimeError("car: wrong number of args.");

        Assoc env1 = env;
        return Expr(new Car(stxs[1].parse(env1)));
    }

    /* cdr, ex: (cdr (a.b)) */
    case E_CDR: {
        if (stxs.size() != 2)
            throw RuntimeError("cdr: wrong number of args.");

        Assoc env1 = env;
        return Expr(new Cdr(stxs[1].parse(env1)));
    }

    /* eq?, ex: (eq? a b) */
    case E_EQQ: {
        if (stxs.size() != 3)
            throw RuntimeError("eq?: wrong number of args.");

        Assoc env1 = env, env2 = env;
        return Expr(new IsEq(stxs[1].parse(env1), stxs[2].parse(env2)));
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
    RE: // TODO: delete this goto (just for test)
        throw RuntimeError("unknown syntax.");
        break;
    }
    }
}

#endif