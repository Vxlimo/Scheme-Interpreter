#include "Def.hpp"
#include "RE.hpp"
#include "expr.hpp"
#include "syntax.hpp"
#include "value.hpp"
#include <cstring>
#include <map>
#include <vector>

extern std ::map<std ::string, ExprType> primitives;
extern std ::map<std ::string, ExprType> reserved_words;

/* function with out list */
Value GetType::eval(Assoc& env)
{
    throw RuntimeError("syntax error.");
    return Value(nullptr);
}

/* let expression */
Value Let::eval(Assoc& env)
{
    /* pre-calculate all value */
    std::vector<Value> vs;
    for (auto expr : bind) {
        Assoc env1 = env;
        vs.push_back(expr.second->eval(env1));
    }

    /* assignment */
    Assoc env1 = env;
    for (int i = 0; i < bind.size(); i++) {
        auto expr = bind[i];
        env1 = extend(expr.first, vs[i], env1);
    }

    return body->eval(env1);
}

/* lambda expression */
Value Lambda::eval(Assoc& env)
{
    return ClosureV(x, e, env);
}

/* for function calling */
Value Apply::eval(Assoc& env)
{
    /* find closure */
    Assoc env1 = env;
    Value rator_eval = rator->eval(env1);
    Closure* closure = dynamic_cast<Closure*>(rator_eval.get());
    if (closure == nullptr)
        throw RuntimeError("apply: type error.");
    if (closure->parameters.size() != rand.size())
        throw RuntimeError("apply: wrong number of args.");

    /* pre-calculate parameters */
    std::vector<Value> vs;
    for (auto expr : rand) {
        Assoc env2 = env;
        vs.push_back(expr->eval(env2));
    }

    /* apply the closure */
    Assoc env2 = closure->env;
    for (int i = 0; i < closure->parameters.size(); i++)
        env2 = extend(closure->parameters[i], vs[i], env2);
    return closure->e->eval(env2);
}

/* letrec expression */
Value Letrec::eval(Assoc& env)
{
    /* add definition */
    Assoc env1 = env;
    for (auto expr : bind)
        env1 = extend(expr.first, Value(NothingV()), env1);

    /* pre-calculate all value */
    std::vector<Value> vs;
    for (auto expr : bind) {
        Assoc env2 = env1;
        vs.push_back(expr.second->eval(env2));
    }

    /* assignment */
    Assoc env2 = env1;
    for (int i = 0; i < bind.size(); i++) {
        auto expr = bind[i];
        modify(expr.first, vs[i], env2);
    }

    return body->eval(env2);
}

/* evaluation of variable */
Value Var::eval(Assoc& env)
{
    Value v = find(x, env);
    if (v.get() != nullptr)
        return v;
    else
        throw RuntimeError(x + ": undefined.");
}

/* evaluation of a fixnum */
Value Fixnum::eval(Assoc& env)
{
    return IntegerV(n);
}

/* if expression */
Value If::eval(Assoc& env)
{
    Assoc env1 = env, env2 = env;
    Value cond_eval = cond->eval(env1);
    Boolean* bool1 = dynamic_cast<Boolean*>(cond_eval.get());
    if (bool1 == nullptr || bool1->b == true)
        return conseq->eval(env2);
    else
        return alter->eval(env2);
}

/* evaluation of #t */
Value True::eval(Assoc& env)
{
    return BooleanV(true);
}

/* evaluation of #f */
Value False::eval(Assoc& env)
{
    return BooleanV(false);
}

/* begin expression */
Value Begin::eval(Assoc& env)
{
    Value v = NothingV();
    for (Expr expr : es) {
        Assoc env1 = env;
        v = expr->eval(env1);
    }
    return v;
}

/* quote expression */
Value Quote_List(std::vector<Syntax>&, int, Assoc&);
Value Quote_Singlevalue(Syntax, Assoc&);

Value Quote_List(std::vector<Syntax>& stxs, int pos, Assoc& env)
{
    if (pos == stxs.size())
        return NullV();
    return PairV(Quote_Singlevalue(stxs[pos], env), Quote_List(stxs, pos + 1, env));
}
Value Quote_Singlevalue(Syntax s, Assoc& env)
{
    /* a list need to be reconstructed in to pair */
    List* list = dynamic_cast<List*>(s.get());
    if (list != nullptr)
        return Quote_List(list->stxs, 0, env);

    /* otherwise, output directly */
    Number* int1 = dynamic_cast<Number*>(s.get());
    if (int1 != nullptr)
        return IntegerV(int1->n);
    TrueSyntax* bool1 = dynamic_cast<TrueSyntax*>(s.get());
    if (bool1 != nullptr)
        return BooleanV(true);
    FalseSyntax* bool2 = dynamic_cast<FalseSyntax*>(s.get());
    if (bool2 != nullptr)
        return BooleanV(false);
    Identifier* id = dynamic_cast<Identifier*>(s.get());
    if (id != nullptr)
        return SymbolV(id->s);

    throw RuntimeError("quote: type error.");
    return Value(nullptr);
}
Value Quote::eval(Assoc& env)
{
    Assoc env1 = env;
    return Quote_Singlevalue(s, env1);
}

/* (void) */
Value MakeVoid::eval(Assoc& env)
{
    return VoidV();
}

/* (exit) */
Value Exit::eval(Assoc& env)
{
    exit(0);
    return Value(nullptr);
}

/* evaluation of two-operators primitive */
Value Binary::eval(Assoc& env)
{
    Assoc env1 = env;
    return evalRator(rand1->eval(env1), rand2->eval(env1));
}

/* evaluation of single-operator primitive */
Value Unary::eval(Assoc& env)
{
    Assoc env1 = env;
    return evalRator(rand->eval(env1));
}

/* * */
Value Mult::evalRator(const Value& rand1, const Value& rand2)
{
    /* type check */
    Integer* int1 = dynamic_cast<Integer*>(rand1.get());
    Integer* int2 = dynamic_cast<Integer*>(rand2.get());
    if (int1 == nullptr || int2 == nullptr)
        throw RuntimeError("*: type error.");

    return IntegerV(int1->n * int2->n);
}

/* + */
Value Plus::evalRator(const Value& rand1, const Value& rand2)
{
    /* type check */
    Integer* int1 = dynamic_cast<Integer*>(rand1.get());
    Integer* int2 = dynamic_cast<Integer*>(rand2.get());
    if (int1 == nullptr || int2 == nullptr)
        throw RuntimeError("+: type error.");

    return IntegerV(int1->n + int2->n);
}

/* - */
Value Minus::evalRator(const Value& rand1, const Value& rand2)
{
    /* type check */
    Integer* int1 = dynamic_cast<Integer*>(rand1.get());
    Integer* int2 = dynamic_cast<Integer*>(rand2.get());
    if (int1 == nullptr || int2 == nullptr)
        throw RuntimeError("-: type error.");

    return IntegerV(int1->n - int2->n);
}

/* < */
Value Less::evalRator(const Value& rand1, const Value& rand2)
{
    /* type check */
    Integer* int1 = dynamic_cast<Integer*>(rand1.get());
    Integer* int2 = dynamic_cast<Integer*>(rand2.get());
    if (int1 == nullptr || int2 == nullptr)
        throw RuntimeError("<: type error.");

    return BooleanV(int1->n < int2->n);
}

/* <= */
Value LessEq::evalRator(const Value& rand1, const Value& rand2)
{
    /* type check */
    Integer* int1 = dynamic_cast<Integer*>(rand1.get());
    Integer* int2 = dynamic_cast<Integer*>(rand2.get());
    if (int1 == nullptr || int2 == nullptr)
        throw RuntimeError("<=: type error.");

    return BooleanV(int1->n <= int2->n);
}

/* = */
Value Equal::evalRator(const Value& rand1, const Value& rand2)
{
    /* type check */
    Integer* int1 = dynamic_cast<Integer*>(rand1.get());
    Integer* int2 = dynamic_cast<Integer*>(rand2.get());
    if (int1 == nullptr || int2 == nullptr)
        throw RuntimeError("=: type error.");

    return BooleanV(int1->n == int2->n);
}

/* >= */
Value GreaterEq::evalRator(const Value& rand1, const Value& rand2)
{
    /* type check */
    Integer* int1 = dynamic_cast<Integer*>(rand1.get());
    Integer* int2 = dynamic_cast<Integer*>(rand2.get());
    if (int1 == nullptr || int2 == nullptr)
        throw RuntimeError(">=: type error.");

    return BooleanV(int1->n >= int2->n);
}

/* > */
Value Greater::evalRator(const Value& rand1, const Value& rand2)
{
    /* type check */
    Integer* int1 = dynamic_cast<Integer*>(rand1.get());
    Integer* int2 = dynamic_cast<Integer*>(rand2.get());
    if (int1 == nullptr || int2 == nullptr)
        throw RuntimeError(">: type error.");

    return BooleanV(int1->n > int2->n);
}

/* eq? */
Value IsEq::evalRator(const Value& rand1, const Value& rand2)
{
    /* type check */
    if (rand1->v_type != rand2->v_type)
        return BooleanV(rand1.get() == rand2.get());

    /* classification discussion */
    else {
        switch (rand1->v_type) {
        case V_INT:
            return BooleanV(dynamic_cast<Integer*>(rand1.get())->n == dynamic_cast<Integer*>(rand2.get())->n);
        case V_BOOL:
            return BooleanV(dynamic_cast<Boolean*>(rand1.get())->b == dynamic_cast<Boolean*>(rand2.get())->b);
        case V_SYM:
            return BooleanV(dynamic_cast<Symbol*>(rand1.get())->s == dynamic_cast<Symbol*>(rand2.get())->s);
        default:
            return BooleanV(rand1.get() == rand2.get());
        }
    }
}

/* cons */
Value Cons::evalRator(const Value& rand1, const Value& rand2)
{
    return PairV(rand1, rand2);
}

/* boolean? */
Value IsBoolean::evalRator(const Value& rand)
{
    return BooleanV(rand->v_type == V_BOOL);
}

/* fixnum? */
Value IsFixnum::evalRator(const Value& rand)
{
    return BooleanV(rand->v_type == V_INT);
}

/* symbol? */
Value IsSymbol::evalRator(const Value& rand)
{
    return BooleanV(rand->v_type == V_SYM);
}

/* null? */
Value IsNull::evalRator(const Value& rand)
{
    return BooleanV(rand->v_type == V_NULL);
}

/* pair? */
Value IsPair::evalRator(const Value& rand)
{
    return BooleanV(rand->v_type == V_PAIR);
}

/* procedure? */
Value IsProcedure::evalRator(const Value& rand)
{
    return BooleanV(rand->v_type == V_PROC);
}

/* not */
Value Not::evalRator(const Value& rand)
{
    Boolean* bool1 = dynamic_cast<Boolean*>(rand.get());
    if (bool1 != nullptr && bool1->b == false)
        return BooleanV(true);
    else
        return BooleanV(false);
}

/* car */
Value Car::evalRator(const Value& rand)
{
    /* type check */
    Pair* pair1 = dynamic_cast<Pair*>(rand.get());
    if (pair1 == nullptr)
        throw RuntimeError("car: type error.");

    return pair1->car;
}

/* cdr */
Value Cdr::evalRator(const Value& rand)
{
    /* type check */
    Pair* pair1 = dynamic_cast<Pair*>(rand.get());
    if (pair1 == nullptr)
        throw RuntimeError("cdr: type error.");

    return pair1->cdr;
}
