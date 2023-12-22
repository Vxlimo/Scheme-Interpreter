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

/* let expression */
Value Let::eval(Assoc& env) { }

/* lambda expression */
Value Lambda::eval(Assoc& env)
{
    return ClosureV(x, e, env);
}

/* for function calling */
Value Apply::eval(Assoc& e) { }

/* letrec expression */
Value Letrec::eval(Assoc& env) { }

/* evaluation of variable */
Value Var::eval(Assoc& e)
{
    Value v = find(x, e);
    if (v.get() != nullptr)
        return v;
    else
        throw RuntimeError(x + ": undefined.");
}

/* evaluation of a fixnum */
Value Fixnum::eval(Assoc& e)
{
    return IntegerV(n);
}

/* if expression */
Value If::eval(Assoc& e)
{
    Assoc e1 = e, e2 = e;
    Boolean* bool1 = dynamic_cast<Boolean*>(cond->eval(e1).get());
    if (bool1 != nullptr && bool1->b == true)
        return conseq->eval(e2);
    else
        return alter->eval(e2);
}

/* evaluation of #t */
Value True::eval(Assoc& e)
{
    return BooleanV(true);
}

/* evaluation of #f */
Value False::eval(Assoc& e)
{
    return BooleanV(false);
}

/* begin expression */
Value Begin::eval(Assoc& e)
{
    Value v = NullV();
    for (Expr expr : es) {
        Assoc e1 = e;
        v = expr->eval(e1);
    }
    return v;
}

/* quote expression */
Value Quote::eval(Assoc& e) { }

/* (void) */
Value MakeVoid::eval(Assoc& e)
{
    return VoidV();
}

/* (exit) */
Value Exit::eval(Assoc& e)
{
    exit(0);
    return;
}

/* evaluation of two-operators primitive */
Value Binary::eval(Assoc& e)
{
    Assoc e1 = e;
    return evalRator(rand1->eval(e1), rand2->eval(e1));
}

/* evaluation of single-operator primitive */
Value Unary::eval(Assoc& e)
{
    Assoc e1 = e;
    return evalRator(rand->eval(e1));
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
    return BooleanV(rand->v_type == V_VOID);
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
