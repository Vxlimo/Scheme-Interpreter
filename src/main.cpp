#include "Def.hpp"
#include "RE.hpp"
#include "expr.hpp"
#include "syntax.hpp"
#include "value.hpp"
#include <iostream>
#include <map>
#include <sstream>

extern std ::map<std ::string, ExprType> primitives;
extern std ::map<std ::string, ExprType> reserved_words;

void REPL()
{
    // read - evaluation - print loop
    Assoc global_env = empty();
    while (1) {
        printf("scm> ");
        Syntax stx = readSyntax(std ::cin); // read
        try {
            Expr expr = stx->parse(global_env); // parse
            // stx->show(std ::cerr); // syntax print
            Value val = expr->eval(global_env);
            if (val->v_type == V_TERMINATE)
                break;
            val->show(std ::cout); // value print
        } catch (const RuntimeError& RE) {
            // std ::cout << RE.message();
            std ::cout << "RuntimeError";
        }
        puts("");
    }
}

int main(int argc, char* argv[])
{
    initPrimitives();
    initReservedWords();
    REPL();
    return 0;
}
