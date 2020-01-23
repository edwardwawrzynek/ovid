#include <iostream>
#include <utility>
#include <vector>
#include <memory>

namespace ast {
    class Expression;
    class Statement;

    typedef std::vector<std::unique_ptr<Expression>> ExpressionList;
    typedef std::vector<std::unique_ptr<Statement>> StatementList;

    /* base ast node */
    class Node {
    public:
        virtual ~Node() {}
    };

    /* ast statements */
    class Statement: public Node {};

    class Block: public Statement {
        StatementList statments;

        explicit Block(StatementList statements): statments(std::move(statements)) {};
    };


    /* ast expressions */
    class Expression: public Statement {};

    class FunctionCall: public Expression {
    public:
        std::unique_ptr<Expression> func_expr;
        ExpressionList args;

        FunctionCall(std::unique_ptr<Expression> func_expr, ExpressionList args): func_expr(std::move(func_expr)), args(std::move(args)) {};
    };

    class Identifier: public Expression {
    public:
        std::string id;
        Identifier(std::string& id): id(id) {};
    };

    class Assignment: public Expression {
    public:
        std::unique_ptr<Expression> lvalue;
        std::unique_ptr<Expression> rvalue;
        Assignment(std::unique_ptr<Expression> lvalue, std::unique_ptr<Expression> rvalue): lvalue(std::move(lvalue)), rvalue(std::move(rvalue)) {};
    };

    class Literal: public Expression {};

    class IntLiteral: public Literal {
    public:
        const long value;
        IntLiteral(const long value): value(value) {};
    };
}