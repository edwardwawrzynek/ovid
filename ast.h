#include <iostream>
#include <vector>

namespace ast {
    class Expression;
    class Statement;

    typedef std::vector<Expression *> ExpressionList;
    typedef std::vector<Statement *> StatementList;

    /* base ast node */
    class Node {
    public:
        virtual ~Node() {}
    };

    /* ast statements */
    class Statement: public Node {};

    class Block: public Statement {
        StatementList& statments;

        explicit Block(StatementList& statements): statments(statements) {};
    };


    /* ast expressions */
    class Expression: public Statement {};

    class FunctionCall: public Expression {
    public:
        Expression& func_expr;
        ExpressionList& args;

        FunctionCall(Expression& func_expr, ExpressionList& args): func_expr(func_expr), args(args) {};
    };

    class Identifier: public Expression {
    public:
        std::string id;
        Identifier(std::string& id): id(id) {};
    };

    class Assignment: public Expression {
    public:
        Expression& lvalue;
        Expression& rvalue;
        Assignment(Expression& lvalue, Expression& rvalue): lvalue(lvalue), rvalue(rvalue) {};
    };

    class Literal: public Expression {};

    class IntLiteral: public Literal {
    public:
        const long value;
        IntLiteral(const long value): value(value) {};
    };
}