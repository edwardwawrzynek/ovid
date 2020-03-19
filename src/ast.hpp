#ifndef H_AST_INCL
#define H_AST_INCL

#include <iostream>
#include <utility>
#include <vector>
#include <memory>

namespace ovid::ast {
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
        class Statement : public Node {
        };

        class Block : public Statement {
            StatementList statements;

            explicit Block(StatementList statements) : statements(std::move(statements)) {};
        };


        /* ast expressions */
        class Expression : public Statement {
        };

        class FunctionCall : public Expression {
        public:
            std::unique_ptr<Expression> func_expr;
            ExpressionList args;

            FunctionCall(std::unique_ptr<Expression> func_expr, ExpressionList args) : func_expr(std::move(func_expr)),
                                                                                       args(std::move(args)) {};
        };

        class Identifier : public Expression {
        public:
            std::string id;

            Identifier(std::string &id) : id(id) {};
        };

        class Assignment : public Expression {
        public:
            std::unique_ptr<Expression> lvalue;
            std::unique_ptr<Expression> rvalue;

            Assignment(std::unique_ptr<Expression> lvalue, std::unique_ptr<Expression> rvalue) : lvalue(
                    std::move(lvalue)), rvalue(std::move(rvalue)) {};
        };

        class Literal : public Expression {
        };

        class IntLiteral : public Literal {
        public:
            const long value;

            IntLiteral(const long value) : value(value) {};
        };

        class Tuple: public Expression {
        public:
            ExpressionList expressions;

            explicit Tuple(ExpressionList expressions): expressions(std::move(expressions)) {};
        };
    }

#endif