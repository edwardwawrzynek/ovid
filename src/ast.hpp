#ifndef H_AST_INCL
#define H_AST_INCL

#include <iostream>
#include <utility>
#include <vector>
#include <memory>
#include "tokenizer.hpp"

namespace ovid::ast {
        class Expression;

        class Statement;

        typedef std::vector<std::unique_ptr<Expression>> ExpressionList;
        typedef std::vector<std::unique_ptr<Statement>> StatementList;

        /* ast types */
        class Type {
        public:
            virtual ~Type() {};

            virtual Type* withoutmutability();
        };

        class BoolType: public Type {
        public:
            BoolType() {};
        };

        class IntType: public Type {
        public:
            int size; // in bits
            bool isUnsigned;

            IntType(int size, bool isUnsigned): size(size), isUnsigned(isUnsigned) {};
        };

        class FloatType: public Type {
        public:
            int size; // in bits
            FloatType(int size): size(size) {};
        };

        class MutType: public Type {
        public:
            std::unique_ptr<Type> type;

            MutType(std::unique_ptr<Type> type): type(std::move(type)) {};
            Type* withoutmutability() override;
        };

        class FunctionType: public Type {
        public:
            std::vector<std::unique_ptr<Type>> argTypes;
            std::unique_ptr<Type> retType;

            FunctionType(std::vector<std::unique_ptr<Type>> argTypes, std::unique_ptr<Type> retType): argTypes(std::move(argTypes)), retType(std::move(retType)) {};
        };

        class FunctionPrototype {
        public:
            std::unique_ptr<FunctionType> type;
            std::vector<std::string> argNames;
            std::string name;

            FunctionPrototype(std::unique_ptr<FunctionType> type, std::vector<std::string> argNames, std::string& name): type(std::move(type)), argNames(std::move(argNames)), name(name) {};
        };

        /* base ast node */
        class Node {
        public:
            virtual ~Node() {};
        };

        /* ast statements */
        class Statement : public Node {
        };

        class VarDecl: public Statement {
        public:
            std::string name;
            std::unique_ptr<Expression> initialValue;

            VarDecl(std::string& name, std::unique_ptr<Expression> initialValue): name(name), initialValue(std::move(initialValue)) {};
        };

        class FunctionDecl: public Statement {
        public:
            std::unique_ptr<FunctionPrototype> proto;
            StatementList body;

            FunctionDecl(std::unique_ptr<FunctionPrototype> proto, StatementList body): proto(std::move(proto)), body(std::move(body)) {};

        };

        class ModuleDecl: public Statement {
        public:
            std::string name;

            explicit ModuleDecl(std::string& name): name(name) {};
        };

        /* ast expressions */
        class Expression : public Statement {
        };

        class FunctionCall : public Expression {
        public:
            std::unique_ptr<Expression> funcExpr;
            ExpressionList args;

            FunctionCall(std::unique_ptr<Expression> funcExpr, ExpressionList args) : funcExpr(std::move(funcExpr)),
                                                                                      args(std::move(args)) {};
        };

        class Identifier : public Expression {
        public:
            std::string id;

            Identifier(std::string &id) : id(id) {};
        };

        class OperatorSymbol : public Expression {
        public:
            TokenType op;

            OperatorSymbol(TokenType op): op(op) {};
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