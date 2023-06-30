#ifndef CPP_SOLUTION_EXPRESSION_H
#define CPP_SOLUTION_EXPRESSION_H

#include <string>
#include <map>

class expression {
public:

    virtual std::string infix_form() = 0;

    virtual bool is_impl() { return false; };

    virtual expression *get_left() { return nullptr; }

    virtual expression *get_right() { return nullptr; }

    virtual ~expression() = default;
};

class variable : public expression {
private:
    std::string _name;
public:
    explicit variable(std::string &name) :
            _name(name) {
    }

    std::string infix_form() override {
        return _name;
    }
};

class disjunction : public expression {
private:
    expression *_left;
    expression *_right;

public:
    disjunction(expression *left, expression *right) :
            _left(left),
            _right(right) {
    }

    std::string infix_form() override {
        return "(" + _left->infix_form() + " | " + _right->infix_form() + ")";
    }
};

class conjunction : public expression {
private:
    expression *_left;
    expression *_right;

public:
    conjunction(expression *left, expression *right) :
            _left(left),
            _right(right) {
    }

    std::string infix_form() override {
        return "(" + _left->infix_form() + " & " + _right->infix_form() + ")";
    }
};

class negation : public expression {
private:
    expression *_expr;

public:
    explicit negation(expression *expr) :
            _expr(expr) {
    }

    std::string infix_form() override {
        return "(!" + _expr->infix_form() + ")";
    }
};

class implication : public expression {
private:
    expression *_left;
    expression *_right;

public:
    implication(expression *left, expression *right) :
            _left(left),
            _right(right) {
    }

    bool is_impl() override { return true; }

    expression *get_left() override { return this->_left; }

    expression *get_right() override { return this->_right; }

    std::string infix_form() override {
        return "(" + _left->infix_form() + " -> " + _right->infix_form() + ")";
    }
};

#endif //CPP_SOLUTION_EXPRESSION_H
