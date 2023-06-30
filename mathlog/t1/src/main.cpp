#include <iostream>
#include "expression.h"
#include "parser/expression.tab.h"
#include "parser/expression.lexer.h"
#include "handlers.hpp"
#include <map>
#include <vector>
#include <set>
#include "util.hpp"

extern expression *current;

void yyerror(const char *error) {
    std::cerr << error;
}

int yywrap() {
    return 1;
}

int main() {
    std::string input;
    std::skipws(std::cin);
    getline(std::cin, input);
    std::vector<std::string> head = split(input, "|-");

    yy_scan_string(head[1].c_str());
    yyparse();
    std::string result = current->infix_form();

    std::vector<std::string> context = split(head[0], ",");

    yy_scan_string(context[context.size() - 1].c_str());
    yyparse();
    std::string hyp_s = current->infix_form();
    const char *fin_hyp_s = hyp_s.c_str();
    context.pop_back();

    std::string header;
    size_t n = context.size();

    for (size_t i = 0; i < n; i++) {
        header += context[i];
        if (i != n - 1) header += ", ";
    }

    header += "|- " + hyp_s + " -> " + result + "\n";
    std::cout << header;

    std::set<std::string> proofset;
    std::map<std::string, std::vector<std::string>> impls;

    while (getline(std::cin, input)) {
        yy_scan_string(input.c_str());
        yyparse();

        std::string curr_proof_s = current->infix_form();
        if (proofset.find(curr_proof_s) == proofset.end()) {

            if (!handle_alpha(curr_proof_s, hyp_s, fin_hyp_s)) {
                if (!handle_mp(impls, proofset, curr_proof_s, fin_hyp_s)) {
                    handle_axiom(fin_hyp_s, curr_proof_s.c_str());
                }
            }
            std::printf("%s -> %s\n", fin_hyp_s, curr_proof_s.c_str());


            bool curr_is_impl = current->is_impl();

            if (curr_is_impl) {
                std::string left_s = current->get_left()->infix_form();
                std::string right_s = current->get_right()->infix_form();

                impls[right_s].push_back(left_s);
            }
        } else {
            std::printf("%s -> %s\n", fin_hyp_s, curr_proof_s.c_str());
        }
        proofset.insert(curr_proof_s);
    }

    return 0;
}   