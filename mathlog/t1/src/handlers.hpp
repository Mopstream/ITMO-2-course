#ifndef HANDLERS_H
#define HANDLERS_H

#include <map>
#include <string>
#include <iostream>
#include <vector>
#include <set>

bool handle_alpha(const std::string &curr_proof_s, const std::string &hyp_s, const char *fin_hyp_s) {
    if (curr_proof_s != hyp_s) {
        return false;
    }
    std::printf("%s -> (%s -> %s)\n", fin_hyp_s, fin_hyp_s, fin_hyp_s);
    std::printf("(%s -> (%s -> %s)) -> (%s -> (%s -> %s) -> %s) -> (%s -> %s)\n", fin_hyp_s, fin_hyp_s,
                fin_hyp_s, fin_hyp_s, fin_hyp_s, fin_hyp_s, fin_hyp_s, fin_hyp_s, fin_hyp_s);
    std::printf("(%s -> (%s -> %s) -> %s) -> (%s -> %s)\n", fin_hyp_s, fin_hyp_s, fin_hyp_s, fin_hyp_s,
                fin_hyp_s, fin_hyp_s);
    std::printf("%s -> (%s -> %s) -> %s\n", fin_hyp_s, fin_hyp_s, fin_hyp_s, fin_hyp_s);
    return true;
}

bool handle_mp(std::map<std::string, std::vector<std::string>> &impls, const std::set<std::string> &proofset,
               const std::string &curr_proof_s, const char *fin_hyp_s) {
    if (impls.find(curr_proof_s) != impls.end()) {
        bool flag = false;
        std::string fi;
        for (auto &it : impls[curr_proof_s]){
            if (proofset.find(it) != proofset.end()) {
                flag = true;
                fi = it;
                break;
            }
        }
        if(flag){
            std::string se = "(" + fi + " -> " + curr_proof_s + ")";
            std::printf("(%s -> %s) -> (%s -> %s) -> (%s -> %s)\n", fin_hyp_s, fi.c_str(), fin_hyp_s, se.c_str(),
                        fin_hyp_s, curr_proof_s.c_str());
            std::printf("(%s -> %s) -> (%s -> %s)\n", fin_hyp_s, se.c_str(), fin_hyp_s, curr_proof_s.c_str());
            return true;
        }
    }
    return false;
}

void handle_axiom(const char *fin_hyp_s, const char *curr_proof_str) {
    std::printf("%s -> %s -> %s\n", curr_proof_str, fin_hyp_s, curr_proof_str);
    std::printf("%s\n", curr_proof_str);
}

#endif //HANDLERS_H
