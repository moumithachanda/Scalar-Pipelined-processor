#include <fstream>
#include <iostream>
#include <utility>
#include <vector>
#include <queue>
#include <getopt.h>
#include <cstdio>
#include <cstdlib>
#include <string>
#include <map>
#include <stack>
#include <algorithm>
#include <iterator>
#include <cassert>

using namespace std;

class registers{
    public:
    int8_t data;
    bool valid;

    registers(int8_t a){
        data = a;
        valid = 1;
    }
};

pair<int,int> IF(int &PC, vector<int>& ins, queue<int>& pres_q, queue<int>& next_q, int& flag,int& Ctrl_stall){
    if(flag == 77 || flag == 76){
        Ctrl_stall++;
        pres_q.pop();
        next_q.push(1);
        flag--;
        return make_pair(-1,-1);
    }
    else{
        pres_q.pop();
         

        if(PC <= 254){
            next_q.push(2);
            next_q.push(1);
        }
        else{
            next_q.push(2);
        }
        pair<int,int> p = make_pair(ins[PC], ins[PC+1]);
        PC+=2;
        return p;
    }
}

pair<pair<int8_t,int8_t>,pair<int,int> > ID(pair<int,int> inp, vector<registers>& R, int &flag, queue<int>& pres_q, queue<int>& next_q,
    int& raw_stall,
    int& arith,
    int& logi,
    int& li,
    int& sh_imm,
    int& mem_ins,
    int& ctrl_ins,
    int& hlt){
    // this is rd destination
    int rd = (inp.first)%16;
    int inst = (inp.first)/16;
    int rs1 = (inp.second)/16;
    int rs2 = (inp.second)%16;
    int8_t a,b;
    if(inst>=0 && inst<=2){ // ADD , SUB , MUL
        
        if(R[rs1].valid && R[rs2].valid){
            flag = 0;
            R[rd].valid = 0;
            a = R[rs1].data;
            b = R[rs2].data;
            pres_q.pop();
            next_q.push(3);
            arith++;

        }
        else {
            flag = 1;
            a = 0;
            b = 0;
            while(!pres_q.empty()){
                pres_q.pop();
            }
            next_q.push(2);
            next_q.push(1);
            raw_stall++;
        }
    }
    else if(inst == 3){ // INC
        if(R[rd].valid){
            flag = 0;
            R[rd].valid = 0;
            a = R[rd].data;
            b = 1;
            pres_q.pop();
            next_q.push(3);
            arith++;
        }
        else{
            flag = 1;
            a = 0;
            b = 0;
            while(!pres_q.empty()){
                pres_q.pop();
            }
            next_q.push(2);
            next_q.push(1);
            raw_stall++;
        }
    }
    else if(inst >=4 && inst<=6){ // AND , OR , XOR
        
        if(R[rs1].valid && R[rs2].valid){
            flag = 0;
            R[rd].valid = 0;
            a = R[rs1].data;
            b = R[rs2].data;
            pres_q.pop();
            next_q.push(3);
            logi++;
        }
        else{
            flag = 1;
            a = 0;
            b = 0;
            while(!pres_q.empty()){
                pres_q.pop();
            }
            next_q.push(2);
            next_q.push(1);
            raw_stall++;
        }
    }
    else if(inst == 7){ // NOT
        
        if(R[rs1].valid){
            R[rd].valid = 0;
            flag = 0;
            a = R[rs1].data;
            b = 0;
            pres_q.pop();
            next_q.push(3);
            logi++;
        }
        else{
            flag = 1;
            a = 0;
            b = 0;
            while(!pres_q.empty()){
                pres_q.pop();
            }
            next_q.push(2);
            next_q.push(1);
            raw_stall++;
        }
    }
    else if(inst == 9 || inst == 8){ // SLLI , SRLI
        
        if(R[rs1].valid){
            R[rd].valid = 0;
            flag = 0;
            a = R[rs1].data;
            b = static_cast<int8_t> (rs2);
            pres_q.pop();
            next_q.push(3);
            sh_imm++;
        }
        else{
            flag = 1;
            a = 0;
            b = 0;
            while(!pres_q.empty()){
                pres_q.pop();
            }
            next_q.push(2);
            next_q.push(1);
            raw_stall++;
        }
    }
    else if(inst == 10){ // LI
        R[rd].valid = 0;
        a = static_cast<int8_t> (inp.second);
        b = 0;
        pres_q.pop();
        next_q.push(3);
        li++;
    }
    else if(inst == 11){ // LD
        
        if(R[rs1].valid){
            R[rd].valid = 0;
            flag = 0;
            a = R[rs1].data;
            b = static_cast<int8_t> (rs2);
            if(b > 7) b = -(16-b);
            pres_q.pop();
            next_q.push(3);
            mem_ins++;
        }
        else{
            flag = 1;
            a = 0;
            b = 0;
            while(!pres_q.empty()){
                pres_q.pop();
            }
            next_q.push(2);
            next_q.push(1);
            raw_stall++;
        }
    }
    else if(inst == 12){ // ST

        if(R[rd].valid && R[rs1].valid){
            a = R[rs1].data;

            b = R[rd].data;
            rd = rs2;
            flag = 0;
            pres_q.pop();
            next_q.push(3);
            mem_ins++;
        }
        else{
            a = R[rs1].data;
            b = R[rd].data;
            rd = rs2;
            flag = 1;
            while(!pres_q.empty()){
                pres_q.pop();
            }
            next_q.push(2);
            next_q.push(1);
            raw_stall++;
        }
    }
    else if(inst == 13){ // JMP
        rd*=16;
        rd+=rs1;
        a = static_cast<int8_t> (rd);
        b = 0;
        flag = 77;
        rd = 0;
        pres_q.pop();
        next_q.push(3);
        ctrl_ins++;
    }
    else if(inst == 14){ // BEQZ
        if(R[rd].valid){
            a = R[rd].data;
            b = static_cast<int8_t> (inp.second);
            flag = 77;
            pres_q.pop();
            next_q.push(3);
            ctrl_ins++;
        }
        else{
            flag = 1;
            a = R[rd].data;
            b = static_cast<int8_t> (inp.second);
            while(!pres_q.empty()){
                pres_q.pop();
            }
            next_q.push(2);
            next_q.push(1);
            raw_stall++;
        }  
    }
    else if(inst == 15){
        pres_q.pop();
        pres_q.pop();
        next_q.push(3);
        //cout << "hlt " << endl;
        a=0;
        b=0;
        hlt++;
    }

    return make_pair(make_pair(a,b),make_pair(inst,rd));
}

pair<pair<int, int>, vector<int8_t> > IE(pair<pair<int8_t,int8_t>,pair<int,int> > inp, int& flag, int& PC, queue<int>& pres_q, queue<int>& next_q ){

    int inst = inp.second.first;
    int8_t a = inp.first.first;
    int8_t b = inp.first.second;
    int8_t out;
    int rd = inp.second.second;

    vector<int8_t> vec_out;

    if(inst == 0){
        out = a+b;
        vec_out.push_back(out);
    }
    else if(inst == 1){
        out = a-b;
        vec_out.push_back(out);
    }
    else if(inst == 2){
        out = a*b;
        vec_out.push_back(out);
    }
    else if(inst == 3){
        out = a+1;
        vec_out.push_back(out);
    }
    else if(inst == 4){
        out = a & b;
        vec_out.push_back(out);
    }
    else if(inst == 5){
        out = a |b;
        vec_out.push_back(out);
    }
    else if(inst == 6){
        out = a ^ b;
        vec_out.push_back(out);
    }
    else if(inst == 7){
        out = ~a;
        vec_out.push_back(out);
    }
    else if(inst == 8){
        out = a<<b;
        vec_out.push_back(out);
    }
    else if(inst == 9){
        out = a>> b;
        vec_out.push_back(out);
    }
    else if(inst == 10){
        vec_out.push_back(a);
    }
    else if(inst == 11){
        out = a + b;
        vec_out.push_back(out);
    }
    else if(inst == 12){
        int8_t c;
        c = static_cast<int8_t> (rd);
        if(c >= 8) c = -(16-c); 
        out = a + c;
        vec_out.push_back(out);
        vec_out.push_back(b);
    }
    else if(inst == 13){
        a*=2;
        PC+=a;

    }
    else if(inst == 14){
        if(a == 0){
            PC = PC + (b*2);
        }
    }

    pres_q.pop();
    next_q.push(4);

    return make_pair(make_pair(inst, rd), vec_out);
}

pair<pair<int, int>, vector<int8_t> > DA(pair<pair<int,int>, vector<int8_t> > inp, vector<int8_t>& v2, vector<registers>& RF, queue<int>& pres_q, queue<int>& next_q){

    int inst = inp.first.first;
    int rd = inp.first.second;
    vector<int8_t> vec_out = inp.second;

    if(inst == 11){
        vec_out[0] = v2[vec_out[0]];
    }
    else if(inst == 12){
        v2[vec_out[0]] = vec_out[1];

    }

    pres_q.pop();
    next_q.push(5);

    return make_pair(make_pair(inst, rd), vec_out);
}

void WB(pair<pair<int,int>, vector<int8_t> > inp, vector<registers> &RF,int &flag, queue<int>& pres_q, queue<int>& next_q,int& no_of_ins ){

    int rd = inp.first.second;
    int inst = inp.first.first; 
    vector<int8_t> vec_out = inp.second;

    if(inst <= 11 && inst >=0){
        RF[rd].data = vec_out[0];
        RF[rd].valid = 1;
    }

    pres_q.pop();
    no_of_ins++;

}

class processor{

    public:

    vector<registers> R;
    vector<int8_t> regs;
    vector<int> ins;
    vector<int8_t> dat;
    int PC;
    
    int Ctrl_stall;
    int raw_stall;
    int arith;
    int logi;
    int li;
    int sh_imm;
    int mem_ins;
    int ctrl_ins;
    int hlt;
    int no_of_ins;
    int cycles;

    processor(vector<int> IC, vector<int8_t> DC, vector<int8_t> RF){
        PC = 0;
        registers a(0);
        R.push_back(a);
        regs.push_back(0);
        for(int i=1;i<16;i++){
            registers b(RF[i]);
            regs.push_back(RF[i]);
            R.push_back(b);
        }
        ins = IC;
        dat = DC;
        Ctrl_stall = 0; 
        raw_stall = 0;
        arith = 0;
        logi = 0;
        li = 0;
        sh_imm = 0;
        mem_ins = 0;
        ctrl_ins = 0;
        hlt = 0;
        no_of_ins = 0;
        cycles = 0;
    }

    void processor_functioning(){
        queue<int> pres_q;
        queue<int> next_q;
        int flag = 0;
        pair<int,int> decode;
        pair<pair<int8_t,int8_t>,pair<int,int> > execute;
        pair<pair<int, int>, vector<int8_t> > data_access;
        pair<pair<int, int>, vector<int8_t> > writeback;

        pres_q.push(1);
        while(!pres_q.empty() || !next_q.empty()){
            if(pres_q.empty()){
                pres_q = next_q;
                while(!next_q.empty()){
                    next_q.pop();
                }
                cycles++;
            }
            else{
                if(pres_q.front() == 1){
                    decode = IF(PC, ins, pres_q, next_q, flag, Ctrl_stall);
                }
                else if(pres_q.front() == 2){
                    execute = ID(decode, R, flag, pres_q, next_q, raw_stall, arith, logi, li, sh_imm, mem_ins, ctrl_ins, hlt);
                }
                else if(pres_q.front() == 3){
                    data_access = IE(execute, flag, PC, pres_q, next_q);
                }
                else if(pres_q.front() == 4){
                    writeback = DA(data_access, dat, R, pres_q, next_q);
                }
                else if(pres_q.front() == 5){
                    WB(writeback, R, flag, pres_q, next_q, no_of_ins);
                }
            }
        }
        cycles++;
    }  
};

int main() {
    
    ifstream ifile1, ifile2, ifile3;
    ifile1.open("input/ICache.txt"); 
    ifile2.open("input/DCache.txt");
    ifile3.open("input/RF.txt");

    vector<int> v1; 
    int b;
    while(ifile1 >> std::hex >> b){
        v1.push_back(b);
    }
    ifile1.close();

    vector<int8_t> v2;
    while(ifile2 >> std::hex >> b){
        v2.push_back(b);
    }
    ifile2.close();

    vector<int8_t> v3;
    while(ifile3 >> std::hex >> b){
        v3.push_back(b);
    }
    ifile3.close();

    processor pro(v1, v2, v3);
    pro.processor_functioning();

    ofstream Dfile, ofile;
    Dfile.open("output/DCache.txt");

    for(int8_t& i : pro.dat){
        int j = static_cast<int>(i);
        Dfile << std::hex << std::setw(2) << std::setfill('0') << (j & 0xff) << endl;
    }

    ofile.open("output/Output.txt");
    ofile << "Total number of instructions executed        : " << pro.no_of_ins << endl;
    ofile << "Number of instructions in each class     "    << endl;
    ofile << "Arithmetic instructions                      : " << pro.arith << endl;
    ofile << "Logical instructions                         : " << pro.logi << endl;
    ofile << "Shift instructions                           : " << pro.sh_imm << endl;
    ofile << "Memory instructions                          : " << pro.mem_ins << endl;
    ofile << "Load immediate instructions                  : " << pro.li << endl;
    ofile << "Control instructions                         : " << pro.ctrl_ins << endl;
    ofile << "Halt instructions                            : " << pro.hlt << endl;
    ofile << "Cycles Per Instruction                       : " << (double)pro.cycles/pro.no_of_ins << endl;
    ofile << "Total number of stalls                       : " << pro.Ctrl_stall + pro.raw_stall << endl;
    ofile << "Data stalls (RAW)                            : " << pro.raw_stall << endl;
    ofile << "Control stalls                               : " << pro.Ctrl_stall << endl;

    return 0;
}
