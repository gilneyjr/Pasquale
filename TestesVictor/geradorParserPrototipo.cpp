#include <bits/stdc++.h>
using namespace std;

string getparsename(string s){
    string t = "parse";
    for(int i = 0; i < (int)s.size(); i++) if(('a' <= s[i]  && s[i] <= 'z') || ('A' <= s[i]  && s[i] <= 'Z')){
        int id;
        if('a' <= s[i] && s[i] <= 'z') id = s[i]-'a';
        else id = s[i] - 'A';
        if(!i) t += char('A'+id);
        else t += char('a'+id);
    }else t += s[i];
    return t;
}

void solve(vector<string> v){
    char c = 'a';
    for(int i = 1; i < (int)v.size(); i++){
        cout << "    " << char(c++) << " <- ";
        if(v[i][0] == '['){
            v[i] = v[i].substr(1,v[i].size()-2);
            cout << "many " << getparsename(v[i]) << endl;
        }else cout << getparsename(v[i]) << endl;
    }
    cout << "    return $ " << v[0];
    for(int i = 0; i < (int)v.size()-1; i++)
        cout << " " << char('a'+i) << " ";
            cout << endl << endl;
}

int main(){
    cout << "module Parser where\n\nimport Text.ParserCombinators.Parsec\nimport Text.Parsec.Combinator\nimport Data.Functor.Identity\nimport Control.Monad\nimport Arvore\nimport ParserTokens\nimport Lexico\n\nparsePasquale :: String -> Either ParseError PROGRAMA\nparsePasquale input = runParser parsePrograma () \"\" (getTokens input)\n\n";
    string s;
    bool fim = 0;
    while(!fim){
        if(s != "data"){
            if(!(cin >> s)){ fim=1; break; }
            continue;
        }
        if(!(cin >> s)){ fim=1; break; }
        string t = getparsename(s);
        cout << t << " :: ParseArgs " << s << endl;
        cout << t << " = ";
        vector<vector<string> >ops;
        if(!(cin >> t)) fim=1;
        while(!fim && t != "data"){
            if(!(cin >> t)){ fim = 1; break; }
            vector<string> cur;
            while(t != "|" && t != "data" && !fim){
                if(t[0] == '{'){
                    string aux = "";
                    for(int j = 2; t[j] != '-'; j++) aux += t[j];
                    t = aux;
                }
                cur.push_back(t);
                if(!(cin >> t)){ fim=1; break; }
            }
            ops.push_back(cur);
        }
        assert(ops.size() > 0);
        if(ops.size() == 1){
            cout << "do\n";
            solve(ops[0]);
        }
        else{
            cout << endl;
            for(int i = 0; i < (int) ops.size() - 1; i++)
                cout << "    (try " << getparsename(ops[i][0]) << ") <|>" << endl;
            cout << "    " << getparsename(ops.back()[0]) << endl;
            cout << endl;
            
            for(int i = 0; i < (int) ops.size(); i++){
                cout << getparsename(ops[i][0]) << " :: ParseArgs " << s << endl;
                cout << getparsename(ops[i][0]) << " = do" << endl;
                solve(ops[i]);
            }
        }
        s = t;
    }
    
}


