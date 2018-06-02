#include <bits/stdc++.h>
using namespace std;

int main(){
    cout << "module ParserTokens where\nimport Data.Functor.Identity\nimport Text.Parsec\nimport Lexico\nimport Arvore\n\n";
    string s;
    while(cin >> s){
        string Token = "parse", t;
        int cnt = 0;
        cin >> t;
        while(t != "|") cin >> t, cnt++;
        for(int i = 0; i < (int)s.size(); i++) if(('a' <= s[i]  && s[i] <= 'z') || ('A' <= s[i]  && s[i] <= 'Z')){
            int id;
            if('a' <= s[i] && s[i] <= 'z') id = s[i]-'a';
            else id = s[i] - 'A';
            if(!i) Token += char('A'+id);
            else Token += char('a'+id);
        }else Token += s[i];
        cout << Token << " :: ParsecT [Token] u Identity Token\n";
        cout << Token << " = tokenPrim show update_pos get_token where\n";
        s = "( " + s;
        for(char c = 'x'; cnt--; c++)
            s += ' ', s += c;
        s += " )";
        cout << "    get_token " << s << " = Just " << s << "\n    get_token _ = Nothing\n\n";
    }
    cout << "update_pos :: SourcePos -> Token -> [Token] -> SourcePos\nupdate_pos pos _ (tok:_) = pos\nupdate_pos pos _ []      = pos\n";  
}


