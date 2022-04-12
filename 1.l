%{ 
    #include<bits/stdc++.h>
    #include <stdlib.h> 
    void yyerror(char *); 
    #include "node.h"
    #include "y.tab.h" 
    char s[]="invalid character";
    using namespace std;
    extern map<char,int >mp;
%} 

%% 

int			{return INT;}
float			{return FLOAT;}
bool			{return BOOL;}
and			{return AND;}
or			{return OR;}
not			{return NOT;}

break       		{return BREAK;}

if          		{yylval.d.place=yytext;	
			return IF;
			}
else      		{yylval.d.place=yytext;
			return ELSE;
			}
then      		{	yylval.d.place=yytext;
				return THEN;
			}
while       		{	yylval.d.place=yytext;
				return WHILE;
			}
case        		{
             			 yylval.d.place=yytext;
                    			return CASE;       
                	}
switch      		{
                			yylval.d.place=yytext;
                			return SWITCH;
            		}
for			{
				yylval.d.place=yytext;
				return FOR;
			}
[a-zA-Z][a-zA-Z0-9]*	{ 
                			yylval.d.place = yytext;  
                			return VARIABLE; 
        		}
[0-9]+"."+[0-9]+  	{
                   			 yylval.d.place = yytext; 
               			 return FLOAT; 
                	}
[0-9]+      		{ 
               			 yylval.d.place = yytext; 
              			  return INTEG; 
            		}
"=="        	{yylval.d.place=yytext;return ee;}
"<="        	{yylval.d.place=yytext;return le;}
">="        	{yylval.d.place=yytext;return ge;}
"!="        	{yylval.d.place=yytext;return ne;}
"+=" 		{yylval.d.EXTRA.op_val=2;return op;}
"-=" 		{yylval.d.EXTRA.op_val=3;return op;}
"*=" 		{yylval.d.EXTRA.op_val=4;return op;}
"/=" 		{yylval.d.EXTRA.op_val=5;return op;}
"="    	{yylval.d.place=yytext;return *yytext;}
	/*	"{"        	{yylval.d.place=yytext;return ob;}
"}"        	{yylval.d.place=yytext;return cb;}	*/
[-+*/&|~^<>@{};]  	{     
              			yylval.d.place=yytext;
              			return *yytext;   
        		}

[()\n#,:] 		{return *yytext; } 
 

[ \t]      {;} 
 


.           yyerror(s); 
%% 

int yywrap(void) 
{ 
    return 1;
}
