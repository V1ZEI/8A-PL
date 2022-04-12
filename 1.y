
 
%{ 
  #include<bits/stdc++.h>
  #include<iostream>
  #include<cstring>
  #include "node.h"
  
  using namespace std;

  extern int yylex();

    void yyerror(char *); 
    int yylex(void); 
    stack<string>stk;
    map <string,string>typecheck; 
  vector<char *>vars;
  map<string,string> values;
    int tc=1,lc=1,casef=0;
    string t;
    string begin1;
string getassignop(int a)			//in lexfile operators are identified by returning an 
{									//int which is decoded here 
	string ss;
   switch(a)
  {
  case 1:{ss="=";break;}
  case 2:{ss="+";break;}
  case 3:{ss="-";break;}
  case 4:{ss="*";break;}
  case 5:{ss="/";break;}

  }
  return ss;
}

string gen_code(string s1,string s2,string s3,string s4,string s5)	//to gen_codeerate string with 
{																// 5 parameters
	string temp="";
	temp.append(s1);
	temp.append(s2);
	temp.append(s3);
	temp.append(s4);
	temp.append(s5);
	temp.append(";");
	temp.append("\n");
	return temp;
}

string gen_code(string s1,string s2,string s3,string s4)		//to generate string with 4 
{														//parameters
	string temp="";
	temp.append(s1);
	temp.append(s2);
	temp.append(s3);
	temp.append(s4);
//	temp.append("\n");
	return temp;
}

int check_declaration(string a, string b, int var_flaga,int var_flagb)	//to check whether 2 
{														//operators are valid or not
	if(var_flaga==1)
	if(typecheck.find(a)==typecheck.end())				//checking if a is declared
	{
		cout<<"the variable "<<a<<" is not declared "<< endl;
		exit(0);
	}
	if(var_flagb==1)
	if(typecheck.find(b)==typecheck.end())				//checking if b is declared
	{	
		cout<<"the variable "<<b<<" is not declared "<< endl;
		exit(0);
	}
	if(var_flaga==3)				//if a is an integer value returned accordingly
	{
		string bb=typecheck[b];
		if(bb=="bool")
			return 0;
		if(bb=="int")
			return 2;
		if(bb=="float") 
			return 1;
	}
	if(var_flagb==3)			//if b is an integer value returned accordingly

	{
		string aa=typecheck[a];
		if(aa=="bool")
			return 0;
		if(aa=="int")
			return 2;
		if(aa=="float") 
			return 1;
	}
	string ta=typecheck[a];
	string tb=typecheck[b];
	if(ta=="bool" && tb=="bool")
		return 0;
	if(ta=="bool" || tb=="bool")
	{
		cout<<"boolean variable used in arithematic expression"<<endl;
		exit(0);
	}
	if(ta=="int"&&tb=="float")
	return 1;
	if(ta=="float"&&tb=="int")
	return 1;
	if(ta=="float"&&tb=="float")
	return 1;
	if(ta=="int" && tb=="int")
	return 2;	
}

int check_compatibility(string a,string b,int var_flaga, int var_flagb)		//to check if both  
{				
	string ta,tb;											//are of same type
	if(var_flaga==1)
	if(typecheck.find(a)==typecheck.end())
	{
		cout<<"the variable "<<a<<" is not declared "<< endl;
		exit(0);
	}
	if(var_flagb==1)
	if(typecheck.find(b)==typecheck.end())
	{
		cout<<"the variable "<<b<<" is not declared "<< endl;
		exit(0);
	}
		
	if(var_flaga!=3&&var_flagb!=3)
	{
		ta=typecheck[a];
		tb=typecheck[b];
		if(ta=="bool"&&(tb=="int"||tb=="float"))
		{
			cout<<"incompatible type conversion"<<endl;
			exit(0);
		}
		if(tb=="bool"&&(ta=="int"||ta=="float"))
		{
			cout<<"incompatible type conversion"<<endl;
			exit(0);
		}
	}
}
 
%} 

%union {    
    DATA d;
}

%left '|'
%left '^'
%left '&' 
%left '>' '<' ne ee ob cb
%left le ge
%left '+' '-' 
%left '*' '/'
%right '@' '~'
%left NOT
%left OR
%left AND

%token <d> op
%token <d> VARIABLE INTEG WHILE FOR IF THEN ELSE  BOOL INT FLOAT AND OR NOT SWITCH CASE BREAK ob cb
%type <d> program statement expr newline decl type vals decls cases


%%

program: 											 
       	decls  program statement '#' newline{
        								
        		cout<<"end of the input"<<endl;
        		cout<<$1.code<<endl;
			cout<<$3.code<<endl<<endl;
                                }
        |  {}
        ;
type	:INT 	{	string code1="int";				
			$$.code=new char[code1.length()];	
                    	strcpy($$.code,code1.c_str());
              	}
	|FLOAT	{		
			string code1="float";
			$$.code=new char[code1.length()];
                    	strcpy($$.code,code1.c_str());
              	}
	|BOOL	{	string code1="bool";
			$$.code=new char[code1.length()];
                    	strcpy($$.code,code1.c_str());
              	}
	;

decls	: decls  decl '\n'	{	
					string code1="";
					code1.append($1.code);
					code1.append("\n");
					code1.append($2.code);
					code1.append("\n");
					$$.code=new char[code1.length()];
                       		strcpy($$.code,code1.c_str());
                       	}
	| decl '\n' 		{
					string code1="";
					code1.append($1.code);
					$$.code=new char[code1.length()];
                    			strcpy($$.code,code1.c_str());
                  		}
	;
decl	:  type vals ';' 	{										
					string code1="";
					string ty="";
					ty.append($1.code);
					code1.append($1.code);
					code1.append(" ");
					code1.append($2.code);
					code1.append("; ");
					$$.code=new char[code1.length()];
                        		strcpy($$.code,code1.c_str());
                       		for(int i=0;i<vars.size();i++)
                       		{ 
                       			string ss="";
                       			ss.append(vars[i]);
                       			if(typecheck.find(ss)==typecheck.end())
                       			typecheck[ss]=ty;
                       			else
                       		 	{
                       		 		cout<<ss<<" variable has been declared more than once"<<endl;
                       		 		exit(1);
                       		 	}                        	
                        		}
                  			vars.clear();		
			}
	;
vals:	expr  {								/*for last variable in declaration*/
				if($1.EXTRA.var_flag==2||$1.EXTRA.var_flag==3)
				{cout<<"error in declaration!! :("<<endl;exit(0);}
				string code1="";
				vars.push_back($1.place);
				code1.append($1.place);
				$$.code=new char[code1.length()];
                        		strcpy($$.code,code1.c_str());
          		}
	|expr '=' INTEG  ',' vals{			/*for variables which are initialized*/
				 if($1.EXTRA.var_flag==2||$1.EXTRA.var_flag==3)
				{cout<<"error in declaration!! :("<<endl;exit(0);}
				string code1="";
				vars.push_back($1.place);
				code1.append($1.place);
				code1.append("=");
				string s;int i=0;
				while($3.place[i]!=',') s+=$3.place[i++];
				s+=" ";
				code1.append(s);
				code1.append(", ");
				code1.append($5.code);
				$$.code=new char[code1.length()];
                        		strcpy($$.code,code1.c_str());						
                        	}
	|expr ',' vals 	{						/*if variables are not yet done*/
				if($1.EXTRA.var_flag==2||$1.EXTRA.var_flag==3)
				{cout<<"error in declaration!! :("<<endl;exit(0);}
				string code1="";
				vars.push_back($1.place);
				code1.append($1.place);
				code1.append(", ");
				code1.append($3.code);
				$$.code=new char[code1.length()];
                        	strcpy($$.code,code1.c_str());
			}
	|{}
	;
newline: '\n'{}
	| { }
	;
statement:							/*for different statements in the code*/
	IF '(' expr ')' '{' newline statement '}' newline statement{	/*IF*/
				string snext="l"+to_string(lc++);
				string code1="";
				code1.append($3.code);
				code1+=gen_code("if ",$3.place," = ","0 ");	
				code1.append("goto ");
				code1.append(snext);
				code1.append("\n");
				code1.append($7.code);
				code1.append("\n");
				code1.append(snext);
				code1.append(":");
				code1.append("\n");
				code1.append($10.code);
				$$.code=new char[code1.length()];                                  
				strcpy($$.code,code1.c_str());
			       }
															
| IF '(' expr ')' '{' newline statement '}' newline ELSE '{' newline statement '}' newline statement {
		/*IF ELSE */
				string s2next="l"+to_string(lc++);
				string snext="l"+to_string(lc++);
				string code1="";
				code1.append($3.code);
				code1+=gen_code("if ",$3.place," = ","0 ");								
				code1.append("goto ");
				code1.append(s2next);
				code1.append("\n");
				code1.append($7.code);
				code1.append("\n");				
				code1.append("goto ");
				code1.append(snext);
				code1.append("\n");
				code1.append(s2next);
				code1.append(":");
				code1.append("\n");
				code1.append($13.code);
				code1.append("\n");
				code1.append(snext);
				code1.append(":");
				code1.append("\n");
				code1.append($16.code);
				$$.code=new char[code1.length()];
                            		strcpy($$.code,code1.c_str());
		}
			
	| FOR '(' statement ';' expr ';' statement ')' '{' newline statement '}' newline statement{			/*FOR*/
			string sbegin="l"+to_string(lc++);
			string ebegin="l"+to_string(lc++);
			string safter="l"+to_string(lc++);
			string code1="";
			code1.append(sbegin);
			code1.append(":");
			code1.append("\n");
			code1.append($3.code);
			code1.append("\n");
			code1.append(ebegin);
			code1.append(":");
			code1.append("\n");
			code1.append($5.code);
			code1+=gen_code("if ",$5.place," = ","0 ");
			code1.append("goto ");
			code1.append(safter);
			code1.append("\n");
			code1.append($11.code);
			code1.append("\n");
			code1.append($7.code);
			code1.append("\n");
			code1.append("goto ");
			code1.append(ebegin);
			code1.append("\n");
			code1.append(safter);
			code1.append(":");
			code1.append("\n");
			code1.append($14.code);
			$$.code=new char[code1.length()];
                                    strcpy($$.code,code1.c_str());
		}
		
| WHILE '(' expr ')' '{' newline statement '}' newline statement   {
	/*WHILE*/
			string begin="l"+to_string(lc++);
			string after="l"+to_string(lc++);
			string code1="";
			code1.append(begin);
			code1.append(":");
			code1.append("\n");
			code1.append($3.code);
			code1+=gen_code("if ",$3.place," = ","0 ");
			code1.append("goto ");
			code1.append(after);
			code1.append("\n");
			code1.append($7.code);
			//code1.append(";");
			code1.append("\n");											
			code1.append("goto ");
			code1.append(begin);
			code1.append("\n");
			code1.append(after);
			code1.append(":");
			code1.append("\n");
			code1.append($10.code);
			$$.code=new char[code1.length()];
                             	strcpy($$.code,code1.c_str());
	  }
|SWITCH '(' expr ')' '{' '\n' cases '}' '\n' statement{
	/*SWITCH*/
			string code1="";											
			casef=0;
			code1.append($3.code);
			code1.append("\n");
			map<string,string>::iterator it;
			for(it=values.begin();it!=values.end();it++){
				code1+=gen_code("\nif ",$3.place," = ",it->first);
				code1.append("    goto ");
			//	code1.append("\n");
				code1.append(it->second);
				code1.append("\n");
			}
			code1.append($7.code);
			code1.append("\n");
			code1.append(begin1);
			code1.append(": \n");
			code1.append($10.code);
			$$.code=new char[code1.length()];
			strcpy($$.code,code1.c_str());
		}
         | expr op expr ';' newline statement    {				/*   for +=,-=,*=,/=   */
         			int x=check_compatibility($1.place,$3.place,$1.EXTRA.var_flag,$3.EXTRA.var_flag);
                                    string code1="";
                                    string place1="";
                                    place1="t"+to_string(tc++);
                                    code1.append($3.code);
                                    code1.append(place1);
                                    code1.append("=");
                                    code1.append($1.place);
                                    code1.append(getassignop($2.EXTRA.op_val));
                                    code1.append($3.place);
                                    code1.append(";\n");
                                    code1.append($1.place);
                                    code1.append("=");
                                    code1.append(place1);
                                    code1.append(";\n");
                                    code1.append($6.code);
                                    $$.code=new char[code1.length()];
                                    strcpy($$.code,code1.c_str());
                                    
                                    
                                  }
     |expr '=' expr ';' newline statement {			/*for assignment statments*/
         			int x=check_compatibility($1.place,$3.place,$1.EXTRA.var_flag,$3.EXTRA.var_flag);
                                    string code1="";
                                    code1.append($3.code);
                                    code1.append($1.place);
                                    code1.append("=");
                                    code1.append($3.place);
                                    code1.append(";\n");
                                    code1.append($6.code);
                                    $$.code=new char[code1.length()];
                                    strcpy($$.code,code1.c_str());			
         							}
     | {									/*for null cases*/
         	string code1="";
         	$$.code=new char[code1.length()];
          	strcpy($$.code,code1.c_str());
         }
     ;
cases :  cases newline CASE expr ':' '{' newline statement BREAK ';' newline '}' '\n' {			/*for cases in switch statement*/
			string code1="";	
			code1.append($1.code);
			if(casef==0){
				begin1="l"+to_string(lc++);
				casef=1;
			}
			string begin="l"+to_string(lc++);
			code1.append(begin);
			code1.append(":");
			code1.append("\n");
			code1.append($8.code);
			code1.append("goto ");
			code1.append(begin1);
			code1.append("\n");
			$$.code=new char[code1.length()];
                                    strcpy($$.code,code1.c_str());
			values[$4.place]=begin;
	}  
	|	{string code1="";$$.code=new char[code1.length()];strcpy($$.code,code1.c_str());}
			;
expr:  expr '+' expr           {   							
	/*for ADDITION*/
          			$$.EXTRA.var_flag=2;
          			string code1="";
          			string place1="";											
          			int x=check_declaration($1.place,$3.place,$1.EXTRA.var_flag,$3.EXTRA.var_flag);
			$$.place=new char[place1.length()];
			place1="t"+to_string(tc++);
			strcpy($$.place,place1.c_str());
                                    $$.code=new char[code1.length()];
                                    code1.append($1.code);
                                    code1.append($3.code);
                                    code1+=gen_code($$.place," = ",$1.place," + ",$3.place);
                                    if(x==1)
                                     typecheck[$$.place]="float";
                                     else if(x==2)
                                     typecheck[$$.place]="int";
                                     else if(x==0)
                                     {cout<<"boolean variable used in arithematic expression"<<endl;exit(0);}
                                     $$.code=new char[code1.length()];
			strcpy($$.code,code1.c_str());
                                  }

         | expr '-' expr           {							
	/*SUBTRACTION*/
         			$$.EXTRA.var_flag=2;	//var_flag is to differentite integer and 
                                       				//float from others
         			string code1="";
			string place1="";
			int x=check_declaration($1.place,$3.place,$1.EXTRA.var_flag,$3.EXTRA.var_flag);
			$$.place=new char[place1.length()];
			place1="t"+to_string(tc++);
			strcpy($$.place,place1.c_str());
                                    $$.code=new char[code1.length()];
                                    code1.append($1.code);
                                    code1.append($3.code);
                                    code1+=gen_code($$.place," = ",$1.place," - ",$3.place);
                                    if(x==1)
                                    typecheck[$$.place]="float";
                                    else if(x==2)
                                    typecheck[$$.place]="int";
                                    else if(x==0)
                                    {cout<<"boolean variable used in arithematic expression"<<endl;exit(0);}
                                     $$.code=new char[code1.length()];
			strcpy($$.code,code1.c_str());
                           } 
         | expr '*' expr           { 	
		/*MULTIPLICATION*/
         			$$.EXTRA.var_flag=2; //var_flag is to differentite integer and 
                                       				//float from others
                                    string code1="";
			string place1="";
			int x=check_declaration($1.place,$3.place,$1.EXTRA.var_flag,$3.EXTRA.var_flag);
			$$.place=new char[place1.length()];
			place1="t"+to_string(tc++);
			strcpy($$.place,place1.c_str());
                                    $$.code=new char[code1.length()];
                                    code1.append($1.code);
                                    code1.append($3.code);
                                    code1+=gen_code($$.place," = ",$1.place," * ",$3.place);
                                     if(x==1)
                                     typecheck[$$.place]="float";
                                     else if(x==2)
                                     typecheck[$$.place]="int";
                                     else if(x==0)
                                     {cout<<"boolean variable used in arithematic expression"<<endl;exit(0);}
                                     $$.code=new char[code1.length()];									
                                     strcpy($$.code,code1.c_str());
                                  } 
         | expr '/' expr           {	
		/*DIVISION*/
       			$$.EXTRA.var_flag=2; //var_flag is to differentite integer and 
                                       				//float from others
                                    string code1="";
			string place1="";
			int x=check_declaration($1.place,$3.place,$1.EXTRA.var_flag,$3.EXTRA.var_flag);
			$$.place=new char[place1.length()];
			place1="t"+to_string(tc++);
			strcpy($$.place,place1.c_str());
                                    $$.code=new char[code1.length()];
                                    code1.append($1.code);
                                    code1.append($3.code);
                                    code1+=gen_code($$.place," = ",$1.place," / ",$3.place);
                                     if(x==1)
                                     typecheck[$$.place]="float";
                                     else if(x==2)
                                     typecheck[$$.place]="int";
                                     else if(x==0)
                                     {cout<<"boolean variable used in arithematic expression"<<endl;exit(0);}
                                     $$.code=new char[code1.length()];									
                                     strcpy($$.code,code1.c_str());
                                   }  
         | expr '@' expr           {	
			/*EXPONENTIATION*/
         			$$.EXTRA.var_flag=2; //var_flag is to differentite integer and 
                                       				//float from others
                                    string code1="";
			string place1="";
			int x=check_declaration($1.place,$3.place,$1.EXTRA.var_flag,$3.EXTRA.var_flag);
			$$.place=new char[place1.length()];
			place1="t"+to_string(tc++);
			strcpy($$.place,place1.c_str());
                                    $$.code=new char[code1.length()];
                                    code1.append($1.code);
                                    code1.append($3.code);
                                    code1+=gen_code($$.place," = ",$1.place," @ ",$3.place);
                                     if(x==1)
                                     typecheck[$$.place]="float";
                                     else if(x==2)
                                     typecheck[$$.place]="int";
                                     else if(x==0)
                                     {cout<<"boolean variable used in arithematic expression"<<endl;exit(0);}
                                     $$.code=new char[code1.length()];									
                                     strcpy($$.code,code1.c_str());
                                   }  
         | expr '|' expr           {	
			/* bitwise OR*/
         			$$.EXTRA.var_flag=2; //var_flag is to differentite integer and 
                                       				//float from others
                                    string code1="";
			string place1="";
			int x=check_declaration($1.place,$3.place,$1.EXTRA.var_flag,$3.EXTRA.var_flag);
			$$.place=new char[place1.length()];
			place1="t"+to_string(tc++);
			strcpy($$.place,place1.c_str());
                                    $$.code=new char[code1.length()];
                                    code1.append($1.code);
                                    code1.append($3.code);
                                    code1+=gen_code($$.place," = ",$1.place," | ",$3.place);
                                     if(x==1)
                                     typecheck[$$.place]="float";
                                     else if(x==2)
                                     typecheck[$$.place]="int";
                                     else if(x==0)
                                     {cout<<"boolean variable used in arithematic expression"<<endl;exit(0);}
                                     $$.code=new char[code1.length()];									
                                     strcpy($$.code,code1.c_str());
                                      
                                   }  
        | expr '&' expr           {	
			/*bitwise AND	*/
        			$$.EXTRA.var_flag=2; //var_flag is to differentite integer and 
                                       				//float from others
                                    string code1="";
			string place1="";
			int x=check_declaration($1.place,$3.place,$1.EXTRA.var_flag,$3.EXTRA.var_flag);
			$$.place=new char[place1.length()];
			place1="t"+to_string(tc++);
			strcpy($$.place,place1.c_str());
                                    $$.code=new char[code1.length()];
                                    code1.append($1.code);
                                    code1.append($3.code);
                                    code1+=gen_code($$.place," = ",$1.place," & ",$3.place);
                                     if(x==1)
                                     typecheck[$$.place]="float";
                                     else if(x==2)
                                     typecheck[$$.place]="int";
                                     else if(x==0)
                                     {cout<<"boolean variable used in arithematic expression"<<endl;exit(0);}
                                     $$.code=new char[code1.length()];
                                     strcpy($$.code,code1.c_str());
                                  }
        |expr ge  expr 	{	
			/*    >=    */
        			$$.EXTRA.var_flag=2; //var_flag is to differentite integer and 
                                       				//float from others
                                    string code1="";
			string place1="";
			int x=check_declaration($1.place,$3.place,$1.EXTRA.var_flag,
			$3.EXTRA.var_flag);
			$$.place=new char[place1.length()];
			place1="t"+to_string(tc++);
			strcpy($$.place,place1.c_str());
                                    $$.code=new char[code1.length()];
                                    code1.append($1.code);
                                    code1.append($3.code);
                                    code1+=gen_code($$.place," = ",$1.place," >= ",$3.place);
                                     if(x==1)
                                     typecheck[$$.place]="float";
                                     else if(x==2)
                                     typecheck[$$.place]="int";
                                     else if(x==0)
                                     {cout<<"boolean variable used in arithematic expression"<<endl;exit(0);}
                                     $$.code=new char[code1.length()];									
                                     strcpy($$.code,code1.c_str());
        							}
        | expr '>' expr	 {	
			/*    >   */
			$$.EXTRA.var_flag=2; //var_flag is to differentite integer and 
                                       				//float from others
                                    string code1="";
			string place1="";
			int x=check_declaration($1.place,$3.place,$1.EXTRA.var_flag,$3.EXTRA.var_flag);
			$$.place=new char[place1.length()];
			place1="t"+to_string(tc++);
			strcpy($$.place,place1.c_str());
                                    $$.code=new char[code1.length()];
                                    code1.append($1.code);
                                    code1.append($3.code);
                                    code1+=gen_code($$.place," = ",$1.place," > ",$3.place);
                                     if(x==1)
                                     typecheck[$$.place]="float";
                                     else if(x==2)
                                     typecheck[$$.place]="int";
                                     else if(x==0)
                                     {cout<<"boolean variable used in arithematic expression"<<endl;exit(0);}
                                     $$.code=new char[code1.length()];									
                                     strcpy($$.code,code1.c_str());
     		}
        |expr le expr	{	
			/*	<=		*/
        			$$.EXTRA.var_flag=2; //var_flag is to differentite integer and 
                                       				//float from others
                                    string code1="";
			string place1="";
			int x=check_declaration($1.place,$3.place,$1.EXTRA.var_flag,$3.EXTRA.var_flag);
			$$.place=new char[place1.length()];
			place1="t"+to_string(tc++);
			strcpy($$.place,place1.c_str());
                                    $$.code=new char[code1.length()];
                                    code1.append($1.code);
                                    code1.append($3.code);
                                    code1+=gen_code($$.place," = ",$1.place," <=",$3.place);
                                     if(x==1)
                                     typecheck[$$.place]="float";
                                     else if(x==2)
                                     typecheck[$$.place]="int";
                                     else if(x==0)
                                     {cout<<"boolean variable used in arithematic expression"<<endl;exit(0);}
                                     $$.code=new char[code1.length()];									
                                     strcpy($$.code,code1.c_str());
        						}
        |expr ne expr 			{	
			/*  	"!="	*/
        			$$.EXTRA.var_flag=2; //var_flag is to differentite integer and 
                                       				//float from others
                                    string code1="";
			string place1="";
			int x=check_declaration($1.place,$3.place,$1.EXTRA.var_flag,$3.EXTRA.var_flag);
			$$.place=new char[place1.length()];
			place1="t"+to_string(tc++);
			strcpy($$.place,place1.c_str());
                                    $$.code=new char[code1.length()];
                                    code1.append($1.code);
                                    code1.append($3.code);
                                    code1+=gen_code($$.place," = ",$1.place," != ",$3.place);
                                     if(x==1)
                                     typecheck[$$.place]="float";
                                     else if(x==2)
                                     typecheck[$$.place]="int";
                                     else if(x==0)
                                     {cout<<"boolean variable used in arithematic expression"<<endl;exit(0);}
                                     $$.code=new char[code1.length()];									
                                     strcpy($$.code,code1.c_str());
        							}
        |expr ee expr 	{	
			/*	"=="  */
        			$$.EXTRA.var_flag=2; //var_flag is to differentite integer and 
                                       				//float from others
                                    string code1="";
			string place1="";
			int x=check_declaration($1.place,$3.place,$1.EXTRA.var_flag,$3.EXTRA.var_flag);
			$$.place=new char[place1.length()];
			place1="t"+to_string(tc++);
			strcpy($$.place,place1.c_str());
                                    $$.code=new char[code1.length()];
                                    code1.append($1.code);
                                    code1.append($3.code);
                                    code1+=gen_code($$.place," = ",$1.place," ==",$3.place);
                                     if(x==1)
                                     typecheck[$$.place]="float";
                                     else if(x==2)
                                     typecheck[$$.place]="int";
                                     else if(x==0)
                                     {cout<<"boolean variable used in arithematic expression"<<endl;exit(0);}
                                     $$.code=new char[code1.length()];									
                                     strcpy($$.code,code1.c_str());
		}
   	| expr '<' expr	 {	
			/*	"<"   */
   			$$.EXTRA.var_flag=2; //var_flag is to differentite integer and 
                                       				//float from others
                                    string code1="";
			string place1="";
			int x=check_declaration($1.place,$3.place,$1.EXTRA.var_flag,$3.EXTRA.var_flag);
			$$.place=new char[place1.length()];
			place1="t"+to_string(tc++);
			strcpy($$.place,place1.c_str());
                                    $$.code=new char[code1.length()];
                                    code1.append($1.code);
                                    code1.append($3.code);
                                    code1+=gen_code($$.place," = ",$1.place," <",$3.place);
                                     if(x==1)
                                     typecheck[$$.place]="float";
                                     else if(x==2)
                                     typecheck[$$.place]="int";
                                     else if(x==0)
                                     {cout<<"boolean variable used in arithematic expression"<<endl;exit(0);}
                                     $$.code=new char[code1.length()];									
                                     strcpy($$.code,code1.c_str());
        									 }
        | '~' expr                {		
			/*	"~"	*/
        			$$.EXTRA.var_flag=2;//var_flag is to differentite integer and 
                                       				//float from others
        			string code1="";
			string place1="";
			place1="t"+to_string(tc++);
                                    strcpy($$.place,place1.c_str());
                                    code1.append($$.place);
                                    code1+=" = ~";
                                    code1+=$2.place;
                                    code1+="\n";
                                    if(typecheck.find($2.place)!=typecheck.end())
                                    typecheck[$$.place]=typecheck[$2.place];
                                    else
                                    {cout<<"Not declared !!"<<endl;exit(0);}
                                    $$.code=new char[code1.length()];
                                    strcpy($$.code,code1.c_str());
                                   }  
          | '(' expr  ')'       {	
			/*	IN BRACKETS	*/
          			$$.EXTRA.var_flag=2;//var_flag is to differentite integer and 
                                       				//float from others
					string code1="";
                                    $$.code=new char[code1.length()];
                                    strcpy($$.code,code1.c_str());
                                    $$.place=new char[strlen($2.place)];
                                  	 strcpy($$.place,$2.place);
                                   	if(typecheck.find($2.place)!=typecheck.end())
                                   	typecheck[$$.place]=typecheck[$2.place];
                                   	else
                                   	{cout<<"Not declared !!"<<endl;exit(0);}
                               }
        	| expr AND expr           {	
		//*	logical AND		*/
        			$$.EXTRA.var_flag=2;//var_flag is to differentite integer and 
                                       				//float from others
         			string code1="";
			string place1="";
			int x=check_declaration($1.place,$3.place,$1.EXTRA.var_flag,$3.EXTRA.var_flag);
			$$.place=new char[place1.length()];
			place1="t"+to_string(tc++);
			strcpy($$.place,place1.c_str());
                                    $$.code=new char[code1.length()];
                                    code1.append($1.code);
                                    code1.append($3.code);
                                    code1+=gen_code($$.place," = ",$1.place," AND ",$3.place);
                                    if(x==0 && x==1 && x==2)
			typecheck[$$.place]="bool";
                                    $$.code=new char[code1.length()];
			strcpy($$.code,code1.c_str());
                                  }  
         | expr OR expr           {	
		/*	logical OR	*/
         		$$.EXTRA.var_flag=2;//var_flag is to differentite integer and 
                                       				//float from others
         			string code1="";
			string place1="";
			int x=check_declaration($1.place,$3.place,$1.EXTRA.var_flag,$3.EXTRA.var_flag);
			$$.place=new char[place1.length()];
			place1="t"+to_string(tc++);
			strcpy($$.place,place1.c_str());
                                    $$.code=new char[code1.length()];
                                    code1.append($1.code);
                                    code1.append($3.code);
                                    code1+=gen_code($$.place," = ",$1.place," OR",$3.place);
                                    if(x==0 && x==1 && x==2)
			typecheck[$$.place]="bool";
                                    $$.code=new char[code1.length()];
			strcpy($$.code,code1.c_str());
                                      
                                   } 
         | NOT expr                {	
		/*	Logical NOT	*/
         			$$.EXTRA.var_flag=2;//var_flag is to differentite integer and 
                                       				//float from others
        			string code1="";
			string place1="";
                                  	place1="t"+to_string(tc++);
                                   	strcpy($$.place,place1.c_str());
                                   	code1.append($$.place);
                                   	code1+=" = NOT";
                                   	code1+=$2.place;
                                   	code1+="\n";
                                   	$$.code=new char[code1.length()];
                                   	strcpy($$.code,code1.c_str());
                              }   

         |VARIABLE              {	
         			$$.EXTRA.var_flag=1;//var_flag is to differentite integer and 
                                       				//float from others
                                    string code1;
                                    string place1;
                                    code1.append("");
                                    place1.append($1.place);
                                    $$.code=new char[code1.length()];
                                    strcpy($$.code,code1.c_str());
                                    $$.place=new char[strlen($1.place)];
                                    strcpy($$.place,$1.place);
		}
        |INTEG            {	$$.EXTRA.var_flag=3; //var_flag is to differentite integerand 
                                       				//float from others
                                    string code1;
                                    string place1;
                                    code1.append("");
                                    place1.append($1.place);
                                    $$.code=new char[code1.length()];
                                    strcpy($$.code,code1.c_str());
                                    $$.place=new char[place1.length()];
                                    strcpy($$.place,place1.c_str());
                               }
        |FLOAT 		{
        			$$.EXTRA.var_flag=3;//var_flag is to differentite integer and 
                                       				//float from others
                                    string code1;
                                    string place1;
                                    code1.append("");
                                    place1.append($1.place);
                                    $$.code=new char[code1.length()];
                                    strcpy($$.code,code1.c_str());
                                    $$.place=new char[place1.length()];
                                    strcpy($$.place,place1.c_str());
        					}
        ;

%% 

void yyerror(char *s) {
    fprintf(stderr, "%s\n", s);
} 
int main(void) { 
  return yyparse(); 
} 													

