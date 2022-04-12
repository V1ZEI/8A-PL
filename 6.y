%{
	#include<bits/stdc++.h>
	typedef long long ll;
	using namespace std;
	int yylex();
	void yyerror(char *s);
	
	//for 3addr
	int tempcount=0;
	int getTemp();
	typedef struct node
	{
	//	string  addr, array;
		pair<string,vector<ll> > type;
		vector<ll> trueList,falseList,nextList;
		int instNum;
	}NODE;

	NODE* makeNode()
	{
		NODE *temp=new NODE();
		ostringstream ss;
		ss<<"t"<<getTemp();
		temp->addr=ss.str();
		ss.str("");
		temp->array="";
		return temp;
	}
	NODE* makeNode(string *x)
	{
		NODE *temp=new NODE();
		temp->addr=*x;
		temp->array="";
		return temp;
	}

	//for symbol table
	deque<string> s;
	deque<int> vars;
	deque<int> blocks;
	int blocknum=-1,varnum=0;
	string currType;
	map<ll,map<string,pair<pair<string,vector<ll> >,string> > > table;//blockno,varname,type,dimensions,value
	map<string,ll> width;
	ll getWidth(pair<string,vector<ll> > type);
	string itoa(ll val);
	ll to_int(string val);
	void insertID(int bno,string varname);
	void printTable();

	// for backpatch
	int instCounter=0;
	map<ll,string> instList;
	int nextInst();
	vector<ll> merge(vector<ll> a,vector<ll> b);
	void backpatch(vector<ll> list,int num);
	void displayInst();

	/*for Control Flow Graph
	map<ll,vector<string> > basicBlock;
	set<ll> leaders;
	vector<vector<ll> > bbgraph;
	void genBasicBlock();
	void findLeaders();*/

%}

%union{
	typedef struct node NODE;
	string *str;
	NODE *node;

}

%token <str> KEY AND OR NOT RELOP ID VAL SC CM PL MI MUL DIV POW OP CL AO AC EQ BO BC UNR TRUE FALSE IF ELSE WHILE
%type <node> bool bb m n e t f g l r a if ifelse while stmt stmt_list block decl
%type <str>  d id arr ss 

%%

ss:		
		{
			blocknum++;
			blocks.push_back(blocknum);
			vars.push_back(varnum);
			varnum=0;
		}
		stmt_list {};

stmt:	if {$$=new NODE();$$->nextList=$1->nextList;} 
	|ifelse {$$=new NODE();$$->nextList=$1->nextList;}
	|while {$$=new NODE();$$->nextList=$1->nextList;}
	|block {$$=new NODE();$$->nextList=$1->nextList;}
	|a {$$=new NODE();$$->nextList=$1->nextList;}
	|decl{$$=new NODE();$$->nextList=$1->nextList;};

if:		IF OP bool CL m stmt {
								$$=new NODE();
								backpatch($3->trueList,$5->instNum);
								$$->nextList=merge($3->falseList,$6->nextList);
							};
ifelse: IF OP bool CL m stmt ELSE n m stmt {
											$$=new NODE();
											backpatch($3->trueList,$5->instNum);
											backpatch($3->falseList,$9->instNum);
											$$->nextList=merge($6->nextList,$8->nextList);
											$$->nextList=merge($$->nextList,$10->nextList);
										};
while:	WHILE m OP bool CL m stmt {
									$$=new NODE();
									backpatch($4->trueList,$6->instNum);
									backpatch($7->nextList,$2->instNum);
									instList[nextInst()]="goto "+itoa($2->instNum);
									$$->nextList=$4->falseList;
								};
block: BO	{
			blocknum++;
			blocks.push_back(blocknum);
			for(int i=0;i<s.size();i++)
			{
				insertID(blocknum,s[i]);
			}
			vars.push_back(varnum);
			varnum=0;
		} 
		stmt_list BC 
		{
			blocks.pop_back();
			for(int i=0;i<varnum;i++)
				s.pop_back();
			varnum=vars.back();
			vars.pop_back();	
			$$=new NODE();
			$$->nextList=$3->nextList;
		};
stmt_list: stmt_list m stmt {
								$$=new NODE();
								backpatch($1->nextList,$2->instNum);
								$$->nextList=$3->nextList;
							} |
			 stmt {$$->nextList=$1->nextList;} | {};
decl: KEY id {
				varnum++;s.push_back(*$1+" "+*$2);
				insertID(blocks.back(),*$1+" "+*$2);
				currType=*$1;
			} d SC {$$=new NODE();}; 

id:	ID arr{$$=new string(*$1+*$2);} | ID arr EQ VAL {$$=new string(*$1+*$2+*$3+*$4);};

arr:	AO VAL AC arr {$$=new string(*$1+*$2+*$3+*$4);}| {$$=new string("");};

d: CM id {
			varnum++;s.push_back(currType+" "+*$2);
			insertID(blocks.back(),currType+" "+*$2);
		} d | {};

a:	l EQ r SC {instList[nextInst()]=$1->array+"["+$1->addr+"] = "+$3->addr;	} 
	| ID EQ r SC 	{
	  					if(table[blocks.back()].find(*$1)==table[blocks.back()].end())
						{
							yyerror("variable not declared");
						}
	  					instList[nextInst()]=*$1+" = "+$3->addr;
	  					$$=new NODE();
	  				} ;
l: 	 ID AO e AC 
				{
					$$=makeNode();
					$$->array=*$1;
					if(table[blocks.back()].find(*$1)==table[blocks.back()].end())
					{
						yyerror("variable not declared");
					}
					$$->type=table[blocks.back()][*$1].first;
					instList[nextInst()]=$$->addr+" = "+$3->addr+" * "+itoa(getWidth($$->type));
				} 
	|l AO e AC
		 		{
		 			NODE *myTemp=makeNode();
		 			$$=makeNode();
		 			$$->array=$1->array;
		 			$$->type=$1->type;
		 			$$->type.second.erase($$->type.second.begin());
		 			instList[nextInst()]=myTemp->addr + " = "+$3->addr +" * "+itoa(getWidth($$->type));
		 			instList[nextInst()]=$$->addr+" = "+$1->addr+ " + " + myTemp->addr;
		 		}
		 		;
r : e 	{$$=$1;};

bool:	bool OR m bb 	{
					$$=new NODE();
					backpatch($1->falseList,$3->instNum);
					$$->trueList=merge($1->trueList,$4->trueList);
					$$->falseList=$4->falseList;
				} 
	|bool AND m bb	{
	 				$$=new NODE();
	 				backpatch($1->trueList,$3->instNum);
	 				$$->trueList=$4->trueList;
	 				$$->falseList=merge($1->falseList,$4->falseList);
				} 
	|bb {$$=new NODE();$$->trueList=$1->trueList;$$->falseList=$1->falseList;};

bb:	  NOT bb {$$=new NODE();$$->trueList=$2->falseList;$$->falseList=$2->trueList;}
	  |OP bool CL {$$=new NODE();$$->trueList=$2->trueList;$$->falseList=$2->falseList;}
	  |e RELOP e 	{
	    				$$=new NODE();
	    				$$->trueList.push_back(nextInst());
	    				instList[$$->trueList.back()]="if "+$1->addr+" "+*$2+" "+$3->addr+" goto";
	    				$$->falseList.push_back(nextInst());
	    				instList[$$->falseList.back()]="goto";
	    			}
	   |TRUE {$$=new NODE();$$->trueList.push_back(nextInst());instList[$$->trueList.back()]="goto";}
	   |FALSE {$$=new NODE();$$->falseList.push_back(nextInst());instList[$$->falseList.back()]="goto";};

m:	{$$=new NODE();$$=new NODE();$$->instNum=instCounter;};
n:	{$$=new NODE();$$->nextList.push_back(nextInst());instList[$$->nextList.back()]="goto";};

e : e PL t {$$=makeNode();instList[nextInst()]=$$->addr+" = "+$1->addr+" + "+$3->addr;}|
	 e MI t {$$=makeNode();instList[nextInst()]=$$->addr+" = "+$1->addr+" - "+$3->addr;}|
	  t {$$=$1;} |
	   l {$$=makeNode();instList[nextInst()]=$$->addr+" = "+$1->array+"["+$1->addr+"]";};

t : t MUL f {$$=makeNode();instList[nextInst()]=$$->addr+" = "+$1->addr+" * "+$3->addr;}|
	 t DIV f {$$=makeNode();instList[nextInst()]=$$->addr+" = "+$1->addr+" / "+$3->addr;}|
	  f {$$=$1;};

f : g POW f {$$=makeNode();instList[nextInst()]=$$->addr+" = "+$1->addr+" ^ "+$3->addr;}|
	 g {$$=$1;};

g : OP e CL {$$=$2;}|
	 ID {
	 		if(table[blocks.back()].find(*$1)==table[blocks.back()].end())
			{
				yyerror("variable not declared");
			}
			$$=makeNode($1);
		} |
	 l {
	 		$$=makeNode();instList[nextInst()]=$$->addr+" = "+$1->array+"["+$1->addr+"]";
	 	} |
	  VAL {$$=makeNode($1);} |
	   MI g {$$=makeNode();instList[nextInst()]=$$->addr+" = - "+$2->addr;};

%%
//for DAG
bool isInt(string s)
{
	bool flag=true;
	for(ll i=0;i<s.length();i++)
	{
		if(s[i]<'0' || s[i]>'9')
			flag=false;
	}
	return flag;
}
string getVarName(string var)
{
	return var.substr(0,var.rfind("_"));
}
void displayInst()
{
	cout<<"Three Address Statements:"<<endl;
	for(map<ll,string>::iterator it=instList.begin();it!=instList.end();it++)
	{
		cout<<it->first<<" "<<it->second<<endl;
	}
	cout<<endl;
}

int getTemp()
{
	return tempcount++;
}
int nextInst()
{
	return instCounter++;
}
vector<ll> merge(vector<ll> a,vector<ll> b)
{
	a.insert(a.end(),b.begin(),b.end());
	return a;
}
void backpatch(vector<ll> list,int num)
{
	for(ll i=0;i<list.size();i++)
	{
		if(instList[list[i]].find("goto")==instList[list[i]].length()-4)
		{
			instList[list[i]]+=" "+itoa(num);
		}
	}
}
void insertID(int bno,string varname)
{
	string type,value;
	int ind=varname.find(" ");
	if(ind!=-1)
	{
		type=varname.substr(0,ind);
		varname=varname.substr(ind+1);
	}
	ind=varname.find("=");
	value="";
	if(ind!=-1)
	{
		value=varname.substr(ind+1);
		varname=varname.substr(0,ind);
	}
	ind=varname.find("[");
	vector<ll> dims;
	if(ind!=-1)
	{
		string indexes=varname.substr(ind);
		varname=varname.substr(0,ind);
		for(ll i=0;i<indexes.length();i++)
		{
			if(indexes[i]=='[' || indexes[i]==']')
				indexes[i]=' ';
		}
		istringstream in(indexes.c_str());
		int dim;
		while(in>>dim)
		{
			dims.push_back(dim);
		}
	}
	if(table[bno].find(varname)!=table[bno].end())
	{
		yyerror("variable already declared");
	}
	table[bno][varname]=make_pair(make_pair(type,dims),value);
}
ll getWidth(pair<string,vector<ll> > type)
{
	ll ans=width[type.first];
	for(ll i=1;i<type.second.size();i++)
	{
		ans=ans*type.second[i];
	}
	return ans;
}
void setWidths()
{
	width["int"]=4;
	width["float"]=4;
	width["char"]=1;
	width["double"]=8;
}
string itoa(ll val)
{
	ostringstream ss;
	ss<<val;
	return ss.str();
}
ll to_int(string val)
{
	istringstream ss(val);
	ll x;
	ss>>x;
	return x;
}
void yyerror(char* s)
{
	cout<<s<<endl;
	exit(0);
}
int main()
{
	setWidths();
	yyparse();
	displayInst();
}

