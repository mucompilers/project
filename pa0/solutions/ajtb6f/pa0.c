/* Andrew Todd, solution based on railroad shunting algorithm*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

//**strrev**public domain:  Written by Bob Stout (Included here because not available in glibc) 
char *strrev(char *str)
{
      	char *p1, *p2;

      	if (! str || ! *str)
            	return str;
     	for (p1 = str, p2 = str + strlen(str) - 1; p2 > p1; ++p1, --p2)
      	{
            	*p1 ^= *p2;
            	*p2 ^= *p1;
        	*p1 ^= *p2;
     	 }
	return str;
}

char *parent_flip(char *str){
	int i;
	int len = strlen(str)-1;
	for(i = 0; i < len; i++){
		if(str[i] == '(') { 
			str[i] = ')';
		}
		else if(str[i] == ')') { 
			str[i] = '(';
		}
	}
	return str;
}

char *rev_numbers(char *str){//written by Andrew Todd, adapted from Bob Stout to reverse only multidigit numbers
	int i = 0;
	int j = 0;
	char *p1, *p2;
	int len = strlen(str)-1;
	for(i=0; i<len; i++){
		while(isdigit(str[i+j])){
			j++;	
		}
		for(p1 = (str + i), p2 = (str + i) + j - 1; p2 > p1; ++p1, --p2){
			*p1 ^= *p2;
                	*p2 ^= *p1;
                	*p1 ^= *p2;

		}
	i += j;
	j=0;
	}
	return str;
}

int get_precedence(char op){
	if((op == '+')||(op == '-')){ return 2;};
	if((op == '*')||(op == '/')){ return 1;};
	return 0;
	//printf("Error:  Operator %c not yet supported.\n", op);
}

int main(int argc, char **argv) {
	
	/***Generate the Bytestream***/
	char * bytestream;
	char * error;
	bytestream = (char *)malloc(1);
	bytestream[0] = '#';
	int count = 1;//master while counter
	char sml_list[120];//Stack machine instruction list
	int i,j;//for counters
	for(j=0; j<120; j++){sml_list[j] = -1;};// set all to -1 for sentineling
	char op_list[120];//Operator stack
	//printf("argc is %d\n",argc);
        
	if(argc == 1){
 		printf("Usage: pa0 \"mathematical expression\"  (e.g. ./pa0 \"(3+4)\")\n");
        exit(-1);
         };
	while(count < argc){//this loop will allow several expressions to be compiled at once in later version
		bytestream = (char *)realloc(bytestream, (strlen(bytestream)-1 + strlen(argv[count])));
		strcat(bytestream, argv[count]);
		strcat(bytestream, "#");	
		count++;
	}

        int bytestream_payload_length = strlen(bytestream) - argc;
        printf("The bytestream contains %d characters to be processed.\n", bytestream_payload_length);
	puts(bytestream);
	
	/*In order to use the "railroad shunting algorithm" the following operations must be done*/
	bytestream = strrev(bytestream);//reverse bytestream
	bytestream = parent_flip(bytestream);//change direction of parenthesis
	bytestream = rev_numbers(bytestream);//un-reverse multidigit numbers

	/***Below is the railroad shunt yard algorithm (courtesy of Djikstra) complete with two whiles***/
	/***This section effectively lexes and parses the specially processed bytestream***/
	count = bytestream_payload_length;
	int bindex = 0;
	int sml_index = 0;
	int op_index = 0;
	int op_top = 0;
	int halt = 0;
	int operator_count=0;
	int num_count=0;
	char c;
	while(count > 0){
		c = bytestream[bindex];
		switch(c){
			case  '#': break;//don't count these, since they are just used as delimiters
			case  ' ': count--; break;//count spaces and newlines because they must be dealt with
			case '\n': count--; break;
			case  '0': case  '1': case  '2': case  '3': case  '4':
			case  '5': case  '6': case  '7': case  '8': case  '9':
				while(isdigit(c)){//while to grab multidigit numbers
                                	sml_list[sml_index] = c;
					sml_index++;
                                	count--;
					bindex++;
					c = bytestream[bindex];
				}
				sml_list[sml_index] = '~';	
				sml_index++;
	                        num_count++;
				bindex--;
				break;
			case  '+': case  '-': case  '*': case  '/'://keep track of # operators, also pop each onto stack based on precedence
				operator_count++; 
                                while((op_index > 0 ) && (op_list[op_top] != '(') && (get_precedence(c)>get_precedence(op_list[op_top]))){
					if(op_top>=0){
						sml_list[sml_index] = op_list[op_top];
						sml_index++;
						op_top--;
						op_index--;
					}
					else {error = "Out of bounds!\n"; halt=1;exit(-1); break;}
				}
				op_list[op_index]=c;
				op_top = op_index;
				op_index++;
                                count--; break;
			case  '('://pop left parents onto the stack
				op_list[op_index]=c;
				op_top = op_index;
				op_index++;
				count--; break;
			case  ')': //baktrack from right parenthesis, popping and outputing from stack as needed until left paren
				while(op_list[op_top] != '('){
					if(op_top>=0){
						sml_list[sml_index] = op_list[op_top];
						sml_index++;
						op_top--;
						op_index--;
					}
					else {error = "Parenthesis Mismatch!\n"; halt=1; break;}
				}
	
				op_top--; //pop left parent off stack and discard
				op_index--;

				if((op_list[0]!='(')&&(op_index<=0)){//happens if no matching left paren is found
					halt=1;
					error = "Parenthesis Mismatch!\n";	
				}
				count--; break;
			default:
				error = "Unknown character!\n";
				halt=1;
				count--;				
		}
		bindex++;
		if(halt == 1) {	puts(error); exit(-1);}
	}
if(num_count<=operator_count){printf("Too many operators or too few numbers!\n");exit(-1);};
if(num_count-1>operator_count){printf("Too few operators or too many numbers!\n");exit(-1);};

while(op_index > 0){
	if(op_list[op_top]=='('){//cataches a stray left parent
		halt=1;
		error = "Parenthesis Mismatch!\n";
	}
	if(halt == 1) { puts(error); exit(-1);}
	sml_list[sml_index] = op_list[op_top];
	sml_index++;
	op_top--;
	op_index--;
}

for(i=0;i<120;i++){
                c = sml_list[i];
                switch(c){
                        case  '0': case  '1': case  '2': case  '3': case  '4':
                        case  '5': case  '6': case  '7': case  '8': case  '9':
                                printf("Push ");
				while(sml_list[i]!= '~'){
					printf("%c",c);
					i++;
					c = sml_list[i];
				}
				printf("\n");
				break;
                        case  '+':printf("add\n"); break;
			case  '-':printf("sub\n"); break;
			case  '*':printf("mul\n"); break;
			case  '/':printf("div\n"); break;
                        }
	}
printf("done\n");

	return 0;
}
