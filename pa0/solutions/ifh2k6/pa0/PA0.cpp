// Ian Howell
// ifh2k6
// program reads a mathematical term in infix notation and 
// writes a stack machine program to evaluate the answer
//
// The algorithm follows Dijkstra's 'shunting yard algorithm'
// Basically, the string is read in backwards, printing out numbers as 
// it finds them, and throwing the operations onto a stack. When the program
// finds a left parenthesis, it spits back the top of the stack

#include <iostream>
#include <string>
#include <cstdio>
#include <cmath>
#include <stack>

int main()
{
	std::cout << "Enter a string:\n>>> ";
	std::stack<std::string> ops;
	std::string str = "";
	char c;
	// this loop's purpose is to immediately rid the string of spaces
	while ((c=getchar()) != '\n')
	{
		if (c == ' ')
			continue;
		str += c;
	}

	int openParens = 0; // used for error checking
	for (int i=str.size()-1; i>=0; i--)
	{
		if (openParens < 0) // if there are more closing parentheses than opening
		{
			std::cout << "Error: Parenthesis error\n";
			return 1;
		}

		// used for debugging...
		//std::cout << "SYMBOL: " << str[i] << '\n';

		switch (str[i])
		{
			case '+':
				ops.push("add");
				break;
			case '-':
				ops.push("sub");
				break;
			case '*':
				ops.push("mul");
				break;
			case '\\':
				ops.push("div");
				break;
			case '(':
				std::cout << ops.top() << '\n';
				ops.pop();
				openParens--;
				break;
			case ')':
				openParens++;
				break;
			//ignore these
			case ' ':
			case '\n':
				break;
			default:
				if (!isdigit(str[i])) // ignore numbers too
				{
					std::cout << "Error: Value entered is not contained within the alphabet\n";
					return 1;
				}
		}


		int num = 0;
		int degree = 0;
		bool isNum = false;
		// the following reads in each number character by character,
		// and builds the full number
		while (isdigit(str[i]))
		{
			num += (((int)str[i]-48) * pow(10, degree));
			degree++;
			i--;
			isNum = true;
		}
		if (isNum)
		{
			i++;
			std::cout << "Push " << num << '\n';	
			isNum = false;
		}


	}

	std::cout << "done\n";

	return 0;
}

