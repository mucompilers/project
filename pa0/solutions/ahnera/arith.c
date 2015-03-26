#include <iostream>

using namespace std;

const char * expressionToParse = "(6-2)*10/5+2*5";

char peek()
{
    return *expressionToParse;
}

char get()
{
    return *expressionToParse++;
}

double expression();

double number()
{
    double result = get() - '0';
    while (peek() >= '0' && peek() <= '9')
    {
        result = 10*result + get() - '0';
    }
    return result;
}

double factor()
{
    if (peek() >= '0' && peek() <= '9')
        return number();
    else if (peek() == '(')
    {
        get(); // '('
        double result = expression();
        get(); // ')'
        return result;
    }
    else if (peek() == '-')
    {
        get();
        return -expression();
    }
    return 0; // error
}

double term()
{
    double result = factor();
    while (peek() == '*' || peek() == '/')
        if (get() == '*')
            result *= factor();
        else
            result /= factor();
    return result;
}

double expression()
{
    double result = term();
    while (peek() == '+' || peek() == '-')
        if (get() == '+')
            result += term();
        else
            result -= term();
    return result;
}

int main(int argc, char* argv[])
{

    double result = expression();
    cout << result << endl;
    return 0;
}