#Terry Ballou-Crawford
#14163546
#TFBBT8
#1/28/15
#assignment 1

import sys
import re

try:
    while 1:
        string = raw_input("Enter operations (or 'quit' to exit): ")
        if string in "quit":
            print("Exiting...")
            sys.exit(1)
        string = string.replace(" ", "")
        if "$" in string:
            print("syntax error encountered with character: $")
            sys.exit(0)
        numTokens = re.findall(r'\d+', string)
        string = re.sub(r'\d+', "$", string)
        
        #local variables for looping
        parenStack = []
        opStack = []
        printStack = []
        numStack = []
        numOps = 0
        i = 0
        numParen = 0
        
        try:
            #check if parentheses and operations match
            for c in string:
                if c is "(":
                    parenStack.append(c)
                    numParen += 1
                elif c is ")":
                    try:
                        parenStack.pop()
                    except IndexError:
                        raise ValueError("parentheses do not match")
                elif c in "\\" or c is "+" or c is "-" or c is "*":
                    numOps += 1
            if len(parenStack) > 0:
                raise ValueError("parentheses do not match")
            
            if numOps != numParen:
                raise ValueError("parentheses and operations do not match")
            
            index = 0
            temp = 1
            if numOps < 1:
                temp = 0
                numOps = 2
                
            tempi = i
            
            #loop at least once to check all characters
            for num in range(0,numOps):
                i = tempi
                for c in string:
                    if c is ")":
                        if len(parenStack)-1 == num:
                            printStack.append(opStack.pop())
                            for o in numStack:
                                printStack.append(o)
                            numStack = []
                        parenStack.pop()
                    elif c is "(":
                        parenStack.append(c)
                    elif c is "$":
                        if len(parenStack)-1 == num:
                            if 10 >= len(numTokens[i]):
                                numStack.append("push " + numTokens[i])
                            else:
                                raise ValueError("number " + numTokens[i] + " greater than 9 digits")
                            i += 1
                        else:
                            i += 1
                    elif c in "\\":
                        if len(parenStack)-1 == num:
                            opStack.append("div")
                    elif c is "+":
                        if len(parenStack)-1 == num:
                            opStack.append("add")
                    elif c is "-":
                        if len(parenStack)-1 == num:
                            opStack.append("sub")
                    elif c is "*":
                        if len(parenStack)-1 == num:
                            opStack.append("mul")
                    else:
                        raise ValueError("syntax error encountered with character " + c + " in the " + str(index) + " position")
                    index += 1
                
            #printing stack
            while len(printStack) > 0:
                print(printStack.pop())
                
            #if no operations, print token
            if temp is 0:
                print("push " + numTokens[0])
        
        #catch any exceptions
        except ValueError as err:
            print("Error: " + str(err))
            sys.exit(0)
            
        #program success
        print("done")
        
except Exception as err:
    print("Syntax error in input, exiting...")
    sys.exit(0)
sys.exit(1)
