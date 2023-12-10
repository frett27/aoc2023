#!/bin/python3


digit = ["one","two","three","four","five","six","seven","eight","nine","ten"]

def find(s,start,inc):
    i = start
    while i < len(s) and i >=0:
        for j in range(0,len(digit)):
            eval = s[i:i + len(digit[j])] 
            if eval == digit[j]:
                return str(j+1)
        
        if s[i].isdigit():
            return s[i]
        i += inc
    raise Exception("not found")

with open("input_day1.txt") as f:
    sum = 0
    lines = f.readlines()
    for l in lines:
        print("line " + str(l))
        value= (int(find(l,0,1) + find(l,len(l)-1,-1) ) )
        print(value)
        sum += value
    print(sum)
    


