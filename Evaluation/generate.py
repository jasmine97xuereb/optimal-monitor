from sys import argv
import math

# ----------------------------------------- EXAMPLE 1 ----------------------------------------- 

def p1(complexity):
    
  final = "max X."

  for i in range(complexity*10): 
    var = "a" + str(i)
    final = final + "(<" + var + ">[" + var + "]ff & <" + var + ">X & [" + var + "]([" + var + "]ff | X)) & "
  final = final[:-3]
  print(final)
  return final


# ----------------------------------------- EXAMPLE 2 ----------------------------------------- 


def p2(complexity): 
  final = ""

  for i in range(complexity): 
    inner = ""
    var1 = "b" + str(i)
    rec = "X" + str(i)
    for i in range(complexity): 
      var2 = "a" + str(i)
      inner = inner + "[" + var2 + "]" + rec + " & <" + var2 + ">" + rec + " & "
    inner = inner + "[" + var1 + "]ff "
    if complexity == 1: 
      final = "min " + rec + "." + inner + " | "
    else:
      final = final + "(min " + rec + "." + inner + ") | " 
  final = final[:-3]
  print(final)
  return final 


# Uncomment to use as a standalone

# try:
#   complexity = int(argv[1])
# except:  
#   print("Enter Complexity: ") 
#   complexity = int(input())
# p1(complexity)
# p2(complexity)