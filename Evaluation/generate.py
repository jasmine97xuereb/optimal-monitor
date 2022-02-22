from sys import argv
import math

try:
  complexity = int(argv[1])
except:  
  print("Enter Complexity: ") 
  complexity = int(input())


# ----------------------------------------- EXAMPLE 1 ----------------------------------------- 

final = "max X."

for i in range(complexity*10): 
  var = "a" + str(i)
  final = final + "(<" + var + ">[" + var + "]ff & <" + var + ">X & [" + var + "]([" + var + "]ff | X)) & "
final = final[:-3]
print(final)


# ----------------------------------------- EXAMPLE 2 ----------------------------------------- 

final = ""

for i in range(complexity): 
  inner = ""
  var1 = "b" + str(i)
  rec = "X" + str(i)
  for i in range(complexity): 
    var2 = "a" + str(i)
    inner = inner + "[" + var2 + "]" + rec + " & <" + var2 + ">" + rec + " & "
  inner = inner + "[" + var1 + "]ff "
  final = final + "(min " + rec + "." + inner + ") | " 
final = final[:-3]
print(final)