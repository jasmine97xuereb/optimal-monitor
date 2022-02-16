from sys import argv
import math

try:
  complexity = int(argv[1])
except:  
  print("Enter Complexity: ") 
  complexity = int(input())


# --------------------------------- WORKING EXAMPLE 3    /\ [a]^i [b]ff     --- OK UNTIL 15 

# final = ""
# ext_diamond = "((<a>[a]ff & <a>[b]ff) | "
# ext_box = "(([a][a]ff & [a][b]ff) | "
# final = "(<a>[b]ff & [a][b]ff & [b]ff)"

# for i in range(complexity-1):
#   temp1 = ext_diamond + final
#   temp2 = ext_box + final
#   final = "(" + temp1 + ")) & (" + temp2 + "))" 
# final = "[a]ff | " + final


# ----------------------      \/ --a->*i [b]ff      --- OVERFLOW AT 10

# final = ""
# fix = "(<a>[b]ff & [a][b]ff)"
# final = fix 

# for i in range(complexity):
#   final = final + " | ((<a>" + final + ") & ([a]" + final + "))"
# # print(final)


#  ----------------- \/ -a->*n [b_i]ff for a fixed n and i (THIS IS POSSIBLY OK)

# final = ""
# disjunct = "" 

# for x in range(complexity):
#   disjunct = "[b" + str(x) + "]ff"
#   for i in range(complexity):
#     disjunct = "<a>(" + disjunct + ") & [a](" + disjunct + ")"
#   if complexity == 1:
#     final = disjunct + " | "
#   else:
#     final = "(" + disjunct + ") | " + final
# final = final[:-3]
# # print(final)


#  ----------------- \/ [a]^n [b_i]ff for a fixed n and i (TESTING)

# final = ""
# disjunct = "" 

# for x in range(complexity):
#   disjunct = "[b" + str(x) + "]ff"
#   for i in range(complexity):
#     disjunct = "[a]ff | (<a>(" + disjunct + ") & [a](" + disjunct + "))"
#   if complexity == 1:
#     final = disjunct + " | "
#   else:
#     final = "(" + disjunct + ") | " + final
# final = final[:-3]
# # print(final)


#  ----------------- \/ [a]^n [b_i]ff for a fixed n and i (GOOD FOR OUR EXAMPLE)

# final = ""
# disjunct = "" 
# lst = []

# for x in range(complexity):
#   disjunct = "[b" + str(x) + "]ff"
#   for i in range(complexity):
#     disjunct = "[a]ff | [a](" + disjunct + ")"
#   # if complexity == 1:
#     # final = disjunct + " | " 
#   # else:
#     # final = "(" + disjunct + ") | " + final
#   lst = lst + ["("+disjunct+")"]
# final = ' | '.join(lst) 
# # final = final[:-3]
# # print(final)

final = ""
disjunct = "" 
lst = []

for x in range(complexity):
  disjunct = "[b" + str(x) + "]ff"
  for i in range(complexity*5):
  # for i in range(complexity):
    disjunct = "[a]ff | [a](" + disjunct + ")"
  lst = lst + ["("+disjunct+")"]
final = ' | '.join(lst) 
# print(final)


# -------------------  NO GOOD BECAUSE 1 UNFOLDING AS WELL -----   \/ max X_i . [a]^n([b_i]ff /\ X_i) for fixed n

# final = ""

# for x in range(complexity):
#   inner = "(<a>X & [a]X)"
#   outer = ""

#   for i in range(complexity-1):
#     inner = "<a>([a]ff | " + inner + ") & [a]([a]ff | " + inner + ")"
#   inner = "max X.([a]ff & [b" + str(x) + "]ff) | ([b" + str(x) + "]ff & " + inner + ")"

#   outer = inner 

#   for i in range(complexity):
#     outer = "[a]ff | (<a>(" + outer + ") & [a](" + outer + "))"

#   if x == 0:
#     final = outer + " | "
#   else:
#     final = final + "(" + outer + ") | "

# final = final[:-3]
# # print(final)



# ---------------------- WITH FIX POINTS - SAME PROBLEM HERE    max X.[a]^i([b]ff /\ X)
# ADD & <c>tt TO MAKE IT NOT MONITORABLE 

# final = ""
# inner = "(<a>X & [a]X)"

# for i in range(complexity-1):
#   inner = "<a>([a]ff | " + inner + ") & [a]([a]ff | " + inner + ")"
# inner = "max X.([a]ff & [b]ff) | ([b]ff & " + inner + ")"
# # print(inner)

# outer = inner 

# for i in range(complexity):
#   outer = "[a]ff | (<a>(" + outer + ") & [a](" + outer + "))"
# # print(outer)


# -------------------  STACK OVERFLOW AT 8 --- EXCELLENT EXAMPLE (multiple unfolding) -----   \/ max X_i . [a]^i([b]ff /\ X_i)

# for x in range(complexity):
#   inner = "(<a>X & [a]X)"
#   outer = ""

#   for i in range(x):
#     inner = "<a>([a]ff | " + inner + ") & [a]([a]ff | " + inner + ")"
#   inner = "max X.([a]ff & [b]ff) | ([b]ff & " + inner + ")"
#   # print(inner)

#   outer = inner 

#   for i in range(x+1):
#     outer = "[a]ff | (<a>(" + outer + ") & [a](" + outer + "))"
#   # print(outer)

#   if x == 0:
#     final = outer + " | "
#   else:
#     final = final + "(" + outer + ") | "

# final = final[:-3]
# # print(final)


# ----------------------------------------- EXAMPLE 1 ----------------------------------------- 
# This generates formulas of the form max X. -a1-> { -a1-> empty, X} & ... & -an-> { -an-> empty, X}

final = "max X."

for i in range(complexity*10): 
# for i in range(complexity): 
  var = "a" + str(i)
  final = final + "(<" + var + ">[" + var + "]ff & <" + var + ">X & [" + var + "]([" + var + "]ff | X)) & "
final = final[:-3]
# print(final)


# ----------------------------------------- EXAMPLE 2 NOT GOOD ----------------------------------------- 

final = ""


for i in range(complexity): 
  inner = ""
  var1 = "b" + str(i)
  rec = "X" + str(i)
  # for i in range(complexity): 
  for i in range(complexity): 
    var2 = "a" + str(i)
    inner = inner + "[" + var2 + "]" + rec + " & <" + var2 + ">" + rec + " & "
  inner = inner + "[" + var1 + "]ff "

# for i in range(complexity): 
  final = final + "(min " + rec + "." + inner + ") | " 
  # final = final + "(min " + rec + ".<a>" + rec + " & [a]" + rec +  " & <b>" + rec + " & [b]" + rec + " & [" + var + "]ff" + " & [" + var + "]ff) | "
  # final = final + "(<" + var + ">[" + var + "]ff & <" + var + ">X & [" + var + "]([" + var + "]ff | X)) & "
final = final[:-3]
print(final)