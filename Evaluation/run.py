import os 
import subprocess
from subprocess import PIPE
import resource # for timing the subprocess
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import six
import time
import generate


my_env = os.environ.copy()
FNULL = open(os.devnull, 'w') # to hide console output of the subprocess

# remove outliers by removing anything which is > or < than the standard deviation (how spread data is around the mean)
# return the mean

def GetMean(df):
  df_clean = df[np.abs(df.Time - df.Time.mean()) <= (df.Time.std())]
  mean = df_clean.Time.mean()
  return mean

def RunToolOnce(mon):
  try: 
    usage_start = resource.getrusage(resource.RUSAGE_CHILDREN)
    command = "../Tool/main.native " + "\"" + mon + "\""
    subprocess.call(command, env=my_env, stderr=subprocess.STDOUT, timeout=18000, shell=True) # shell=True to print all output 
    usage_end = resource.getrusage(resource.RUSAGE_CHILDREN)
    time = usage_end.ru_utime - usage_start.ru_utime
  except:
    time=np.nan
  return time

# run the tool a number of times
# store each running time in a dataframe
# clean the dataframe and return the mean running time

def GetData(p):
  time_arr = []
  df = pd.DataFrame(columns=['Time'])
  for i in range (0, 5):
    print("running", i)
    t = RunToolOnce(p) 
    time_arr.append(t)
    if np.isnan(t):
      break     
  df['Time'] = time_arr
  return GetMean(df)

# get the list of generated properties
# specify the required number of repetitions by passing this value as a parameter
# run generate.py
# get the tool's mean running time 
# return a record with the mean running time for complexity 

def GetSMC(complexity, results, p):
  time_record = []  
  if p == 1: 
    output = generate.p1(complexity)
  else:  
    output = generate.p2(complexity) 
  time_record.append(float(GetData(output)))
  return time_record

def UpToComplexity(complexity, p):  
  results = pd.DataFrame(columns=['Time'])

  for i in range(1, complexity+1, 5):
    print("For complexity ", i)
    record = GetSMC(i, results, p)
    results.loc[len(results)] = record
  results.index += 1 
  return results

# This performs the random tests and saves the results to results_random.csv
def RandomTest():
  command = "../Tool/main.native " + "\"test\""
  subprocess.call(command, env=my_env, stderr=subprocess.STDOUT, timeout=18000, shell=True) # shell=True to print all output 


# PARAMETRISED TESTS - P1
# Note that presently, the formula size is hardcoded
results = UpToComplexity(200, 1)
results.insert(0, "Size", [130,780,1430,2080,2730,3380,4030,4680,5330,5980,6630,7280,7930,8580,9230,9880], True)
# print(results)
results.to_csv("results_parametrised_p1.csv")

# PARAMETRISED TESTS - P2
results = UpToComplexity(200, 2)
results.insert(0, "Size", [9,65,169,321,521,769,1065,1409,1801,2241,2729,3265,3849,4481,5161,5889,6665,7489,8361,9281], True)
# print(results)
results.to_csv("results_parametrised_p2.csv")

# RANDOM TESTS
RandomTest()


