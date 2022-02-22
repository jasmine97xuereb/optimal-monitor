import os 
import subprocess
from subprocess import PIPE
import resource # for timing the subprocess
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import six
import time
 

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
# get the tool's mean running time for each property in the list
# return a record with the mean running time for complexity 

def GetSMC(complexity, results):
  time_record = []  

  output = subprocess.check_output("python generate.py "+str(complexity), env=my_env, stderr=subprocess.STDOUT, shell=True)
  output = output.splitlines()
  p_lst = [e.decode('ascii') for e in output]

  for p in p_lst:
    i = p_lst.index(p) #column of the current property template in results
    time_record.append(float(GetData(p)))
  return time_record

def UpToComplexity(complexity):  
  results = pd.DataFrame(columns=['test'])

  for i in range(1, complexity+1, 5):
  # for i in range(1, complexity+1, 2):
    print("For complexity ", i)
    record = GetSMC(i, results)
    # print(record)
    results.loc[len(results)] = record
  results.index += 1 
  return results

results = UpToComplexity(200)
print(results)
# results.to_csv("RunningTimesBatch.csv")

