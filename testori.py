import multiprocessing
import os
import random
import time
import sys

k='test2'
j=0
now = time.time()
#archive the max double value
double_max = (sys.float_info.max)/9000
startval = 0
def get_max(f):
	fi=open(f,'r')
	temp = 0.0
	for line in fi.readlines():
		line=line.strip()
		s = float(line)
		if temp < s:
			temp = s
	return temp
def get_avg(f):
	fi=open(f,'r')
	temp = 0.0
	n=0
	for line in fi.readlines():
		n=n+1
		line=line.strip()
		s = float(line)
		temp = temp + s
	return temp/n
flag = k
os.mkdir(flag)
dir_l = 'cp '+flag+'.c '+flag
print dir_l
os.system(dir_l)
os.chdir(flag)
file_log=open("log_s_"+k,"wb")
file_log2=open("log_s2_"+k,"wb")
#begin evalue for ten iter
for n in (0,10):
	#file_logrel=open("log_"+k,"wb")
	#find mid value between [0,double_max]
	midvalue = ((double_max-startval)/2.0)+startval
	#test and evaluation to get the max relative-erro of the two interval [0,midvalue], [midvalue,double_max]
	#every interval get 1000 sample point for test
	for i in range(0,1000):
		i = i+1
		#random get a point in the interval
		frand=random.uniform(startval,midvalue)
		file_log.write(repr(frand)+"\n")
		#run the frand to get the relative-error
		os.system("../gsl_fun.sh "+flag+" " + repr(frand))
		if os.path.isfile("statistic_relative_error_1"):
			os.system("cat statistic_relative_error_1>>log_"+k)
			os.system("rm statistic*") 
		else:
			os.system("echo ' NaN'>>log_"+k)
	#get the max relative error and avg relative error
	maxval = get_max("log_"+k)
	avgval = get_avg("log_"+k)
        for i in range(0,1000):
		i = i+1
		#random get a point in the interval
		file_log2.write(repr(frand)+"\n")
		frand=random.uniform(midvalue,double_max)
		#run the frand to get the relative-error
		os.system("../gsl_fun.sh "+flag+" " + repr(frand))
		if os.path.isfile("statistic_relative_error_1"):
			os.system("cat statistic_relative_error_1>>log2_"+k)
			os.system("rm statistic*") 
		else:
			os.system("echo ' NaN'>>log2_"+k)
	#get the max relative error and avg relative error
	maxval2 = get_max("log2_"+k)
	avgval2 = get_avg("log2_"+k)
	if avgval2 > avgval :
		startval = midvalue
	else:
		double_max = midvalue
	n=n+1
os.system("rm "+ flag + "_*_*")
os.chdir('../')
if avgval2>avgval:
	print "avgval2"
	print midvalue
	print double_max
	print avgval2
	print maxval2
else:
	print "avgval"
	print startval
	print midvalue
	print avgval
	print maxval

file_log.close()
file_log2.close()
#file_logrel.close()
st_l="echo "+repr(time.time()-now)+">>log_time"
print st_l
os.system(st_l)
print "time delta: %s" % (time.time()-now)

