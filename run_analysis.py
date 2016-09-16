#The file do tree things: 1)get the filename. 2)run the fpdebug to get the max relative error 3)get the line_num that cause the max_relative error
import os
import random
import time
import sys
import getname

now = time.time()
#archive the max double value
DOUBLE_MAX = (sys.float_info.max)/9000
MAX_LINE = 1
def get_max(f):
	global MAX_LINE
	fi=open(f,'r')
	temp = 0.0
	line_num = 0
	for line in fi.readlines():
		line=line.strip()
		s = float(line)
		line_num = line_num + 1
		if temp < s:
			temp = s
			MAX_LINE = line_num
			print MAX_LINE
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
def get_bit(num):
	n=0
	result = 1
	int_num = int(num)
	while int_num !=0:
		result = int(int_num % 2)
		int_num = int_num/2
		n=n+1
	return n

#run run.sh to get the relative_error
def run_re(n,stval,mxval,log,k):
	i=0
	for i in range(0,n):
		exp_i=random.uniform(get_bit(stval),get_bit(mxval))
		frand=random.random()
		frand=frand*pow(2,exp_i)
		log.write(repr(frand)+"\n")
		os.system("../gsl_fun.sh "+k+" " + repr(frand))
		if os.path.isfile("statistic_relative_error_1"):
			os.system("cat statistic_relative_error_1>>log_"+k)
			os.system("cat max_location_1>>max_"+k)
			os.system("rm statistic*") 
			os.system("rm max_location_*")
		else:
			os.system("echo ' NaN'>>log_"+k)

startval = 0
#begin evalue for ten iter
def eva_times(n,itr,flag):

	global MAX_LINE
	os.mkdir(flag)
	k = flag
	dir_l = 'cp '+flag+'.c '+flag
	os.system(dir_l)
	
	os.chdir(flag)
	os.system("gcc "+flag+".c -g -o "+flag)
	#log_the_input value
	file_log=open("log_s_"+k,"wb")
	file_log2=open("log_s2_"+k,"wb")

	stval = startval
	maxval = DOUBLE_MAX


	for i in range(0,n):
		#find mid value between [0,DOUBLE_MAX]
		midval = ((maxval-stval)/2.0)+stval
		#test and evaluation to get the max relative-erro of the two interval [0,midvalue], [midvalue,DOUBLE_MAX]
		run_re(itr,startval,midval,file_log,k)
	        avgval = get_avg("log_"+k)
		os.system("rm "+ "log_"+k)
		run_re(itr,midval,maxval,file_log2,k)
		avgval2 = get_avg("log_"+k)
		os.system("rm "+ "log_"+k)
		if avgval2 > avgval :
			stval = midval
		else:
			maxval = midval


	if avgval2>avgval:
		print "avgval2"
		print midval
		print maxval
		print avgval2
		tag_a = midval
		tag_b = maxval
	else:
		print "avgval"
		print stval
		print midval
		print avgval
		tag_a =stval
		tag_b = midval

	os.system("rm "+ "max_"+k)
	run_re(10,tag_a,tag_b,file_log,k)
	max_err = get_max("log_"+k)
	avg_err = get_avg("log_"+k)
	print "the max error the tool found is :"+repr(max_err)
	read=getname.read_file('max_'+k,MAX_LINE)
	print MAX_LINE
	print read[0]
	print read[1]
	os.system("rm "+ flag + "_*_*")
	os.chdir('../')
	file_log.close()
	file_log2.close()
	st_l="echo "+repr(time.time()-now)+">>log_time"
	print st_l
	os.system(st_l)
	print "time delta: %s" % (time.time()-now)
	return read[1]

def pass_to_astop(line_num,file_name):
	os.system('./astop '+'--id ' + str(line_num) +' '+ file_name)

def cover_to_herbie(file_name):
	fi=open(file_name,'r')
	for line in fi.readlines():
		


if __name__ == '__main__':
	get_line=eva_times(2,5,'float')
	pass_to_astop(get_line,'float.c')
	covert_to_herbie('a.txt')


