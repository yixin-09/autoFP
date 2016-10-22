import os
import random
import time
import sys,getopt
import getname
import re

#gcc sqr2.c test.c -g -o sqr2

#set the interval
x=[0,2]

def bgrt():



now = time.time()
#archive the max double value
DOUBLE_MAX = 1000000.0
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
		exp_i2=random.uniform(get_bit(stval),get_bit(mxval))
		frand2=random.random()
		frand2=frand*pow(2,exp_i2)
		log.write(repr(frand)+" ")
		log.write(repr(frand2)+"\n")
		os.system("../run.sh "+k+" " + repr(frand) + repr(frand2))
		if os.path.isfile("statistic_relative_error_1"):
			os.system("cat statistic_relative_error_1>>log_"+k)
			os.system("cat max_location_1>>max_"+k)
			os.system("rm  statistic_relative_error_1")
			os.system("rm max_location_*")
		else:
			os.system("echo ' NaN'>>log_"+k)

startval = 0
#begin evalue for ten iter
def eva_times(n,itr,flag):
	global MAX_LINE
	if not os.path.exists(flag):
		os.mkdir(flag)
	else:
		os.system("rm -rf "+flag)
		os.mkdir(flag)
	k = flag
	dir_l = 'cp bench/'+flag+'.c '+flag
	dir_l2 = 'cp bench/'+'test.c '+flag
	dir_l3 = 'cp bench/'+flag+'.h '+flag
	os.system(dir_l)
	os.system(dir_l2)
	os.system(dir_l3)
	os.chdir(flag)
	os.system("gcc "+flag+".c test.c -g -lm -o "+flag)
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
	print "the avg error the tool found is :"+repr(avg_err)
	print "the intervel is:["+repr(tag_a)+','+repr(tag_b) + ']'
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
	os.system('./astop '+'--id ' + str(line_num) +' '+ 'bench/'+file_name)



def Usage():
    print 'autoFP usage:'
    print '-h,--help: print help message.'
    print '-v, --version: 1.0'
    print '-m, --mainiter: the number you want to iter for mainloop to get fault location,default 10'
    print '-s, --sampleiter: the number of sample point in each loop, default 1000'
    print '-f, --filename: the file to be repair'
    print '-i, --inputnum: the input num of the function'

def Version():
    print 'autoFP 1.0'

def main():

        opts, args = getopt.getopt(sys.argv[1:], "hvm:s:f:i:", ["help", "version","mainiter=","sampleiter=","filename="])

	main_iter = 10
	s_iter = 1000
	filename = ''
	inum = 1
	print opts
	for o, a in opts:
		if o in ('-h', '--help'):
            		Usage()
            		sys.exit(1)
        	elif o in ('-v', '--version'):
            		Version()
            		sys.exit(1)
        	elif o in ('-m', '--mainiter'):
            		main_iter = int(a)
        	elif o in ('-s', '--sampleiter'):
            		s_iter=int(a)
        	elif o in ('-f', '--filename'):
            		filename=a
		elif o in ('-i', '--inputnum'):
			inum = int(a)
        	else:
            		print 'unhandled option'
            		sys.exit(3)
	if (filename != '')&(os.path.exists('bench/'+filename)):
		stripf = re.sub('\.c','',filename)
		print stripf
		get_line=eva_times(main_iter,s_iter,stripf,inum)
		os.system('./rmtxt.sh')
		pass_to_astop(get_line,filename)
		to_herbie(stripf)
		re_load(stripf+'.output','bench/'+filename,get_line)
    	else:
		print "file no exist or empty"
     
if __name__ == "__main__":
    main()
#python run_analysis.py -m 2 -s 5 -f 'physic.c'



