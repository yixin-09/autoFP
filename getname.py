import re
def read_file(f,n):
	fi=open(f,'r')
	tab=fi.readlines()
	cobs=re.findall(r'\(.*\)',tab[n-1].strip())
	finame = re.findall(r'[^\(][a-zA-Z0-9]*.c',cobs[0])
	numi = re.findall(r'\:\d*',cobs[0])
	num = re.findall(r'[^\:]\d*',numi[0])
	return (finame[0],int(num[0]))

