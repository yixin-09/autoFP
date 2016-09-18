import os
import re


def call_table(filename):
	vr_dic = {}
	exp_l = []
	vr_l = []
	fn = open(filename,'r')
	i = 0
	for line in fn.readlines():
		
		if line.find("variable")>-1:
			li = line.split(" ")
			vr_l.append(li[1].strip())
			if len(exp_l) > 1 :
				exp_str = exp_l[0]
				exp_l.pop(0)
				exp_str = exp_str + '('+','.join(exp_l)+')'
			else:
				exp_str = ''.join(exp_l) 
			if i == 0:
				vr_dic[vr_l[i]] = exp_str
			else:
				vr_dic[vr_l[i-1]] = exp_str
			i = i + 1
			exp_l = []
		else:
			exp_l.append(line.strip())
	exp_str = exp_l[0]
	exp_l.pop(0)
	exp_str = exp_str + '('+','.join(exp_l)+')'	
	vr_dic[vr_l[i-1]] = exp_str
	return vr_dic

def get_vr_expand (vr,dic):
	if vr in dic.keys():
		val_vr = dic[vr]
		while (h_key(val_vr,dic)):
			for i in dic.keys():
				if val_vr.find(i) > -1:
					strinfo = re.compile(i)
					print i
					val_vr = strinfo.sub(dic[i],val_vr)
	print val_vr				
def h_key(val,dic):
	for i in dic.keys():
		if val.find(i) > -1:
			return True
	return False

def get_vr_list (filename):
	fn = open(filename,'r')
	vr_l = []
	for line in fn.readlines():
		vr_l.append(line.strip())
	print vr_l

	


if __name__ == '__main__':
	dic = call_table('exp.txt')
	get_vr_expand('f',dic)
	get_vr_list('val.txt')