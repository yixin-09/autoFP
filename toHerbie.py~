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
	fn.close();
	return vr_dic

def search_var(ors,tags):
	pat_tags = '(?<!\w)'+tags+'(?!\w)'
	strinfo = re.search(r''+pat_tags+'',ors)
	if str(strinfo) == 'None':
		return False
	else:
		return True

def get_vr_expand (vr,dic):
	val_vr = ''
	if vr in dic.keys():
		val_vr = dic[vr]
		val_vr = '('+val_vr+')'
		while (h_key(val_vr,dic)):
			for i in dic.keys():
				print val_vr
				if search_var(val_vr,i):
					pat_i = '(?<!\w)'+i+'(?!\w)'
					strinfo = re.compile(r''+pat_i+'')
					val_vr = strinfo.sub('('+dic[i]+')',val_vr)
					
	return val_vr				
def h_key(val,dic):
	for i in dic.keys():
		if search_var(val,i):
			print 'get here'
			return True
	return False

def get_vr_list (filename):
	fn = open(filename,'r')
	vr_l = []
	for line in fn.readlines():
		vr_l.append(line.strip())
	fn.close()
	return vr_l
def get_exp (filename):
	fn  = open(filename,'r')
	exp_l = []
	for line in fn.readlines():
		exp_l.append(line.strip())
	fn.close()
	return exp_l

def multiple_replace(text, adict):  
     rx = re.compile('|'.join(map(re.escape, adict)))  
     def one_xlat(match):  
           return adict[match.group(0)]  
     return rx.sub(one_xlat, text) 

def to_expand (val_l,dic,exp):	
	call_l = []
	re_dic = {}
	for i in val_l:
		if (h_key(i,dic)):
			call_l.append(i)
	if len(call_l)==0:
		return exp
	else:
		for j in call_l:
			e_1 = get_vr_expand(j,dic)
			re_dic[j]=e_1
			pattern_str = '(?<!\w)'+j+'(?!\w)'
			strinfo = re.compile(pattern_str)
			exp = strinfo.sub(e_1,exp)
	return exp




def to_herbie(filename):
	val_l = get_vr_list('val.txt')
	exp = get_exp ('a.txt')
	if os.path.exists('exp.txt'):
		dic = call_table('exp.txt')
		exp_e = to_expand(val_l,dic,exp[0])
	else:
		dic = {}
		exp_e = exp[0]
	infix_name = '\'' + filename + '.fpcore\''
	print './infix.js -m ' + '\'' + exp_e + '\'' + ' -f ' + infix_name
	os.system('./infix.js -m ' + '\'' + exp_e + '\'' + ' -f ' + infix_name ) 
	os.system('racket herbie/src/herbie.rkt ' + filename+'.fpcore' + '>'+ filename+'.output')



