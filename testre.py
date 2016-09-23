import re

def match_var(ors,tags):
	pat_tags = '(?<!\w)'+tags+'(?!\w)'
	print pat_tags
	print ors
	strinfo = re.search(pat_tags,ors)
	if str(strinfo) == 'None':
		return False
	else:
		return True




if __name__ == '__main__':
	print match_var('(tmp___0 + tmp___1) + 1','tmp___1')
