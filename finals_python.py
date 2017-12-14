d = [ ("a", 10), ("b", 20), ("c", 30), ("a", 40) ]

def lookup(d,k):
	# only true case is needed
	return [x[1] for x in d if (x[0] == k)]

	# if true and false block is needed follow below style
	# return [x[1] if (x[0] == k) else x[0] for x in d]


# print(lookup(d,"a"))
# print(lookup(d,"b"))
# print(lookup(d,"c"))
# print(lookup(d,"d"))

def cond(b, t, f):
	if b: 
		return t
	else: 
		return f

def update(d,k,v):
	return [cond((x[0] == k), v, x[1]) for x in d]

# print(update(d, "a", "CSE130"))
# print(update(d, "b", "CSE130"))
# print(update(d, "d", "CSE130"))

def delete(d,k):
	return [x for x in d if x[0] != k]

def add(d,k,v):
	return [x for x in d] + [(k,v)]

#print(add(d, 100, 100))

def update2(d,k,v):
	temp = []
	for x in d:
		if(x[0] == k):
			temp.append((k,v))
		else:
			temp.append(x)
	return temp

#print(update2(d, "a", "CSE130"))
#print(update2(d, "b", "CSE130"))
#print(update2(d, "d", "CSE130"))

############################################

# normal decorators
def deco_temp(f):
	def g(*args, **kargs):
		res = f(*args, **kargs)
		# do what you want
		return res
	return g

class deco_temp:
	def __init__(self, f):
		self.f = f
		self.__name__ = f.__name__
	def __call__(self, *args, **kargs):
		res = self.f(*args, **kargs)
		# do something if you want
		return res

############################################

def in_range(i, range):
	def deco(f):
		def g(*args, **kargs):
			res = f(*args, **kargs)
			if i == -1:
				if res < range[0]:
					raise Exception("Exception: Return value " + str(res) + " too small")
				elif res > range[1]:
					raise Exception("Exception: Return value " + str(res) + " too big")
				else:
					return res
			else:
				if i < len(args):
					if args[i] < range[0]:
						raise Exception("Exception: " + str(i) + "th arg " + str(args[i]) + " too small")
					elif args[i] > range[1]:
						raise Exception("Exception: " + str(i) + "th arg " + str(args[i]) + " too big")
					else:
						return res
				else:
					raise Exception("Exception: invalid index")
		return g
	return deco
	

def in_range(i, range):
	class deco_temp:
		def __init__(self, f):
			self.f = f
			self.__name__ = f.__name__
		def __call__(self, *args, **kargs):
			res = self.f(*args, **kargs)
			if i == -1:
				if res < range[0]:
					raise Exception("Exception: Return value " + str(res) + " too small")
				elif res > range[1]:
					raise Exception("Exception: Return value " + str(res) + " too big")
				else:
					return res
			else:
				if i < len(args):
					if args[i] < range[0]:
						raise Exception("Exception: " + str(i) + "th arg " + str(args[i]) + " too small")
					elif args[i] > range[1]:
						raise Exception("Exception: " + str(i) + "th arg " + str(args[i]) + " too big")
					else:
						return res
				else:
					raise Exception("Exception: invalid index")
	return deco_temp

@in_range(0, (0,10))
@in_range(1, (-10, 20))	
def plus(a, b):
	return a+b

#print(plus(10,-5))
#print(plus(11,3))
#print(plus(-1,3))
#print(plus(2,25))
#print(plus(2,-13))

@in_range(-1, (5,10))
def plus2(a,b): return a+b

#print(plus2(6,4))
#print(plus2(3,2))
#print(plus2(6,5))
#print(plus2(3,1))

############################################

# spring 2013 finals

def rev(l):
	return [ e for e in l[::-1]]

l = [1,2,3,4,5,6]
#print(rev(l))

def sum(l):
	def fold_fn(acc,elm): 
		return acc + elm
	return reduce(fold_fn, l, 0)

#print(sum([1,2,3]))

def rev2(l):
	def fold_fn(acc, e):
		return [e] + acc
	return reduce(fold_fn, l, [])

#print(rev2(l))

def print_some(l):
	def deco(f):
		def g(*args):
			flag = 0
			for i in l:
				if i > -1:
					if i < len(args):
						print("Arg " + str(i) + " : " + str(args[i]))
				else:
					flag = 1
			res = f(*args)
			if flag == 1:
				print("Return: " + str(res))
			return res
		return g
	return deco

@print_some([-1,1,0])
def plus(x,y):
	print "-- plus called --"
	return x+y 

#print(plus(1,2))

@print_some([-2,100])
def plus2(x,y):
	print "-- plus2 called --"
	return x+y 

#print(plus2(1,2))

@print_some([-1,0])
def fac(n):
	print "-- fac called --"
	if n is 0: return 1
	else: return n * fac(n-1)

#print(fac(2))

def apply_to_tree(s,t):
	if not t.is_var():
		return node(t.name, [apply_to_tree(s, c) for c in t.children])
	elif t.name in s:
		return apply_to_tree(s, s[t.name])
	else:
		return t

def unify(a,b,s={}):
	a = apply_to_tree(s, a)
	b = apply_to_tree(s, b)
	result = s.copy()
	if a.is_var() and b.is_var(): result[a.name] = b
	elif a.is_var() and not b.is_var():
		if a.name in result: result = unify(result[a.name], b, result)
		else: result[a.name] = b
	elif not a.is_var() and b.is_var():
		return unify(b,a,s)
	elif not a.is_var() and not b.is_var():
		if len(a.children) != len(b.children): return False
		if a.name != b.name : return False
		for (c,d) in zip(a.children, b.children):
			result = unify( c, d, result)
			if result == False:
				return result
	return result

########################################################################################

# winter 2013 finals

def transpose(m):
	height = len(m)
	width = len(m[0])
	return [ [ m[j][i] for j in range(height) ] for i in range(width)]

#print(transpose([[ 1, 2, 3],[ 4, 5, 6]]))

def access(g, x, y):
	try: return g[y][x]
	except: return 0

def count_live_neighbours(g, x, y):
	live = 0
	for x_delta in [ 0 , 1 , -1 ]:
		for y_delta in [ 0 , 1 , -1 ]:
			if access(g, x+x_delta, y+y_delta) == 1:
				live = live + 1
	return live

def new_val(g, x, y):
	c = count_live_neighbours(g, x, y)
	if c < 2:
		return 0
	
	if c == 3 and access(g, x, y) == 0:
		return 1

	if c >= 2 and c <= 3:
		return access(g, x, y)

	if c > 3:
		return 0

	

def step(g):
	height = len(g)
	width = len(g[0])
	return [ [ new_val(g, i, j) for j in range(height)] for i in range(width)]


g = [[ 0, 0, 0, 0, 0 ],
	[ 0, 0, 0, 0, 0 ],
	[ 0, 1, 1, 1, 0 ],
	[ 0, 0, 0, 0, 0 ],
	[ 0, 0, 0, 0, 0 ]]

#print(g)
#print(step(g))
#print(step(step(g)))
#print(step(step(step(g))))

def lift_1(f):
	def decorated(x):
		return [ f(xx) for xx in x ]
	return decorated

@lift_1
def inc(x): return x+1

@lift_1
def sqr(x): return x*x

#print(inc([1,2,3]))
#print(sqr([1,2,3]))


def lift_2(f):
	def decorated(x,y):
		return [ f(a,b) for (a,b) in zip(x, y)]
	return decorated

@lift_2
def plus3(x,y): return x + y

@lift_2
def mul3(x,y): return x * y

#print(plus3([1,2,3,4], [4,5,6,7]))
#print(mul3([10,11], [2,3]))


def lift(f):
	def decorated(*args):
		return [ f(*x) for x in zip(*args) ]
	return decorated

@lift
def avg(a,b,c,d): return (a+b+c+d)/4.0

#print(avg([1,2], [5,6], [10,12], [4,8]))

##########################################################################################

# winter 2012 finals

img1 = [[ 11, 0, 12], [ 0, 0, 0], [ 13, 0, 14],[ 15, 16, 17]]

def square_img(img):
	return [ [ ele*ele for ele in l ] for l in img ]

#print(square_img(img1))

def crop_img(img,x1,y1,x2,y2):
	return [ [ col for col in row[x1:x2]] for row in img[y1:y2] ]

#print(crop_img(img1,0,1,2,4))

def zip(l1,l2):
	len1 = min(len(l1), len(l2))
	return [ (l1[i],l2[i]) for i in range(len1)]

#print(zip([1,2,3], [4,5,6]))
#print(zip([1,2,3], [4,5]))

def add_imgs(img1, img2):
	return [ [ a+b for (a,b) in zip(l1,l2)] for (l1,l2) in zip(img1,img2)]

#print(add_imgs(img1, img1))

def derivative(delta):
	def deco(f):
		def g(*args):
			a = f(args[0]+delta)
			b = f(args[0])
			res = (a - b)/(1.0*delta)
			return round(res, 2)
		return g
	return deco

@derivative(0.0001)
def double(x): return 2*x

#print(double(10.0))
#print(double(20.0))
#print(double(30.0))

@derivative(0.0001)
def square(x): return x*x

# print(square(10.0))
# print(square(20.0))
# print(square(30.0))


def derivative2(delta):
	class deco:
		def __init__(self, f):
			self.f = f
			self.__name__ = f.__name__
		def __call__(self, *args):
			a = self.f(args[0]+delta)
			b = self.f(args[0])
			res = (a - b)/(1.0*delta)
			return round(res, 2)
	return deco

@derivative2(0.0001)
def square(x): return x*x

# print(square(10.0))
# print(square(20.0))
# print(square(30.0))

class derivative3:
	def __init__(self, delta):
		self.delta = delta
	def __call__(self, f):
		class deco:
			def __init__(self1, f):
				self1.f = f
				self1.__name__ = f.__name__
			def __call__(self1, *args):
				a = self1.f(args[0]+self.delta)
				b = self1.f(args[0])
				res = (a - b)/(1.0*self.delta)
				return round(res, 2)
	 	return deco(f)


##########################################################################################

# winter 2011 finals

def print_first_k_args(k):
	def deco(f):
		def g(*args):
			if len(args) < k:
				i = 1
				for a in args:
					print("Arg " + str(i) + ": " + str(a))
					i = i + 1
			else:
				for i in range(k):
					print("Arg " + str(i+1) + ": " + str(args[i]))
			res = f(*args)
			print("Result: " + str(res))
			return res
		return g
	return deco

@print_first_k_args(1)
def sum(a,b): return a + b

@print_first_k_args(2)
def sum2(a,b): return a + b

@print_first_k_args(3)
def sum3(a,b): return a + b

@print_first_k_args(1)
def fac(n):
	if n <= 0: return 1
	else: return n*fac(n-1)

#print(sum(3,4))
#print(sum2(3,4))
#print(sum3(3,4))
#print(fac(3))


img1=[[255,255, 0, 0,255,255],
[255,255, 0, 0,255,255],
[ 0, 0,255, 0, 0, 0],
[ 0, 0,255,255, 0, 0],
[255, 0, 0, 0, 0, 255],
[ 0,255,255,255,255, 0]]

def create_image(w,h,c):
	#return [ [ c for col in range(w)] for row in range(h)]
	l = [c] * w
	return [l]*h
	
#print(create_image(3,2,27))

def well_formed(img):
	l = len(img[0])
	for row in img[1:]:
		if len(row) != l:
			return False
		for e in row:
			if e < 0 or e > 255:
				return False
	return True

#print(well_formed(img1))


def fill_rect(img,x0,y0,x1,y1,c):
	for y in range(y0, y1):
		for x in range(x0, x1):
			try:
				img[y][x] = c
			except Exception:
				pass
	return img

#print(fill_rect(img1, 0, 0, 5, 5, 24))


def fill_region(img, oldcolor, newcolor, x, y):
	img[y][x] = newcolor
	for (x1, y1) in [ (0,1), (-1, 0), (0,-1), (0,1)]:
		try:
			if img[y+y1][x+x1] == oldcolor and y+y1 != -1 and x+x1 != -1: fill_region(img, oldcolor, newcolor, x+x1, y+y1)
		except:
			pass

#before #after
img = [[ 0, 3, 0, 3, 0], [ 0, 3, 0, 3, 0], [ 0, 0, 3, 0, 0], [ 5, 0, 0, 0, 5],[ 0, 5, 5, 5, 0]]
#fill_region(img, 0,10,1,2)
#print(img)

##########################################################################################

# fall 2007 finals

'''
-> 5
a: (['a', 'b', 'c'], ['r', 2, 3])
b: (8990, -10)
c: (120, 0)
'''

# class deco():
# 	def __init__(self, f):
# 		self.f = f
# 		self.__name__ = f.__name__
# 		self.c = 0
# 	def __call__(self):
# 		self.f()
# 		self.c += 1
# 		return self.c

def deco(f):
	def g():
		g.counter += 1
		f()
		return g.counter
	g.counter = 0
	return g

@deco
def tick():
	return

# print(tick())
# print(tick())
# print(tick())
# print(tick())

def valid(es,c):
	for (x,y) in es:
		if c[x] == c[y]:
			return False
	return True

es = [(0,1),(1,2),(2,3),(3,0)]

# print(valid(es,[0,1,0,1]))
# print( valid(es,[0,0,1,1]))

def colorings(n,k):
	#return [ [ 1 if i & (1<<j) else 0 for j in range(n)] for i in range(pow(2, n))]

	for i in range(pow(2, n)):
		temp = []
		for j in range(n):
			if i & (1 << j):
				temp.append(1)
			else:
				temp.append(0)
		yield temp

# for c in colorings(3,2):
# 	print c


def initColoring(n):
	return [0 for i in range(n)]

def nextColoring(c,k):
	pass

def lastColoring(c,k):
	for e in c:
		if e != k:
			return False
	return True

class colorings:
	def __init__(self,n,k):
		self.colors = k
		self.vertices = n
		self.current = 0
	def next(self):
		if self.current == 0:
			self.current = initColoring(self.vertices)
		elif lastColoring(self.current,self.colors):
			raise StopIteration
		else:
			self.current = nextColoring(self.current,self.colors)
		return self.current
	def __iter__(self):
		return self


c = colorings(3,2)
# print(c.next())
# print(c.next())
# print(c.next())


def tracked(C):
	class CC(C): # CC extends C
		id_counter = 0
		objList = []
		def __init__(self,*args):
			CC.objList.append(C(*args))
			self.id = CC.id_counter
			CC.id_counter += 1
		def instId(self):
			return self.id
		def getInst(self, x):
			return CC.objList[x]
	return CC

class C:
	def __init__(self,v):
		self.x = v

C = tracked(C)
c1 = C("Jack")
c2 = C([0,1,2])
c3 = C(2004)
ids = [c1.instId(), c2.instId(), c3.instId()]
# print(ids)
# print([c1.getInst(id).x for id in ids])

def automap(f):
	def g(*args):
		if type(args[0]) == list:
			return [f(a) for a in args[0]]
		elif hasattr(args[0], "__iter__"):
			return [f(a) for a in args[0]]
		else:
			return f(*args)
	return g

@automap
def square(x):
	return x*x

# print(square(2))
# print(square([1,2,3,4,5]))
# print(square(tuple([1,2,3])))


############################################################

# fall 2005 finals


'''
5
a - 1
b - error - doesn't end
c - (0, 1031)
d - ['a', 'd', 'b', 'c']
e - 30

'''

# class A():
# 	def __init__(self):
# 		self.x = []
# 	def a(self):
# 		self.x += ["a"]
# 		self.d()

# class B(A):
# 	def b(self):
# 		self.x += ["b"]

# class C(A):
# 	def a(self):
# 		self.x += ["ca"]
# 	def c(self):
# 		self.x += ["c"]

# class D(B,C):
# 	def d(self):
# 		self.x += ["d"]
# 		self.b()
# 		self.c()

# o = D()
# o.a()
# ans = o.x
# print(ans)


def element_and_rest(l):
	for e in l:
		yield((e, [a for a in l if a != e]))

# for t in element_and_rest([1,2,3,4,5]):
# 	print(t)

def permutations(l):
	if len(l) <= 1:
		yield l
	else:
		for ele in permutations(l[1:]):
			for i in range(len(l)):
				yield ele[:i] + l[0:1] + ele[i:]

# for p in permutations([1,2,3]):
# 	print p








