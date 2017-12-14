from misc import Failure

class Vector(object):
	"""
	The Vector class will be a fixed length vector which implements a variety of operations.
	"""
	def __init__(self, l):
		"""
		The constructor should take a single argument. If this argument is either an 
	    int or a long or an instance of a class derived from one of these, then consider 
	    this argument to be the length of the Vector. In this case, construct a Vector of 
	    the specified length with each element is initialized to 0.0. If the length is 
	    negative, raise a ValueError with an appropriate message. If the argument is not 
	    considered to be the length, then if the argument is a sequence (such as a list), 
	    then initialize with vector with the length and values of the given sequence. If 
	    the argument is not used as the length of the vector and if it is not a sequence, 
	    then raise a TypeError with an appropriate message.
	    """
		try:
			if l < 0:
				raise ValueError("Vector length cannot be negative")
		except TypeError:
			pass
		try:
			self.v = [0.0] * l
			self.len = l
		except Exception:
			self.v = list(l)
			self.len = len(self.v)

	def __repr__(self):
		"""
		returns a string of python code which could be used to initialize the Vector. 
		This string of code should consist of the name of the class followed by an open 
		parenthesis followed by the contents of the vector represented as a list followed 
		by a close parenthisis.
		"""
		return "Vector(" + repr(self.v) + ")"

	def __len__(self):
		"""
		returns the length of the Vector
		"""
		return self.len

	def __iter__(self):
		"""
		returns an element one at a time for iteration over all elements of the Vector
		"""
		for ele in self.v:
			yield ele

	def __add__(self, other):
		"""
		function to add a vector + vector or vector + other-type or += operation.
		"""
		if not isinstance(other, Vector):
			return Vector(([x + y for (x,y) in zip(self.v, list(other))]))
		else:
			return Vector(([x + y for (x,y) in zip(self.v, other.v)]))

	def __radd__(self, other):
		"""
		reusing __add__ function
		"""
		return self.__add__(other)

	def __iadd__(self, other):
		"""
		reusing __add__ function
		"""
		return self.__add__(other)

	def dot(self, other):
		"""method takes either a Vector or a sequence and returns the dot product of the 
		argument with current Vector instance. The dot product is defined as the sum of 
		the component-wise products. The behavior of this function if any elements are not 
		numeric is undefined"""
		return sum([x*y for (x,y) in zip(self.v, other)])

	def __getitem__(self, index):
		"""this method to allow element level retrival to the Vector. Indexing should 
		be 0 based (as in C). If the index is negative, it should translate to the 
		length of the Vector plus the index. Thus, index -1 is the last element. 
		If the index is out of range, your implementation should raise an IndexError 
		with an appropriate message."""
		try:
			return self.v[index]
		except IndexError:
			raise IndexError("Index out of range")

	def __setitem__(self, index, value):
		"""this method to allow element level setting to the Vector. Indexing should 
		be 0 based (as in C). If the index is negative, it should translate to the 
		length of the Vector plus the index. Thus, index -1 is the last element. 
		If the index is out of range, your implementation should raise an IndexError 
		with an appropriate message."""
		try:
			self.v[index] = value
			if len(self.v) != self.len:
				raise ValueError("length of Vector not preserved")
		except IndexError:
			raise IndexError("Index out of range")

	def __eq__(self, other):
		"""
		compares two vectors if they are equal to each other or not.
		"""
		if not isinstance(other, Vector):
			return False

		for (x,y) in zip(self.v, other.v):
			if x != y:
				return False
		
		return True

	def __ne__(self, other):
		"""
		compares two vectors if they are not equal to each other or not.
		"""
		if not isinstance(other, Vector):
			return True

		for (x,y) in zip(self.v, other.v):
			if x != y:
				return True
		
		return False

	def __gt__(self, other):
		"""
		compares two vectors A and B, if A is greater than B or not.
		"""
		for (x,y) in zip(sorted(self.v, reverse=True), sorted(other.v, reverse=True)):
			if x != y:
				return x > y
			else:
				continue
		return False

	def __lt__(self, other):
		"""
		compares two vectors A and B, if A is lesser than B or not.
		reuses __gt__ function.
		"""
		return (other > self)

	def __ge__(self, other):
		"""
		compares two vectors A and B, if A is greater than or equal to B or not.
		"""
		for (x,y) in zip(sorted(self.v, reverse=True), sorted(other.v, reverse=True)):
			if not x >= y:
				return False
		return True

	def __le__(self, other):
		"""
		compares two vectors A and B, if A is lesser than or equal to B or not.
		reusing __ge__ function.
		"""
		return (other >= self)
