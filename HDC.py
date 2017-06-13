from abc import ABC, abstractmethod

#https://stackoverflow.com/questions/13646245/is-it-possible-to-make-abstract-classes-in-python
#https://docs.scipy.org/doc/scipy/reference/generated/scipy.signal.convolve.html
#https://stackoverflow.com/questions/35474078/python-1d-array-circular-convolution

class HDC:
    @abstractmethod
    def bind(self, x, y):
        pass
    @abstractmethod
    def unbind(self, x, t):
        pass
    @abstractmethod
    def add(self, x, y):
        pass
    @abstractmethod
    def ref(self, x):
        pass
    @abstractmethod
    def newComb(self):
        pass
    @abstractmethod
    def newRot(self):
        pass
    @abstractmethod
    def extract(self, x):
        pass

class KanervaHDC(HDC):
    def __init__(self, dim, threshold):
        self.dim = dim
        self.threshold = threshold
        self.refs = []
        pass
    def bind(self, x, y):
        return x * y
    def unbind(self, x, y):
        return (-x) * y
    def add(self, x, y):
        return x + y
    def ref(self, x):
        # make random vec
        v = np.random.randn(dims)
        self.refs = self.refs + [(v,x)]
    def newRot(self, x):
        rot = np.random.permutation(dims)
        pass
    def extract(self, v):
        li = self.sortByActivation(v)
        return [(w, x) for (d, w, x) in li if d>= self.threshold]
    def sortByActivation(self, v):
        li = [(np.dot(v, w), w, x) for (w,x) in self.refs] 
        #[(np.dot(v, w), i) for (i,(w,x)) in enumerate(self.refs)]
        return sorted(li, reverse=True)
