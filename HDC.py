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
