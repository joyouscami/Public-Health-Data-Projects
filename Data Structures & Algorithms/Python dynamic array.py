class DynamicArray:
    def __init__(self):
        self.capacity = 1 #Initial Capacity
        self.size = 0 #Number of Elements
        self.array = self.make_array(self.capacity)
        def__len__(self):
        return self.size
    def append (self,element):
        