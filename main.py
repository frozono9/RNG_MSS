class LCG:
    def __init__(self, seed=42, a=1664525, c=1013904223, m=2**32):
        """
        Initialize Linear Congruential Generator with parameters:
        - seed: initial value
        - a: multiplier
        - c: increment
        - m: modulus
        """
        self.state = seed
        self.a = a
        self.c = c
        self.m = m
    
    def next(self):
        """Generate next random number"""
        self.state = (self.a * self.state + self.c) % self.m
        return self.state
    
    def next_float(self):
        """Generate next random number in [0,1)"""
        return self.next() / self.m
    
    def generate_samples(self, n):
        """Generate n random samples in [0,1)"""
        return [self.next_float() for _ in range(n)]

# Generate 500 uniform random values
if __name__ == "__main__":
    # Parameters from Numerical Recipes
    rng = LCG(seed=12345, a=1664525, c=1013904223, m=2**32)
    samples = rng.generate_samples(500)
    
    # Save to file
    with open("lcg_samples.txt", "w") as f:
        for sample in samples:
            f.write(f"{sample}\n")
    
    print(f"Generated {len(samples)} random samples and saved to lcg_samples.txt")