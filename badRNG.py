def bad_rng(seed=0.1, n=500):
    """
    A deliberately poor RNG that creates obvious patterns.
    Adds 0.1 to the previous value and takes modulo 1.
    """
    values = [0] * n
    values[0] = seed
    
    for i in range(1, n):
        values[i] = (values[i-1] + 0.1) % 1.0
    
    return values

# Generate 500 values with the bad RNG
if __name__ == "__main__":
    samples = bad_rng(seed=0.1, n=500)
    
    # Save to file
    with open("bad_rng_samples.txt", "w") as f:
        for sample in samples:
            f.write(f"{sample}\n")
    
    print(f"Generated {len(samples)} samples with bad RNG and saved to bad_rng_samples.txt")