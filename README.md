# לספור
Getting back to basics: learning how to count.

## Sizes of possible distributions

```haskell
distributionSizeSequence n
```

Given a set of cardinality `n`, count the number of possible distributions for lengths `[0..]`.

Example: Bits

A `bit` has cardinality 2.

0. If you have 0 bits, you vacuously have one way to distribute them (which is distributing nothing)
1. If you have 1 bit, you can either have a zero bit or a one bit, so two ways total.
2. If you have 2 bits, you can have 3 possible distributions:
  * 0 zeros and 2 ones
  * 1 zero and 1 one
  * 2 zeros and 0 ones
3. If you have 3 bits, you can have 4 possible distributions:
  * 0 zeros and 3 ones
  * 1 zero and 2 ones
  * 2 zeros and 1 one
  * 3 zeros and 0 ones

Things get more interesting for sets of cardinality 3 or more.

These are apparently the same as the [binomial coefficients](https://en.wikipedia.org/wiki/Binomial_coefficient).

More precisely:
```haskell
distributionSizeSequence k == fmap (\n -> choose n (k - 1)) [(k - 1)..]

checkProp c k = take c (fmap fromIntegral (distributionSizeSequence k)) == take c (fmap (\n -> choose n (k - 1)) [(k - 1)..])
and $ fmap (checkProp 7) [2..8]
```

### distributionSizeSequence 2
```haskell
[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67
```

### distributionSizeSequence 3
```haskell
[1,3,6,10,15,21,28,36,45,55,66,78,91,105,120,136,153,171,190,210,231,253,276,300,325,351,378,406,435,465,496,528,561,595,630,666,703,741,780,820,861,903,946,990,1035,1081,1128,1176,1225,1275,1326,1378
```
* [A000217](https://oeis.org/A000217)  Triangular numbers: a(n) = binomial(n+1,2) = n(n+1)/2 = 0 + 1 + 2 + ... + n.
* [A161680](https://oeis.org/A161680)  a(n) = binomial(n,2): number of size-2 subsets of {0,1,...,n} that contain no consecutive integers.
* [A105340](https://oeis.org/A105340)  a(n) = n*(n+1)/2 mod 2048.

### distributionSizeSequence 4
```haskell
[1,4,10,20,35,56,84,120,165,220,286,364,455,560,680,816,969,1140,1330,1540,1771,2024,2300,2600,2925,3276,3654,4060,4495,4960,5456
```
[A000292](https://oeis.org/A000292)  Tetrahedral (or triangular pyramidal) numbers: a(n) = C(n+2,3) = n*(n+1)*(n+2)/6.

### distributionSizeSequence 5
```haskell
[1,5,15,35,70,126,210,330,495,715,1001,1365,1820,2380,3060,3876,4845,5985,7315,8855
```
[A000332](https://oeis.org/A000332)  Binomial coefficient binomial(n,4) = n*(n-1)*(n-2)*(n-3)/24.

### distributionSizeSequence 6
```haskell
[1,6,21,56,126,252,462,792,1287,2002,3003,4368,6188,8568,11628,15504
```
[A000389](https://oeis.org/A000389)  Binomial coefficients C(n,5).

### distributionSizeSequence 7
```haskell
[1,7,28,84,210,462,924,1716,3003,5005,8008
```
[A000579](https://oeis.org/A000579)  Figurate numbers or binomial coefficients C(n,6).
