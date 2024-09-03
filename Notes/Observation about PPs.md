
A 1-bit only has complexities described by single polynomials

## Smallest n and simplest example of a 2-piece PP

With 2 bits, four functions result in a 2-piece PP. Notably, all of them result in the same PP:

$$
p1(p) = 1 + p
$$

$$p2(p) = 2 - p$$

p1 wins when p < 0.5, p2 wins otherwise.

The functions with this complexity are:

00 0
01 0
10 1
11 0

00 0
01 1
10 0
11 0

00 1
01 1
10 0
11 1

00 1
01 0
10 1
11 1

## Smallest n and simplest example of a 3-piece PP

For functions with 3 bits, their complexities are at most described by 2-piece PPs.

With 4 bits we find many PWs with more than 2 pieces.

So far, the simplest one we've found is composed of

p1(p) = 1 + 2p + p²
p2(p) = 1 + 3p - p²
p3(p) = 3 - p²

We say that this is simple since it only consists of 2nd degree polynomials.

We are not yet sure whether a simpler PW exists.

Right now, computing the complexities for the 2^2^4 4-bit functions would take hours.