namespace PIDigits {

    // pidigits, from the Computer Language Benchmarks Game.
    // http://benchmarksgame.alioth.debian.org/u64q/pidigits-description.html#pidigits

    // This implementation is loosely based on:
    // http://benchmarksgame.alioth.debian.org/u64q/program.php?test=pidigits&lang=yarv&id=3

    def ndigits(): BigInt = 10000ii

    def compTpl1(a2: BigInt, n1: BigInt, d1: BigInt, t1: BigInt, u: BigInt): (BigInt, BigInt) =
      let tmp = (n1 * 3ii) + a2;
      if (a2 >= n1) (tmp / d1, (tmp % d1) + n1) else (t1, u)

    def compTpl2(a2: BigInt, n1: BigInt, d1: BigInt, u1: BigInt, i: BigInt, t2: BigInt): (BigInt, BigInt, BigInt) =
      if ((a2 >= n1) && (d1 > u1)) (i - 1ii, (a2 - (d1 * t2)) * 10ii, n1 * 10ii) else (i, a2, n1)

    def piHelper(i: BigInt, k: BigInt, l: BigInt, n: BigInt, a: BigInt, d: BigInt, t: BigInt, u: BigInt): BigInt =
      if (i == 0ii) t
      else let k1 = k + 1ii;
           let t1 = n <<< 1;
           let n1 = n * k1;
           let a1 = a + t1;
           let l1 = l + 2ii;
           let a2 = a1 * l1;
           let d1 = d * l1;
           let (t2, u1) = compTpl1(a2, n1, d1, t1, u);
           let (i1, a3, n2) = compTpl2(a2, n1, d1, u1, i, t2);
           piHelper(i1, k1, l1, n2, a3, d1, t2, u1)

    def pi(i: BigInt): BigInt = piHelper(i, 0ii, 1ii, 1ii, 0ii, 1ii, 0ii, 0ii)

    def pidigits(): BigInt = pi(ndigits())
}

