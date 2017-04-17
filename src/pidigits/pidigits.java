import java.math.BigInteger;

class pidigits {

    // pidigits, from the Computer Language Benchmarks Game.
    // http://benchmarksgame.alioth.debian.org/u64q/pidigits-description.html#pidigits

    // This implementation is a loose translation of the Flix program, which was loosely based on:
    // http://benchmarksgame.alioth.debian.org/u64q/program.php?test=pidigits&lang=yarv&id=3

    public static final BigInteger N = BigInteger.valueOf(10000);

    public static BigInteger pi(BigInteger i) {
        BigInteger j = i;
        BigInteger k = BigInteger.ZERO;
        BigInteger l = BigInteger.ONE;
        BigInteger n = BigInteger.ONE;
        BigInteger a = BigInteger.ZERO;
        BigInteger d = BigInteger.ONE;
        BigInteger t = BigInteger.ZERO;
        BigInteger u = BigInteger.ZERO;
        while (!j.equals(BigInteger.ZERO)) {
            k = k.add(BigInteger.ONE);
            t = n.shiftLeft(1);
            n = n.multiply(k);
            a = a.add(t);
            l = l.add(BigInteger.valueOf(2));
            a = a.multiply(l);
            d = d.multiply(l);
            if (a.compareTo(n) >= 0) {
                BigInteger tmp = (n.multiply(BigInteger.valueOf(3))).add(a);
                t = tmp.divide(d);
                u = (tmp.mod(d)).add(n);
                if (d.compareTo(u) > 0) {
                    //System.out.print(t);
                    j = j.subtract(BigInteger.ONE);
                    a = (a.subtract(d.multiply(t))).multiply(BigInteger.TEN);
                    n = n.multiply(BigInteger.TEN);
                }
            }
        }
        return t;
    }

    public static void main(String[] args) {
        long start = System.nanoTime();
        BigInteger result = pi(N);
        long end = System.nanoTime();
        long elapsed = (end - start) / 1000000;

        System.out.println("Time: " + elapsed + " ms");
        System.out.println("Result: " + result);
    }

}

