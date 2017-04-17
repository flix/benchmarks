class fibonacci {

    public static final long N = 35;

    public static long fib(long n) {
        if (n == 0) {
            return 0;
        } else if (n == 1) {
            return 1;
        } else {
            return fib(n - 1) + fib(n - 2);
        }
    }

    public static void main(String[] args) {
        long start = System.nanoTime();
        long result = fib(N);
        long end = System.nanoTime();
        long elapsed = (end - start) / 1000000;

        System.out.println("Time: " + elapsed + " ms");
        System.out.println("Result: " + result);
    }

}

