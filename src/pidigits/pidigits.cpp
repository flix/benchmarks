#include <cstdio>
#include <chrono>
#include <tuple>
#include <gmpxx.h>

// pidigits, from the Computer Language Benchmarks Game.
// http://benchmarksgame.alioth.debian.org/u64q/pidigits-description.html#pidigits

// This implementation is a loose translation of the Flix program, which was loosely based on:
// http://benchmarksgame.alioth.debian.org/u64q/program.php?test=pidigits&lang=yarv&id=3

const mpz_class N = 10000;

mpz_class pi(mpz_class i) {
    mpz_class j = i;
    mpz_class k = 0;
    mpz_class l = 1;
    mpz_class n = 1;
    mpz_class a = 0;
    mpz_class d = 1;
    mpz_class t = 0;
    mpz_class u = 0;
    while (j != 0) {
        k = k + 1;
        t = n << 1;
        n = n * k;
        a = a + t;
        l = l + 2;
        a = a * l;
        d = d * l;
        if (a >= n) {
            t = ((n * 3) + a) / d;
            u = (((n * 3) + a) % d) + n;
            if (d > u) {
                //gmp_printf("%Zd", t.get_mpz_t());
                j = j - 1;
                a = (a - (d * t)) * 10;
                n = n * 10;
            }
        }
    }
    return t;
}

int main(int, char**) {
    auto start = std::chrono::high_resolution_clock::now();
    auto result = pi(N);
    auto end = std::chrono::high_resolution_clock::now();
    auto elapsed = std::chrono::duration_cast<std::chrono::nanoseconds>(end - start).count() / 1000000;

    printf("Time: %ld ms\n", elapsed);
    gmp_printf("Result: %Zd\n", result.get_mpz_t());
}

