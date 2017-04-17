#include <cstdio>
#include <chrono>

const long N = 35;

long fib(long n) {
    if (n == 0) {
        return 0;
    } else if (n == 1) {
        return 1;
    } else {
        return fib(n - 1) + fib(n - 2);
    }
}

int main(int, char**) {
    auto start = std::chrono::high_resolution_clock::now();
    auto result = fib(N);
    auto end = std::chrono::high_resolution_clock::now();
    auto elapsed = std::chrono::duration_cast<std::chrono::nanoseconds>(end - start).count() / 1000000;

    printf("Time: %ld ms\n", elapsed);
    printf("Result: %ld\n", result);
}

