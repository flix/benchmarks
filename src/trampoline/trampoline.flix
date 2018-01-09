namespace Trampoline {

    def n(): Int = 12345678

    def even(n: Int): Bool = if (n == 0) true else odd(n - 1)

    def odd(n: Int): Bool = if (n == 0) false else even(n - 1)

    @benchmark
    def trampoline(): Bool = even(n())

}