require 'benchmark'

# pidigits, from the Computer Language Benchmarks Game.
# http://benchmarksgame.alioth.debian.org/u64q/pidigits-description.html#pidigits

# This implementation is a loose translation of the Flix program, which was loosely based on:
# http://benchmarksgame.alioth.debian.org/u64q/program.php?test=pidigits&lang=yarv&id=3

N = 10000

def pi(i)
  j, k, l, n, a, d, t, u = [i, 0, 1, 1, 0, 1, 0, 0]
  while j != 0 do
    k = k + 1
    t = n << 1
    n = n * k
    a = a + t
    l = l + 2
    a = a * l
    d = d * l
    if a >= n then
      tmp = (n * 3) + a
      t = tmp / d
      u = (tmp % d) + n
      if d > u then
        #print t
        j = j - 1
        a = (a - (d * t)) * 10
        n = n * 10
      end
    end
  end
  t
end

result = nil
time = Benchmark.realtime { result = pi(N) }

puts "Time: #{(time * 1000).round} ms"
puts "Result: #{result}"

