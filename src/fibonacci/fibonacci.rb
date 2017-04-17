require 'benchmark'

N = 35

def fib(n)
  if n == 0 then 0
  elsif n == 1 then 1
  else fib(n - 1) + fib(n - 2)
  end
end

result = nil
time = Benchmark.realtime { result = fib(N) }

puts "Time: #{(time * 1000).round} ms"
puts "Result: #{result}"

