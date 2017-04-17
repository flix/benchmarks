require 'benchmark'

# n-body, from the Computer Language Benchmarks Game
# http://benchmarksgame.alioth.debian.org/u64q/nbody-description.html#nbody

# This implementation is a loose translation of the Flix program, which was loosely based on:
# http://benchmarksgame.alioth.debian.org/u64q/program.php?test=nbody&lang=java&id=2

N = 100000

Pi = 3.141592653589793;
SolarMass = 4.0 * (Pi * Pi);
DaysPerYear = 365.24;

def distance(dx, dy, dz)
  ((dx * dx) + (dy * dy) + (dz * dz)) ** 0.5
end

class Body
  attr_accessor :x, :y, :z, :vx, :vy, :vz, :m

  def initialize(x, y, z, vx, vy, vz, m)
    @x, @y, @z, @vx, @vy, @vz, @m = x, y, z, vx, vy, vz, m
  end

  def offsetMomentum(px, py, pz)
    @vx = -px / SolarMass
    @vy = -py / SolarMass
    @vz = -pz / SolarMass
  end

  def getSpeedSq
    (vx * vx) + (vy * vy) + (vz * vz)
  end

  def getEnergy
    0.5 * m * getSpeedSq
  end

  def moveBody(dt)
    @x = @x + (dt * @vx)
    @y = @y + (dt * @vy)
    @z = @z + (dt * @vz)
  end

  def advanceBody(dx, dy, dz, delta)
    @vx = @vx + (dx * delta)
    @vy = @vy + (dy * delta)
    @vz = @vz + (dz * delta)
  end
end

class SolarSystem
  attr_accessor :sun, :jupiter, :saturn, :uranus, :neptune

  def initialize
    @sun = Body.new(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, SolarMass)
    @jupiter = Body.new(
      4.84143144246472090,
      -1.16032004402742839,
      -0.103622044471123109,
      0.00166007664274403694 * DaysPerYear,
      0.00769901118419740425 * DaysPerYear,
      -0.0000690460016972063023 * DaysPerYear,
      0.000954791938424326609 * SolarMass)
    @saturn = Body.new(
      8.34336671824457987,
      4.12479856412430479,
      -0.403523417114321381,
      -0.00276742510726862411 * DaysPerYear,
      0.00499852801234917238 * DaysPerYear,
      0.0000230417297573763929 * DaysPerYear,
      0.000285885980666130812 * SolarMass)
     @uranus = Body.new(
       12.8943695621391310,
       -15.1111514016986312,
       -0.223307578892655734,
       0.00296460137564761618 * DaysPerYear,
       0.00237847173959480950 * DaysPerYear,
       -0.0000296589568540237556 * DaysPerYear,
       0.0000436624404335156298 * SolarMass)
     @neptune = Body.new(
       15.3796971148509165,
       -25.9193146099879641,
       0.179258772950371181,
       0.00268067772490389322 * DaysPerYear,
       0.00162824170038242295 * DaysPerYear,
       -0.0000951592254519715870 * DaysPerYear,
       0.0000515138902046611451 * SolarMass)

    px = (sun.vx * sun.m) +
         (jupiter.vx * jupiter.m) +
         (saturn.vx * saturn.m) +
         (uranus.vx * uranus.m) +
         (neptune.vx * neptune.m)
    py = (sun.vy * sun.m) +
         (jupiter.vy * jupiter.m) +
         (saturn.vy * saturn.m) +
         (uranus.vy * uranus.m) +
         (neptune.vy * neptune.m)
    pz = (sun.vz * sun.m) +
         (jupiter.vz * jupiter.m) +
         (saturn.vz * saturn.m) +
         (uranus.vz * uranus.m) +
         (neptune.vz * neptune.m)
    sun.offsetMomentum(px, py, pz)
  end

  def advance(dt)
    advanceHelper(sun, jupiter, dt)
    advanceHelper(sun, saturn, dt)
    advanceHelper(sun, uranus, dt)
    advanceHelper(sun, neptune, dt)
    advanceHelper(jupiter, saturn, dt)
    advanceHelper(jupiter, uranus, dt)
    advanceHelper(jupiter, neptune, dt)
    advanceHelper(saturn, uranus, dt)
    advanceHelper(saturn, neptune, dt)
    advanceHelper(uranus, neptune, dt)
    sun.moveBody(dt)
    jupiter.moveBody(dt)
    saturn.moveBody(dt)
    uranus.moveBody(dt)
    neptune.moveBody(dt)
  end

  def advanceHelper(b1, b2, dt)
    dx = b1.x - b2.x
    dy = b1.y - b2.y
    dz = b1.z - b2.z
    d = distance(dx, dy, dz)
    mag = dt / (d * d * d)
    b1.advanceBody(dx, dy, dz, -b2.m * mag)
    b2.advanceBody(dx, dy, dz, b1.m * mag)
  end

  def energy
    posE = sun.getEnergy + jupiter.getEnergy + saturn.getEnergy + uranus.getEnergy + neptune.getEnergy
    negE = energyHelper(sun, jupiter) + energyHelper(sun, saturn) + energyHelper(sun, uranus) + energyHelper(sun, neptune) +
           energyHelper(jupiter, saturn) + energyHelper(jupiter, uranus) + energyHelper(jupiter, neptune) +
           energyHelper(saturn, uranus) + energyHelper(saturn, neptune) +
           energyHelper(uranus, neptune)
    posE - negE
  end

  def energyHelper(b1, b2)
    dx = b1.x - b2.x
    dy = b1.y - b2.y
    dz = b1.z - b2.z
    (b1.m * b2.m) / distance(dx, dy, dz)
  end

  def run(i)
    x = 0
    while x < i do
      advance(0.01)
      x += 1
    end
    energy
  end

  private :advanceHelper, :energyHelper
end

init = nil
result = nil
time = Benchmark.realtime do
  s = SolarSystem.new
  init = s.run(0)
  result = s.run(N)
end

puts "Time: #{(time * 1000).round} ms"
puts "Initial: #{init}"
puts "Result:  #{result}"

