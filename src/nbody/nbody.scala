package nbody

object nbody {

  val WarmupRounds = 1000
  val ActualRounds = 1000

  // n-body, from the Computer Language Benchmarks Game
  // http://benchmarksgame.alioth.debian.org/u64q/nbody-description.html#nbody

  // This implementation is a loose translation of the Flix program, which was loosely based on:
  // http://benchmarksgame.alioth.debian.org/u64q/program.php?test=nbody&lang=java&id=2

  val N: Int = 100000

  val pi = 3.141592653589793
  val solarMass = 4.0 * (pi * pi)
  val daysPerYear = 365.24

  def distance(dx: Double, dy: Double, dz: Double): Double =
    math.pow((dx * dx) + (dy * dy) + (dz * dz), 0.5)

  case class Body(x: Double, y: Double, z: Double, vx: Double, vy: Double, vz: Double, m: Double) {
    def offsetMomentum(px: Double, py: Double, pz: Double): Body =
      this.copy(vx = -px / solarMass, vy = -py / solarMass, vz = -pz / solarMass)

    def getSpeedSq: Double = (vx * vx) + (vy * vy) + (vz * vz)

    def getEnergy: Double = 0.5 * m * getSpeedSq

    def moveBody(dt: Double): Body =
      this.copy(x = x + (dt * vx), y = y + (dt * vy), z = z + (dt * vz))

    def advanceBody(dx: Double, dy: Double, dz: Double, delta: Double): Body =
      this.copy(vx = vx + (dx * delta), vy = vy + (dy * delta), vz = vz + (dz * delta))
  }

  case class SolarSystem(sun: Body, jupiter: Body, saturn: Body, uranus: Body, neptune: Body) {
    def advance(dt: Double): SolarSystem = {
      def advanceHelper(b1: Body, b2: Body, dt: Double): (Body, Body) = {
        val dx = b1.x - b2.x
        val dy = b1.y - b2.y
        val dz = b1.z - b2.z
        val d = distance(dx, dy, dz)
        val mag = dt / (d * d * d)
        val newB1 = b1.advanceBody(dx, dy, dz, -b2.m * mag)
        val newB2 = b2.advanceBody(dx, dy, dz, b1.m * mag)
        (newB1, newB2)
      }

      val (sun1, jupiter1) = advanceHelper(sun, jupiter, dt)
      val (sun2, saturn1) = advanceHelper(sun1, saturn, dt)
      val (sun3, uranus1) = advanceHelper(sun2, uranus, dt)
      val (sun4, neptune1) = advanceHelper(sun3, neptune, dt)
      val (jupiter2, saturn2) = advanceHelper(jupiter1, saturn1, dt)
      val (jupiter3, uranus2) = advanceHelper(jupiter2, uranus1, dt)
      val (jupiter4, neptune2) = advanceHelper(jupiter3, neptune1, dt)
      val (saturn3, uranus3) = advanceHelper(saturn2, uranus2, dt)
      val (saturn4, neptune3) = advanceHelper(saturn3, neptune2, dt)
      val (uranus4, neptune4) = advanceHelper(uranus3, neptune3, dt)
      SolarSystem(sun4.moveBody(dt), jupiter4.moveBody(dt), saturn4.moveBody(dt), uranus4.moveBody(dt), neptune4.moveBody(dt))
    }


    def energy: Double = {
      def energyHelper(b1: Body, b2: Body): Double = {
        val dx = b1.x - b2.x
        val dy = b1.y - b2.y
        val dz = b1.z - b2.z
        (b1.m * b2.m) / distance(dx, dy, dz)
      }

      val posE = sun.getEnergy + jupiter.getEnergy + saturn.getEnergy + uranus.getEnergy + neptune.getEnergy
      val negE = energyHelper(sun, jupiter) + energyHelper(sun, saturn) + energyHelper(sun, uranus) + energyHelper(sun, neptune) +
        energyHelper(jupiter, saturn) + energyHelper(jupiter, uranus) + energyHelper(jupiter, neptune) +
        energyHelper(saturn, uranus) + energyHelper(saturn, neptune) +
        energyHelper(uranus, neptune)
      posE - negE
    }

    @scala.annotation.tailrec
    final def run(i: Int): Double =
      if (i == 0) energy
      else {
        val s1 = advance(0.01)
        s1.run(i - 1)
      }

  }

  val initJupiter = Body(
    4.84143144246472090,
    -1.16032004402742839,
    -0.103622044471123109,
    0.00166007664274403694 * daysPerYear,
    0.00769901118419740425 * daysPerYear,
    -0.0000690460016972063023 * daysPerYear,
    0.000954791938424326609 * solarMass
  )
  val initSaturn = Body(
    8.34336671824457987,
    4.12479856412430479,
    -0.403523417114321381,
    -0.00276742510726862411 * daysPerYear,
    0.00499852801234917238 * daysPerYear,
    0.0000230417297573763929 * daysPerYear,
    0.000285885980666130812 * solarMass
  )
  val initUranus = Body(
    12.8943695621391310,
    -15.1111514016986312,
    -0.223307578892655734,
    0.00296460137564761618 * daysPerYear,
    0.00237847173959480950 * daysPerYear,
    -0.0000296589568540237556 * daysPerYear,
    0.0000436624404335156298 * solarMass
  )
  val initNeptune = Body(
    15.3796971148509165,
    -25.9193146099879641,
    0.179258772950371181,
    0.00268067772490389322 * daysPerYear,
    0.00162824170038242295 * daysPerYear,
    -0.0000951592254519715870 * daysPerYear,
    0.0000515138902046611451 * solarMass
  )
  val initSun = Body(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, solarMass)

  val initialSystem: SolarSystem = {
    val px = (initSun.vx * initSun.m) +
      (initJupiter.vx * initJupiter.m) +
      (initSaturn.vx * initSaturn.m) +
      (initUranus.vx * initUranus.m) +
      (initNeptune.vx * initNeptune.m)
    val py = (initSun.vy * initSun.m) +
      (initJupiter.vy * initJupiter.m) +
      (initSaturn.vy * initSaturn.m) +
      (initUranus.vy * initUranus.m) +
      (initNeptune.vy * initNeptune.m)
    val pz = (initSun.vz * initSun.m) +
      (initJupiter.vz * initJupiter.m) +
      (initSaturn.vz * initSaturn.m) +
      (initUranus.vz * initUranus.m) +
      (initNeptune.vz * initNeptune.m)
    val sun = initSun.offsetMomentum(px, py, pz)
    SolarSystem(sun, initJupiter, initSaturn, initUranus, initNeptune)
  }

  def run(): Double = {
    val s = initialSystem
    val init = s.run(0)
    s.run(N)
  }

  def warmpup(): Unit = {
    for (_ <- 0 until WarmupRounds) {
      run()
    }
  }

  def sample(): Long = {
    var result = List.empty[Long]
    var i = 0
    while (i < ActualRounds) {
      val t = System.nanoTime()
      run()
      val e = System.nanoTime() - t
      i = i + 1
      result = e :: result
    }
    median(result)
  }

  def median(xs: List[Long]): Long = {
    if (xs.isEmpty) throw new IllegalArgumentException("Empty list.")
    if (xs.length == 1) return xs.head

    val l = xs.sorted
    val n = xs.length
    if (n % 2 == 0) {
      val index = n / 2
      l(index)
    } else {
      val index = n / 2
      (l(index) + l(index + 1)) / 2
    }
  }

  def main(args: Array[String]): Unit = {
    warmpup()
    println(sample() / (1000 * 1000))
  }

}

