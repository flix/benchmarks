namespace nbody {

    // n-body, from the Computer Language Benchmarks Game
    // http://benchmarksgame.alioth.debian.org/u64q/nbody-description.html#nbody

    // This implementation is loosely based on:
    // http://benchmarksgame.alioth.debian.org/u64q/program.php?test=nbody&lang=java&id=2

    def numSteps(dummy: Int): Int = 100000

    def pi: Float = 3.141592653589793
    def solarMass: Float = 4.0 * (pi * pi)
    def daysPerYear: Float = 365.24

    enum Body { case B(Float, Float, Float, Float, Float, Float, Float) }

    enum SolarSystem { case S(Body, Body, Body, Body, Body) }

    def distance(dx: Float, dy: Float, dz: Float): Float = ((dx * dx) + (dy * dy) + (dz * dz)) ** 0.5

    def offsetMomentum(b: Body, px: Float, py: Float, pz: Float): Body =
      let Body.B(x, y, z, vx, vy, vz, m) = b in
        Body.B(x, y, z, -px / solarMass, -py / solarMass, -pz / solarMass, m)

    def getM(b: Body): Float = let Body.B(_, _, _, _, _, _, m) = b in m

    def getSpeedSq(b: Body): Float =
      let Body.B(_, _, _, vx, vy, vz, _) = b in (vx * vx) + (vy * vy) + (vz * vz)

    def getEnergy(b: Body): Float = 0.5 * getM(b) * getSpeedSq(b)

    def moveBody(b: Body, dt: Float): Body =
      let Body.B(x, y, z, vx, vy, vz, m) = b in
        Body.B(x + (dt * vx), y + (dt * vy), z + (dt * vz), vx, vy, vz, m)

    def advanceBody(b: Body, dx: Float, dy: Float, dz: Float, delta: Float): Body =
      let Body.B(x, y, z, vx, vy, vz, m) = b in
        Body.B(x, y, z, vx + (dx * delta), vy + (dy * delta), vz + (dz * delta), m)

    def advanceHelper(b1: Body, b2: Body, dt: Float): (Body, Body) =
      let Body.B(x1, y1, z1, vx1, vy1, vz1, m1) = b1 in
      let Body.B(x2, y2, z2, vx2, vy2, vz2, m2) = b2 in
      let dx = x1 - x2 in
      let dy = y1 - y2 in
      let dz = z1 - z2 in
      let d = distance(dx, dy, dz) in
      let mag = dt / (d * d * d) in
      let newB1 = advanceBody(b1, dx, dy, dz, -getM(b2) * mag) in
      let newB2 = advanceBody(b2, dx, dy, dz, getM(b1) * mag) in
        (newB1, newB2)

    def advance(s: SolarSystem, dt: Float): SolarSystem =
      let SolarSystem.S(sun0, jupiter0, saturn0, uranus0, neptune0) = s in
      let (sun1, jupiter1) = advanceHelper(sun0, jupiter0, dt) in
      let (sun2, saturn1) = advanceHelper(sun1, saturn0, dt) in
      let (sun3, uranus1) = advanceHelper(sun2, uranus0, dt) in
      let (sun4, neptune1) = advanceHelper(sun3, neptune0, dt) in
      let (jupiter2, saturn2) = advanceHelper(jupiter1, saturn1, dt) in
      let (jupiter3, uranus2) = advanceHelper(jupiter2, uranus1, dt) in
      let (jupiter4, neptune2) = advanceHelper(jupiter3, neptune1, dt) in
      let (saturn3, uranus3) = advanceHelper(saturn2, uranus2, dt) in
      let (saturn4, neptune3) = advanceHelper(saturn3, neptune2, dt) in
      let (uranus4, neptune4) = advanceHelper(uranus3, neptune3, dt) in
        SolarSystem.S(moveBody(sun4, dt), moveBody(jupiter4, dt), moveBody(saturn4, dt), moveBody(uranus4, dt), moveBody(neptune4, dt))

    def energyHelper(b1: Body, b2: Body): Float =
      let Body.B(x1, y1, z1, _, _, _, m1) = b1 in
      let Body.B(x2, y2, z2, _, _, _, m2) = b2 in
      let dx = x1 - x2 in
      let dy = y1 - y2 in
      let dz = z1 - z2 in
        (m1 * m2) / distance(dx, dy, dz)

    def energy(s: SolarSystem): Float =
      let SolarSystem.S(sun, jupiter, saturn, uranus, neptune) = s in
      let posE = getEnergy(sun) + getEnergy(jupiter) + getEnergy(saturn) + getEnergy(uranus) + getEnergy(neptune) in
      let negE = energyHelper(sun, jupiter) + energyHelper(sun, saturn) + energyHelper(sun, uranus) + energyHelper(sun, neptune) +
                 energyHelper(jupiter, saturn) + energyHelper(jupiter, uranus) + energyHelper(jupiter, neptune) +
                 energyHelper(saturn, uranus) + energyHelper(saturn, neptune) +
                 energyHelper(uranus, neptune) in
        posE - negE

    def step(s: SolarSystem, e: Float, i: Int): Float =
      if (i == 0) e
      else let s1 = advance(s, 0.01) in
        step(s1, energy(s1), i - 1)

    def run(i: Int): Float = step(initialSystem, energy(initialSystem), i)

    def initJupiter: Body = Body.B(
      4.84143144246472090,
      -1.16032004402742839,
      -0.103622044471123109,
      0.00166007664274403694 * daysPerYear,
      0.00769901118419740425 * daysPerYear,
      -0.0000690460016972063023 * daysPerYear,
      0.000954791938424326609 * solarMass)
    def initSaturn: Body = Body.B(
      8.34336671824457987,
      4.12479856412430479,
      -0.403523417114321381,
      -0.00276742510726862411 * daysPerYear,
      0.00499852801234917238 * daysPerYear,
      0.0000230417297573763929 * daysPerYear,
      0.000285885980666130812 * solarMass)
    def initUranus: Body = Body.B(
      12.8943695621391310,
      -15.1111514016986312,
      -0.223307578892655734,
      0.00296460137564761618 * daysPerYear,
      0.00237847173959480950 * daysPerYear,
      -0.0000296589568540237556 * daysPerYear,
      0.0000436624404335156298 * solarMass)
    def initNeptune: Body = Body.B(
      15.3796971148509165,
      -25.9193146099879641,
      0.179258772950371181,
      0.00268067772490389322 * daysPerYear,
      0.00162824170038242295 * daysPerYear,
      -0.0000951592254519715870 * daysPerYear,
      0.0000515138902046611451 * solarMass)
    def initSun: Body = Body.B(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, solarMass)

    def initialSystem: SolarSystem =
      let Body.B(_, _, _, sunVx, sunVy, sunVz, sunM) = initSun in
      let Body.B(_, _, _, jupiterVx, jupiterVy, jupiterVz, jupiterM) = initJupiter in
      let Body.B(_, _, _, saturnVx, saturnVy, saturnVz, saturnM) = initSaturn in
      let Body.B(_, _, _, uranusVx, uranusVy, uranusVz, uranusM) = initUranus in
      let Body.B(_, _, _, neptuneVx, neptuneVy, neptuneVz, neptuneM) = initNeptune in
      let px = (sunVx * sunM) +
               (jupiterVx * jupiterM) +
               (saturnVx * saturnM) +
               (uranusVx * uranusM) +
               (neptuneVx * neptuneM) in
      let py = (sunVy * sunM) +
               (jupiterVy * jupiterM) +
               (saturnVy * saturnM) +
               (uranusVy * uranusM) +
               (neptuneVy * neptuneM) in
      let pz = (sunVz * sunM) +
               (jupiterVz * jupiterM) +
               (saturnVz * saturnM) +
               (uranusVz * uranusM) +
               (neptuneVz * neptuneM) in
      let sun = offsetMomentum(initSun, px, py, pz) in
        SolarSystem.S(sun, initJupiter, initSaturn, initUranus, initNeptune)

    rel Output(x: Int, y: Float);
    Output(0, run(0)).
    Output(numSteps(0), run(numSteps(0))).

}

