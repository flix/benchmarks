class nbody {

    // n-body, from the Computer Language Benchmarks Game
    // http://benchmarksgame.alioth.debian.org/u64q/nbody-description.html#nbody

    // This implementation is a loose translation of the Flix program, which was loosely based on:
    // http://benchmarksgame.alioth.debian.org/u64q/program.php?test=nbody&lang=java&id=2

    public static final int N = 100000;

    public static final double pi = 3.141592653589793;
    public static final double solarMass = 4.0 * (pi * pi);
    public static final double daysPerYear = 365.24;

    public static double distance(double dx, double dy, double dz) {
        return java.lang.Math.pow((dx * dx) + (dy * dy) + (dz * dz), 0.5);
    }

    public static void main(String[] args) {
        long start = System.nanoTime();
        SolarSystem s = new SolarSystem();
        double init = s.run(0);
        double result = s.run(N);
        long end = System.nanoTime();
        long elapsed = (end - start) / 1000000;

        System.out.println("Time: " + elapsed + " ms");
        System.out.println("Initial: " + init);
        System.out.println("Result:  " + result);
    }

}

class Body {
    public double x, y, z, vx, vy, vz, m;

    public Body(double x, double y, double z, double vx, double vy, double vz, double m) {
        this.x = x;
        this.y = y;
        this.z = z;
        this.vx = vx;
        this.vy = vy;
        this.vz = vz;
        this.m = m;
    }

    public void offsetMomentum(double px, double py, double pz) {
        vx = -px / nbody.solarMass;
        vy = -py / nbody.solarMass;
        vz = -pz / nbody.solarMass;
    }

    public double getSpeedSq() {
        return (vx * vx) + (vy * vy) + (vz * vz);
    }

    public double getEnergy() {
        return 0.5 * m * getSpeedSq();
    }

    public void moveBody(double dt) {
        x = x + (dt * vx);
        y = y + (dt * vy);
        z = z + (dt * vz);
    }

    public void advanceBody(double dx, double dy, double dz, double delta) {
        vx = vx + (dx * delta);
        vy = vy + (dy * delta);
        vz = vz + (dz * delta);
    }
}

class SolarSystem {
    public Body sun, jupiter, saturn, uranus, neptune;

    public SolarSystem() {
        jupiter = new Body(
            4.84143144246472090,
            -1.16032004402742839,
            -0.103622044471123109,
            0.00166007664274403694 * nbody.daysPerYear,
            0.00769901118419740425 * nbody.daysPerYear,
            -0.0000690460016972063023 * nbody.daysPerYear,
            0.000954791938424326609 * nbody.solarMass);
        saturn = new Body(
            8.34336671824457987,
            4.12479856412430479,
            -0.403523417114321381,
            -0.00276742510726862411 * nbody.daysPerYear,
            0.00499852801234917238 * nbody.daysPerYear,
            0.0000230417297573763929 * nbody.daysPerYear,
            0.000285885980666130812 * nbody.solarMass);
        uranus = new Body(
            12.8943695621391310,
            -15.1111514016986312,
            -0.223307578892655734,
            0.00296460137564761618 * nbody.daysPerYear,
            0.00237847173959480950 * nbody.daysPerYear,
            -0.0000296589568540237556 * nbody.daysPerYear,
            0.0000436624404335156298 * nbody.solarMass);
        neptune = new Body(
            15.3796971148509165,
            -25.9193146099879641,
            0.179258772950371181,
            0.00268067772490389322 * nbody.daysPerYear,
            0.00162824170038242295 * nbody.daysPerYear,
            -0.0000951592254519715870 * nbody.daysPerYear,
            0.0000515138902046611451 * nbody.solarMass);
        sun = new Body(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, nbody.solarMass);

        double px = (sun.vx * sun.m) +
                    (jupiter.vx * jupiter.m) +
                    (saturn.vx * saturn.m) +
                    (uranus.vx * uranus.m) +
                    (neptune.vx * neptune.m);
        double py = (sun.vy * sun.m) +
                    (jupiter.vy * jupiter.m) +
                    (saturn.vy * saturn.m) +
                    (uranus.vy * uranus.m) +
                    (neptune.vy * neptune.m);
        double pz = (sun.vz * sun.m) +
                    (jupiter.vz * jupiter.m) +
                    (saturn.vz * saturn.m) +
                    (uranus.vz * uranus.m) +
                    (neptune.vz * neptune.m);
        sun.offsetMomentum(px, py, pz);
    }

    public void advance(double dt) {
        advanceHelper(sun, jupiter, dt);
        advanceHelper(sun, saturn, dt);
        advanceHelper(sun, uranus, dt);
        advanceHelper(sun, neptune, dt);
        advanceHelper(jupiter, saturn, dt);
        advanceHelper(jupiter, uranus, dt);
        advanceHelper(jupiter, neptune, dt);
        advanceHelper(saturn, uranus, dt);
        advanceHelper(saturn, neptune, dt);
        advanceHelper(uranus, neptune, dt);
        sun.moveBody(dt);
        jupiter.moveBody(dt);
        saturn.moveBody(dt);
        uranus.moveBody(dt);
        neptune.moveBody(dt);
    }

    private void advanceHelper(Body b1, Body b2, double dt) {
        double dx = b1.x - b2.x;
        double dy = b1.y - b2.y;
        double dz = b1.z - b2.z;
        double d = nbody.distance(dx, dy, dz);
        double mag = dt / (d * d * d);
        b1.advanceBody(dx, dy, dz, -b2.m * mag);
        b2.advanceBody(dx, dy, dz, b1.m * mag);
    }

    public double energy() {
        double posE = sun.getEnergy() + jupiter.getEnergy() + saturn.getEnergy() + uranus.getEnergy() + neptune.getEnergy();
        double negE = energyHelper(sun, jupiter) + energyHelper(sun, saturn) + energyHelper(sun, uranus) + energyHelper(sun, neptune) +
                      energyHelper(jupiter, saturn) + energyHelper(jupiter, uranus) + energyHelper(jupiter, neptune) +
                      energyHelper(saturn, uranus) + energyHelper(saturn, neptune) +
                      energyHelper(uranus, neptune);
        return posE - negE;
    }

    private double energyHelper(Body b1, Body b2) {
        double dx = b1.x - b2.x;
        double dy = b1.y - b2.y;
        double dz = b1.z - b2.z;
        return (b1.m * b2.m) / nbody.distance(dx, dy, dz);
    }

    public double run(int i) {
        for (int x = 0; x < i; x++) {
            advance(0.01);
        }
        return energy();
    }
}

