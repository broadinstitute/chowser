package chowser.util

object MathUtils {

  /*
   * The probit function (inverse cumulative normal distribution)
   *
   * Ported from http://lib.stat.cmu.edu/apstat/241
   *
   * ALGORITHM AS241  APPL. STATIST. (1988) VOL. 37, NO. 3
   *
   * Produces the normal deviate Z corresponding to a given lower
   * tail area of P; Z is accurate to about 1 part in 10**16.
   *
   * The hash sums below are the sums of the mantissas of the
   * coefficients.   They are included for use in checking
   * transcription.
   */
  def probit(p: Double): Double = {
    val split1 = 0.425
    val split2 = 5.0
    val const1 = 0.180625
    val const2 = 1.6

    // 	Coefficients for P close to 0.5
    val a0 = 3.3871328727963666080
    val a1 = 1.3314166789178437745e+2
    val a2 = 1.9715909503065514427e+3
    val a3 = 1.3731693765509461125e+4
    val a4 = 4.5921953931549871457e+4
    val a5 = 6.7265770927008700853e+4
    val a6 = 3.3430575583588128105e+4
    val a7 = 2.5090809287301226727e+3
    val b1 = 4.2313330701600911252e+1
    val b2 = 6.8718700749205790830e+2
    val b3 = 5.3941960214247511077e+3
    val b4 = 2.1213794301586595867e+4
    val b5 = 3.9307895800092710610e+4
    val b6 = 2.8729085735721942674e+4
    val b7 = 5.2264952788528545610e+3
    //	HASH SUM AB    55.88319 28806 14901 4439

    //	Coefficients for P not close to 0, 0.5 or 1.
    val c0 = 1.42343711074968357734
    val c1 = 4.63033784615654529590
    val c2 = 5.76949722146069140550
    val c3 = 3.64784832476320460504
    val c4 = 1.27045825245236838258
    val c5 = 2.41780725177450611770e-1
    val c6 = 2.27238449892691845833e-2
    val c7 = 7.74545014278341407640e-4
    val d1 = 2.05319162663775882187
    val d2 = 1.67638483018380384940
    val d3 = 6.89767334985100004550e-1
    val d4 = 1.48103976427480074590e-1
    val d5 = 1.51986665636164571966e-2
    val d6 = 5.47593808499534494600e-4
    val d7 = 1.05075007164441684324e-9
    //	HASH SUM CD    49.33206 50330 16102 89036

    //	Coefficients for P near 0 or 1.
    val e0 = 6.65790464350110377720
    val e1 = 5.46378491116411436990
    val e2 = 1.78482653991729133580
    val e3 = 2.96560571828504891230e-1
    val e4 = 2.65321895265761230930e-2
    val e5 = 1.24266094738807843860e-3
    val e6 = 2.71155556874348757815e-5
    val e7 = 2.01033439929228813265e-7
    val f1 = 5.99832206555887937690e-1
    val f2 = 1.36929880922735805310e-1
    val f3 = 1.48753612908506148525e-2
    val f4 = 7.86869131145613259100e-4
    val f5 = 1.84631831751005468180e-5
    val f6 = 1.42151175831644588870e-7
    val f7 = 2.04426310338993978564e-15
    //	HASH SUM EF    47.52583 31754 92896 71629

    val q = p - 0.5
    if (Math.abs(q) < split1) {
      val r = const1 - q * q
      q * (((((((a7 * r + a6) * r + a5) * r + a4) * r + a3) * r + a2) * r + a1) * r + a0) /
        (((((((b7 * r + b6) * r + b5) * r + b4) * r + b3) * r + b2) * r + b1) * r + 1.0)
    } else {
      val r2 = if (q < 0.0) p else 1.0 - p
      if (r2 < 0.0) {
        Double.NaN
      } else {
        val r3 = Math.sqrt(-Math.log(r2))
        val zAbs = if (r3 < split2) {
          val r = r3 - const2
          (((((((c7 * r + c6) * r + c5) * r + c4) * r + c3) * r + c2) * r + c1) * r + c0) /
            (((((((d7 * r + d6) * r + d5) * r + d4) * r + d3) * r + d2) * r + d1) * r + 1.0)
        } else {
          val r = r3 - split2
          (((((((e7 * r + e6) * r + e5) * r + e4) * r + e3) * r + e2) * r + e1) * r + e0) /
            (((((((f7 * r + f6) * r + f5) * r + f4) * r + f3) * r + f2) * r + f1) * r + 1.0)
        }
        if (q < 0.0) -zAbs else zAbs
      }
    }
  }
}
