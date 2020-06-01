package com.tsp

import org.specs2.mutable.SpecificationWithJUnit

class ConfigSpecs extends SpecificationWithJUnit {
  "Genetic Algorithm Config" should {
    "Have consistent Probabilities" in {
      import Config.GA.Probability._
      math.abs(crossover + mutation + random - 1.0) < 2 * Double.MinPositiveValue
    }
  }
}
