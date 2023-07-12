package Musica

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers._

class MusicaTest extends AnyFreeSpec {
  "Notas" - {
    "sostenido debe ser la siguiente nota" in {
      C.sostenido shouldBe Cs
      B.sostenido shouldBe C
    }

    "bemol debe ser la nota anterior" in {
      C.bemol shouldBe B
      B.bemol shouldBe As
    }
  }
}
