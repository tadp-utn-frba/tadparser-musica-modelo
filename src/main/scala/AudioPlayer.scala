import Musica._
import javax.sound.midi.{MidiChannel, MidiSystem}

object AudioPlayer {
  private val VOLUME = 80
  require(VOLUME <= 127)
  require(VOLUME >= 0)

  def reproducir(melodia: Melodia): Unit = {
    val synth = MidiSystem.getSynthesizer

    synth.open()

    val channel = synth.getChannels().head

    melodia.foreach(tocar(channel))

    synth.close()
  }

  def tocar(channel: MidiChannel)(tocable: Tocable) = {
    tocable match {
      case Sonido(tono, figura) ⇒ {
        prenderTono(channel)(tono)

        descansar(figura.duracion)

        apagarTono(channel)(tono)
      }
      case Silencio(figura) ⇒ descansar(figura.duracion)
      case Acorde(tonos, figura) ⇒ {
        tonos.foreach(prenderTono(channel))

        descansar(figura.duracion)

        tonos.foreach(apagarTono(channel))
      }
    }
  }

  def descansar(duration: Int): Unit = {
    Thread.sleep(duration)
  }

  def prenderTono(channel: MidiChannel)(tono: Tono): Unit = {
    channel.noteOn(midiId(tono), VOLUME)
  }

  def apagarTono(channel: MidiChannel)(tono: Tono): Unit = {
    channel.noteOff(midiId(tono))
  }

  private def midiId(tono: Tono) = {
    val idNota = tono.nota match {
      case C ⇒ 0
      case Cs ⇒ 1
      case D ⇒ 2
      case Ds ⇒ 3
      case E ⇒ 4
      case F ⇒ 5
      case Fs ⇒ 6
      case G ⇒ 7
      case Gs ⇒ 8
      case A ⇒ 9
      case As ⇒ 10
      case B ⇒ 11
    }
    idNota + 12 * tono.octava + 12
  }
}

object PlayMusic extends App {
  // Si solo hubiese una forma de escribir
  // "4F1/8 4A1/8 4B1/2 4F1/8 4A1/8 4B1/2 4F1/8 4A1/8 4B1/4 5Cs1/8 5Ds1/4 5C1/4"
  val melodiaDeEjemplo = List(
    Sonido(Tono(4, F), Corchea),
    Sonido(Tono(4, A), Corchea),
    Sonido(Tono(4, B), Negra),
    Sonido(Tono(4, F), Corchea),
    Sonido(Tono(4, A), Corchea),
    Sonido(Tono(4, B), Blanca),
    Sonido(Tono(4, F), Corchea),
    Sonido(Tono(4, A), Corchea),
    Sonido(Tono(4, B), Negra),
    Sonido(Tono(5, C.sostenido), SemiCorchea),
    Sonido(Tono(5, D.sostenido), Negra),
    Sonido(Tono(5, C), Blanca),
  )

  AudioPlayer.reproducir(melodiaDeEjemplo)
}
