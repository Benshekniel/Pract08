import scala.io.StdIn._

object CaesarCipher {

  def encrypt(plaintext: String, shift: Int): String = {
    plaintext.map { char =>
      if (char.isLetter) {
        val offset = if (char.isUpper) 'A' else 'a'
        ((char - offset + shift + 26) % 26 + offset).toChar
      } else {
        char
      }
    }
  }

  def decrypt(ciphertext: String, shift: Int): String = {
    encrypt(ciphertext, -shift)
  }

  def cipher(text: String, shift: Int, processFunction: (String, Int) => String): String = {
    processFunction(text, shift)
  }

  def main(args: Array[String]): Unit = {
    println("Choose an option: 1) Encrypt 2) Decrypt")
    val choice = readInt()

    println("Enter the text:")
    val text = readLine()

    println("Enter the shift value:")
    val shift = readInt()

    choice match {
      case 1 =>
        val encrypted = cipher(text, shift, encrypt)
        println(s"Encrypted: $encrypted")
      case 2 =>
        val decrypted = cipher(text, shift, decrypt)
        println(s"Decrypted: $decrypted")
      case _ =>
        println("Invalid option selected.")
    }
  }
}
