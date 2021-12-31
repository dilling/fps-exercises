import scala.annotation.tailrec

sealed trait ShiftCode

object ShiftCode {
    val aIdx = 'A'.toInt
    val zIdx = 'Z'.toInt
    val zeroIdx = '0'.toInt

    def key(code: String): LazyList[Int] = {
        val codeList = code.toList.map(_.toInt - zeroIdx)
        LazyList.continually(codeList).flatten
    }

    def applyCode(code: String)(op: (Int, Int) => Int)(message: String) = {
        @tailrec
        def go(a1: List[Char], a2: LazyList[Int], z: List[Char]): List[Char] =  
            (a1, a2) match {
                case (h :: t, keyList) if (h < aIdx || h > zIdx) => go(t, keyList, h :: z)
                case (h :: t, key #:: next) => go(t, next, op(h.toInt, key).toChar :: z)
                case _ => z
            }
        go(message.toUpperCase.toList.reverse, key(code), Nil).mkString
    }

    def decode(code: String)(message: String): String = {
        def f(char: Int, key: Int) = {
            val c = char + key
            if c > zIdx then c - 26 else c
        }
        applyCode(code)(f)(message)
    }

    def encode(code: String)(message: String): String = {
        def f(char: Int, key: Int) = {
            val c = char - key
            if c < aIdx then c + 26 else c
        }
        applyCode(code)(f)(message)
    }
}

val code = "123"
val encode = ShiftCode.encode(code)
val decode = ShiftCode.decode(code)

val message = "this is a test message. zzz!!"
val encodedMessage = encode(message)
decode(encodedMessage)


val fromDad = "RJ FKXC TNT ZNPKC GNDM TP SCHR TDZJ. ZIVVXR DQZZS QN NDD VNP. HLMQZRRFUZ INY NI RNIUDMF QGZ LTOCZQ. VB KJUD VNP."
ShiftCode.decode("15113")(fromDad)

val toDad = "Thanks for the message, and for planning this trip. It was great to spend mom's birthday together. We love you both!"
ShiftCode.encode("7305")(toDad)