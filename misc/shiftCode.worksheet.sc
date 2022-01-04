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
        def go(a1: List[Char], a2: LazyList[Int]): List[Char] =
            (a1, a2) match {
                case (c :: t, keyList) if (c < aIdx || c > zIdx) => c :: go(t, keyList)
                case (char :: t, key #:: next) => op(char.toInt, key).toChar :: go(t, next)
                case _ => Nil
            }

        go(message.toUpperCase.toList, key(code)).mkString
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

val code = "1234"
val encode = ShiftCode.encode(code)
val decode = ShiftCode.decode(code)

val message = "this is a test zz"
val encodedMessage = encode(message)
decode(encodedMessage)


val fromDad = "RJ FKXC TNT ZNPKC GNDM TP SCHR TDZJ. ZIVVXR DQZZS QN NDD VNP. HLMQZRRFUZ INY NI RNIUDMF QGZ LTOCZQ. VB KJUD VNP."
ShiftCode.decode("15113")(fromDad)

val toDad = "Thanks for the message, and for planning this trip. It was great to spend mom's birthday together. We love you both!"
ShiftCode.encode("7305")(toDad)