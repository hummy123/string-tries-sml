(* generate a words.sml file with a vector of strings, 
 * from a line-delimited words.txt file *)
val inIo = TextIO.openIn "words.txt"
val outIO = TextIO.openOut "words.sml"

fun readLines (inIo, acc) =
  case TextIO.inputLine inIo of
    SOME word => readLines (inIo, word :: acc)
  | NONE => List.rev acc

fun writeLines (outIO, lst) =
  case lst of
    [] => ()
  | word :: tl =>
      let
        (* remove \r and \n from the word *)
        val word = Substring.full word
        val word =
          Substring.dropr (fn chr => chr = #"\n" orelse chr = #"\r") word
        val word = Substring.string word

        val isLast = tl = []
        val word = if isLast then "\"" ^ word ^ "\"" else "\"" ^ word ^ "\",\n"
        val _ = TextIO.output (outIO, word)
      in
        writeLines (outIO, tl)
      end

fun main () =
  let
    val lst = readLines (inIo, [])
    val _ = TextIO.output
      (outIO, "structure WordsList = \nstruct \n val words = #[\n")
    val _ = writeLines (outIO, lst)
    val _ = TextIO.output (outIO, "]\n end")
  in
    ()
  end
