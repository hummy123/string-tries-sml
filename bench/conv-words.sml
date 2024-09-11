(* generate a words.sml file with a vector of strings, 
 * from a line-delimited words.txt file *)
val inIo = TextIO.openIn "words.txt"
val outIO = TextIO.openOut "words.sml"

fun consWordChrs (wordChrs, acc) =
  case wordChrs of
    [] => acc
  | _ => (String.concat wordChrs) :: acc

fun helpTokeniseLine (pos, wordChrs, line, acc) =
  if pos < 0 then
    consWordChrs (wordChrs, acc)
  else
    let
      val chr = String.sub (line, pos)
      (* using Char.toString is necessary because it escapes double-quotes.
       * Without escaping, there's a chance we will produce an invalid .sml file
       * *)
      val sChr = Char.toString chr
    in
      if Char.isPrint chr andalso not (Char.isSpace chr) then
        helpTokeniseLine (pos - 1, sChr :: wordChrs, line, acc)
      else
        helpTokeniseLine (pos - 1, [], line, consWordChrs (wordChrs, acc))
    end

fun tokeniseLine (line, acc) =
  helpTokeniseLine (String.size line - 1, [], line, acc)

fun readLines (inIo, acc) =
  case TextIO.inputLine inIo of
    SOME line => readLines (inIo, tokeniseLine (line, acc))
  | NONE => List.rev acc

fun writeLines (outIO, lst) =
  case lst of
    [] => ()
  | word :: tl =>
      let
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

val _ = main ()
