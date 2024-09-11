structure BuildExistsBroTree =
struct
  fun helpExists (pos, tree, acc) =
    if pos = Vector.length WordsList.words then
      acc
    else
      let
        val word = Vector.sub (WordsList.words, pos)
        val newAcc = BroTree.exists (word, tree)
        val acc = newAcc orelse acc
      in
        helpExists (pos + 1, tree, acc)
      end

  fun exists (tree) = helpExists (0, tree, true)

  fun main () =
    let
      val endTree = Vector.foldl BroTree.insert BroTree.empty WordsList.words

      val startTime = Time.now ()
      val wordsExist = exists endTree
      val finishTime = Time.now ()

      val searchDuration = Time.- (finishTime, startTime)
      val searchDuration = Time.toMilliseconds searchDuration
      val searchDuration = LargeInt.toString searchDuration ^ "\n"
    in
      print searchDuration
    end
end

val _ = BuildExistsBroTree.main ()
