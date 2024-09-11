structure BuildExistsStringSet =
struct
  fun helpExists (pos, trie, acc) =
    if pos = Vector.length WordsList.words then
      acc
    else
      let
        val word = Vector.sub (WordsList.words, pos)
        val newAcc = StringSet.exists (word, trie)
        val acc = newAcc orelse acc
      in
        helpExists (pos + 1, trie, acc)
      end

  fun exists trie = helpExists (0, trie, true)

  fun main () =
    let
      val endTrie =
        Vector.foldl StringSet.insert StringSet.empty WordsList.words

      val startTime = Time.now ()
      val wordsExist = exists endTrie
      val finishTime = Time.now ()

      val searchDuration = Time.- (finishTime, startTime)
      val searchDuration = Time.toMilliseconds searchDuration
      val searchDuration = LargeInt.toString searchDuration ^ "\n"
    in
      print searchDuration
    end
end

val _ = BuildExistsStringSet.main ()
