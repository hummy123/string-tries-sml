structure BuildGetPrefixBroTree =
struct
  fun main () =
    let
      val endTrie =
        Vector.foldl BroTree.insert BroTree.empty WordsList.words

      val startTime = Time.now ()
      val lst = BroTree.getPrefixList ("a", endTrie)
      val finishTime = Time.now ()

      val searchDuration = Time.- (finishTime, startTime)
      val searchDuration = Time.toNanoseconds searchDuration
      val searchDuration = LargeInt.toString searchDuration ^ " ns\n"
    in
      print searchDuration
    end
end

val _ = BuildGetPrefixBroTree.main ()
