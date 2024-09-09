structure InsertBroTree =
struct
  fun main () =
  let
    val endTree = Vector.foldl BroTree.insert BroTree.empty WordsList.words
  in
    ()
  end
end

val _ = InsertBroTree.main ()
