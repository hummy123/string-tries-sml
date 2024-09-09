structure InsertStringSet =
struct
  fun main () =
  let
    val endTrie = Vector.foldl StringSet.insert StringSet.empty WordsList.words
  in
    ()
  end
end

val _ = InsertStringSet.main ()
