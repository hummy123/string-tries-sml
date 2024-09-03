structure StringSet =
struct
  datatype t =
    CHILDREN of {keys: string vector, children: t vector}
  | FOUND_WITH_CHILDREN of {keys: string vector, children: t vector}
  | FOUND

  fun helpBinSearch (findChr, keyPos, children, low, high) =
    if high >= low then
      let
        val mid = low + ((high - low) div 2)
        val midStr = Vector.sub (children, mid)
        val midChr = String.sub (midStr, keyPos)
      in
        if midChr = findChr then
          SOME mid
        else if midChr < findChr then
          helpBinSearch (findChr, keyPos, children, mid + 1, high)
        else
          helpBinSearch (findChr, keyPos, children, low, mid - 1)
      end
    else
      NONE

  fun findBinSearch (findChr, keyPos, children) =
    if Vector.length children > 0 then
      helpBinSearch (findChr, keyPos, children, 0, Vector.length children - 1)
    else
      NONE

  datatype search_string_match =
    NO_SEARCH_MATCH
  | FULL_SEARCH_MATCH
  | SEARCH_KEY_CONTAINS_TRIE_KEY
  | TRIE_KEY_CONTAINS_SEARCH_KEY

  fun searchKeyMatch (searchKey, trieKey, keyPos) =
    if
      keyPos < String.size searchKey
    then
      if keyPos < String.size trieKey then
        let
          val searchChr = String.sub (searchKey, keyPos)
          val trieChr = String.sub (trieKey, keyPos)
        in
          if searchChr = trieChr then
            searchKeyMatch (searchKey, trieKey, keyPos + 1)
          else
            NO_SEARCH_MATCH
        end
      else
        SEARCH_KEY_CONTAINS_TRIE_KEY
    else if
      keyPos = String.size searchKey
    then
      if keyPos < String.size trieKey then TRIE_KEY_CONTAINS_SEARCH_KEY
      else if keyPos = String.size trieKey then FULL_SEARCH_MATCH
      else SEARCH_KEY_CONTAINS_TRIE_KEY
    else 
      (* implicit: keyPos > String.size searchKey *) 
      if keyPos <= String.size trieKey then
        TRIE_KEY_CONTAINS_SEARCH_KEY
      else
        NO_SEARCH_MATCH

  fun isFoundNode node =
    case node of
      FOUND => true
    | FOUND_WITH_CHILDREN _ => true
    | CHILDREN _ => false

  fun exists (searchKey, keyPos, trie) =
    case trie of
      CHILDREN {keys, children} =>
        let
          val findChr = String.sub (searchKey, keyPos)
        in
          (case findBinSearch (findChr, keyPos, keys) of
             SOME idx =>
               let
                 val trieKey = Vector.sub (keys, idx)
                 val nextKeyPos = keyPos + 1
               in
                 (case searchKeyMatch (searchKey, trieKey, nextKeyPos) of
                    NO_SEARCH_MATCH => false
                  | FULL_SEARCH_MATCH =>
                      let val trieChild = Vector.sub (children, idx)
                      in isFoundNode trieChild
                      end
                  | SEARCH_KEY_CONTAINS_TRIE_KEY =>
                      let val trieChild = Vector.sub (children, idx)
                      in exists (searchKey, nextKeyPos, trieChild)
                      end
                  | TRIE_KEY_CONTAINS_SEARCH_KEY => false)
               end
           | NONE => false)
        end
    | FOUND_WITH_CHILDREN {keys, children} =>
        let
          val findChr = String.sub (searchKey, keyPos)
        in
          (case findBinSearch (findChr, keyPos, keys) of
             SOME idx =>
               let
                 val trieKey = Vector.sub (keys, idx)
                 val nextKeyPos = keyPos + 1
               in
                 (case searchKeyMatch (searchKey, trieKey, nextKeyPos) of
                    NO_SEARCH_MATCH => false
                  | FULL_SEARCH_MATCH =>
                      let val trieChild = Vector.sub (children, idx)
                      in isFoundNode trieChild
                      end
                  | SEARCH_KEY_CONTAINS_TRIE_KEY =>
                      let val trieChild = Vector.sub (children, idx)
                      in exists (searchKey, nextKeyPos, trieChild)
                      end
                  | TRIE_KEY_CONTAINS_SEARCH_KEY => false)
               end
           | NONE => false)
        end
    | FOUND => keyPos = String.size searchKey

  datatype insert_string_match =
    NO_INSERT_MATCH
    (* may need to split string if difference found but prefix matched *)
  | DIFFERENCE_FOUND_AT of int
    (* no need to do anything if insert key matched, 
     * as this is a set where only strings are stored *)
  | FULL_INSERT_MATCH
    (* if insert key contains trie key, may need to recurse down node *)
  | INSERT_KEY_CONTAINS_TRIE_KEY
    (* if trie key contains insert key, need to split node *)
  | TRIE_KEY_CONTAINS_INSERT_KEY

  fun insertKeyMatch (insertKey, trieKey, keyPos) =
    if
      keyPos < String.size insertKey
    then
      if keyPos < String.size trieKey then
        let
          val searchChr = String.sub (insertKey, keyPos)
          val trieChr = String.sub (trieKey, keyPos)
        in
          if searchChr = trieChr then
            insertKeyMatch (insertKey, trieKey, keyPos + 1)
          else
            DIFFERENCE_FOUND_AT keyPos
        end
      else
        INSERT_KEY_CONTAINS_TRIE_KEY
    else if
      keyPos = String.size insertKey
    then
      if keyPos < String.size trieKey then TRIE_KEY_CONTAINS_INSERT_KEY
      else if keyPos = String.size trieKey then FULL_INSERT_MATCH
      else INSERT_KEY_CONTAINS_TRIE_KEY
    else 
      (* implicit: keyPos > String.size insertKey *) 
      if keyPos <= String.size trieKey then
        TRIE_KEY_CONTAINS_INSERT_KEY
      else
        NO_INSERT_MATCH

  (* 
   * todo: 
   * Code another function for binary search; this time for insert.
   * The difference between findBinSearch and insertBinSearch
   * should be that, while findBinSearch returns NONE if the value is not found,
   * insertBinSearch should return the appropriate index to insert at
   * if the value is not found.
   * The main insert function can then create new key/children vectors,
   * containing the insertKey and FOUND at the appropriate index.
   *)

  fun insert (insKey, keyPos, trie) =
    case trie of
      FOUND =>
        if keyPos = String.size insKey then
          FOUND
        else
          FOUND_WITH_CHILDREN
            { keys = Vector.fromList [insKey]
            , children = Vector.fromList [FOUND]
            }
    | CHILDREN {keys, children} => 0 (* todo *)
    | FOUND_WITH_CHILDREN {keys, children} => 0 (* todo *)
end
