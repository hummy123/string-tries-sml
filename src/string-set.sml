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
    else (* implicit: keyPos > String.size searchKey *) if
      keyPos <= String.size trieKey
    then
      TRIE_KEY_CONTAINS_SEARCH_KEY
    else
      NO_SEARCH_MATCH

  fun isFoundNode node =
    case node of
      FOUND => true
    | FOUND_WITH_CHILDREN _ => true
    | CHILDREN _ => false

  fun helpExists (searchKey, keyPos, trie) =
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
                      in helpExists (searchKey, nextKeyPos, trieChild)
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
                      in helpExists (searchKey, nextKeyPos, trieChild)
                      end
                  | TRIE_KEY_CONTAINS_SEARCH_KEY => false)
               end
           | NONE => false)
        end
    | FOUND => keyPos = String.size searchKey

  fun exists (searchKey, trie) = helpExists (searchKey, 0, trie)

  datatype insert_string_match =
    NO_INSERT_MATCH
  (* may need to split string if difference found but prefix matched *)
  | DIFFERENCE_FOUND_AT of int
  (* may not need to do anything if insert key matched, 
   * as this is a set where only strings are stored. 
   * however, if this is a non-found node, then I need to change
   * the tag/case. *)
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
    else (* implicit: keyPos > String.size insertKey *) if
      keyPos <= String.size trieKey
    then
      TRIE_KEY_CONTAINS_INSERT_KEY
    else
      NO_INSERT_MATCH

  datatype insert_bin_search_result =
    INSERT_NEW_CHILD of int
  | FOUND_INSERT_POS of int
  | APPEND_NEW_CHILD

  fun linearSearch (findChr, keyPos, idx, children) =
    if idx = Vector.length children then
      APPEND_NEW_CHILD
    else
      let
        val curStr = Vector.sub (children, idx)
        val curChr = String.sub (curStr, keyPos)
      in
        if curChr > findChr then INSERT_NEW_CHILD idx
        else linearSearch (findChr, keyPos, idx + 1, children)
      end

  fun helpInsertBinSearch (findChr, keyPos, children, low, high) =
    let
      val mid = low + ((high - low) div 2)
    in
      if high >= low then
        let
          val midStr = Vector.sub (children, mid)
          val midChr = String.sub (midStr, keyPos)
        in
          if midChr = findChr then
            FOUND_INSERT_POS mid
          else if midChr < findChr then
            helpInsertBinSearch (findChr, keyPos, children, mid + 1, high)
          else
            helpInsertBinSearch (findChr, keyPos, children, low, mid - 1)
        end
      else
        linearSearch (findChr, keyPos, mid, children)
    end

  fun insertBinSearch (findChr, keyPos, children) =
    helpInsertBinSearch
      (findChr, keyPos, children, 0, Vector.length children - 1)

  (* 
   * todo: 
   * Complete code for CHILDREN and FOUND_WITH_CHILDREN cases in insert function.
   * The start will be similar to the exists function, 
   * with binary search possibly followed by a string matching comparison.
   * No string matching comparison is needed if the current char is not found
   * in the binary search stage, however; 
   * instead, we will insert a new element into the array when that happens.
   *)

  fun insertDifferenceFoundAt
    ( insKey
    , insIdx
    , splitTrieKeyStart
    , splitTrieKeyEnd
    , trieChild
    , childKeys
    , childChildren
    , parentKeys
    , parentChildren
    ) =
    let
      val childNode = CHILDREN {keys = childKeys, children = childChildren}

      val keys =
        Vector.mapi
          (fn (idx, key) => if idx <> insIdx then key else splitTrieKeyStart)
          parentKeys

      val children =
        Vector.mapi (fn (idx, elt) => if idx <> insIdx then elt else childNode)
          parentChildren

      val node = CHILDREN {keys = keys, children = children}
    in
      SOME node
    end


  fun helpInsert (insKey, keyPos, trie) : t option =
    case trie of
      FOUND =>
        let
          val node =
            if keyPos = String.size insKey then
              FOUND
            else
              FOUND_WITH_CHILDREN
                { keys = Vector.fromList [insKey]
                , children = Vector.fromList [FOUND]
                }
        in
          SOME node
        end
    | CHILDREN {keys, children} =>
        let
          val findChr = String.sub (insKey, keyPos)
        in
          (case insertBinSearch (findChr, keyPos, keys) of
             INSERT_NEW_CHILD insIdx =>
               let
                 val newLength = Vector.length keys + 1
                 val newKeys = Vector.tabulate (newLength, fn idx =>
                   if idx < insIdx then Vector.sub (keys, idx)
                   else if idx > insIdx then Vector.sub (keys, idx - 1)
                   else insKey)

                 val newChildren = Vector.tabulate (newLength, fn idx =>
                   if idx < insIdx then Vector.sub (children, idx)
                   else if idx > insIdx then Vector.sub (children, idx - 1)
                   else FOUND)

                 val node = CHILDREN {keys = newKeys, children = newChildren}
               in
                 SOME node
               end
           | FOUND_INSERT_POS insIdx =>
               let
                 val trieKey = Vector.sub (keys, insIdx)
                 val nextKeyPos = keyPos + 1
               in
                 (case insertKeyMatch (insKey, trieKey, nextKeyPos) of
                    NO_INSERT_MATCH => SOME trie
                  (* may need to split string if difference found but prefix matched *)
                  | DIFFERENCE_FOUND_AT diffIdx =>
                      let
                        val splitTrieKeyStart =
                          String.substring (trieKey, 0, diffIdx)
                        val splitTrieKeyEnd = String.substring
                          (trieKey, diffIdx, String.size trieKey - diffIdx)
                        val trieChild = Vector.sub (children, insIdx)
                      in
                        if
                          String.sub (trieKey, nextKeyPos)
                          > String.sub (insKey, nextKeyPos)
                        then
                          (* place insKey before trieKey *)
                          let
                            val childKeys =
                              Vector.fromList [insKey, splitTrieKeyEnd]
                            val childChildren =
                              Vector.fromList [FOUND, trieChild]
                          in
                            insertDifferenceFoundAt
                              ( insKey
                              , insIdx
                              , splitTrieKeyStart
                              , splitTrieKeyEnd
                              , trieChild
                              , childKeys
                              , childChildren
                              , keys
                              , children
                              )
                          end
                        else
                          (* place trieKey before insKey *)
                          let
                            val childKeys =
                              Vector.fromList [splitTrieKeyEnd, insKey]
                            val childChildren =
                              Vector.fromList [trieChild, FOUND]
                          in
                            insertDifferenceFoundAt
                              ( insKey
                              , insIdx
                              , splitTrieKeyStart
                              , splitTrieKeyEnd
                              , trieChild
                              , childKeys
                              , childChildren
                              , keys
                              , children
                              )
                          end
                      end
                  (* may not need to do anything if insert key matched, 
                   * as this is a set where only strings are stored. 
                   * however, if this is a non-found node, then I need to change
                   * the tag/case. *)
                  | FULL_INSERT_MATCH =>
                      let
                        val node =
                          FOUND_WITH_CHILDREN {keys = keys, children = children}
                      in
                        SOME node
                      end
                  (* if insert key contains trie key, need to recurse down node *)
                  | INSERT_KEY_CONTAINS_TRIE_KEY =>
                      let
                        val trieChild = Vector.sub (children, insIdx)
                      in
                        (case
                           helpInsert
                             (insKey, keyPos + String.size trieKey, trieChild)
                         of
                           (result as SOME _) => result
                         | NONE => SOME trie)
                      end
                  (* if trie key contains insert key, need to split node *)
                  | TRIE_KEY_CONTAINS_INSERT_KEY =>
                      let
                        val trieChild = Vector.sub (children, insIdx)
                        val newKeys =
                          Vector.mapi
                            (fn (idx, key) =>
                               if idx <> insIdx then insKey else key) keys

                        val splitTrieKeyEnd = String.substring
                          ( trieKey
                          , String.size insKey
                          , String.size trieKey - String.size insKey
                          )
                        val newTrieChild = FOUND_WITH_CHILDREN
                          { keys = Vector.fromList [splitTrieKeyEnd]
                          , children = Vector.fromList [trieChild]
                          }

                        val newChildren =
                          Vector.mapi
                            (fn (idx, elt) =>
                               if idx <> insIdx then elt else newTrieChild)
                            children

                        val node =
                          CHILDREN {keys = newKeys, children = newChildren}
                      in
                        SOME node
                      end)
               end
           | APPEND_NEW_CHILD =>
               let
                 val newKeys = Vector.concat [keys, Vector.fromList [insKey]]
                 val newChildren = Vector.concat
                   [children, Vector.fromList [FOUND]]
                 val node = CHILDREN {keys = newKeys, children = newChildren}
               in
                 SOME node
               end)
        end
    | FOUND_WITH_CHILDREN {keys, children} => raise Empty (* todo *)

  fun insert (insKey, trie) =
    if String.size insKey > 0 then
      case helpInsert (insKey, 0, trie) of
        SOME trie => trie
      | NONE => trie
    else
      trie
end
