structure StringSet =
struct
  datatype t =
    CHILDREN of {keys: string vector, children: t vector}
  | FOUND_WITH_CHILDREN of {keys: string vector, children: t vector}
  | FOUND

  val empty =
    CHILDREN {keys = Vector.fromList [], children = Vector.fromList []}

  fun isEmpty trie = trie = empty

  fun fromString str =
    if String.size str > 0 then
      CHILDREN
        {keys = Vector.fromList [str], children = Vector.fromList [FOUND]}
    else
      empty

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
               in
                 (case searchKeyMatch (searchKey, trieKey, keyPos + 1) of
                    NO_SEARCH_MATCH => false
                  | FULL_SEARCH_MATCH =>
                      let val trieChild = Vector.sub (children, idx)
                      in isFoundNode trieChild
                      end
                  | SEARCH_KEY_CONTAINS_TRIE_KEY =>
                      let val trieChild = Vector.sub (children, idx)
                      in helpExists (searchKey, String.size trieKey, trieChild)
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
               in
                 (case searchKeyMatch (searchKey, trieKey, keyPos + 1) of
                    NO_SEARCH_MATCH => false
                  | FULL_SEARCH_MATCH =>
                      let val trieChild = Vector.sub (children, idx)
                      in isFoundNode trieChild
                      end
                  | SEARCH_KEY_CONTAINS_TRIE_KEY =>
                      let val trieChild = Vector.sub (children, idx)
                      in helpExists (searchKey, String.size trieKey, trieChild)
                      end
                  | TRIE_KEY_CONTAINS_SEARCH_KEY => false)
               end
           | NONE => false)
        end
    | FOUND =>
        (*
         * This case should only occur if we recurse in a node 
         * when there is a partial, but not full, string match.
         * This is because of the isFoundNode helper function
         * which is called by the parent.
         * In other words, only the parent node returns true,
         * by checking if the child is a found node and has
         * a full string match.
         *)
        false

  fun exists (searchKey, trie) =
    if isEmpty trie orelse String.size searchKey = 0 then false
    else helpExists (searchKey, 0, trie)

  datatype prefix_result =
    PREFIX_FOUND of string * t
  | NO_PREFIX_FOUND
  | PREFIX_MATCHES_WHOLE_TRIE

  fun helpGetPrefixSubtrieChildren (prefix, keyPos, keys, children, trie) =
    let
      val findChr = String.sub (prefix, keyPos)
    in
      case findBinSearch (findChr, keyPos, keys) of
        SOME idx =>
          let
            val trieKey = Vector.sub (keys, idx)
          in
            (case searchKeyMatch (prefix, trieKey, keyPos + 1) of
               NO_SEARCH_MATCH => NO_PREFIX_FOUND
             | SEARCH_KEY_CONTAINS_TRIE_KEY =>
                 let
                   val trieChild = Vector.sub (children, idx)
                 in
                   helpGetPrefixSubtrie (prefix, String.size trieKey, trieChild)
                 end
             | FULL_SEARCH_MATCH =>
                 let val node = Vector.sub (children, idx)
                 in PREFIX_FOUND (prefix, node)
                 end
             | TRIE_KEY_CONTAINS_SEARCH_KEY =>
                 let val node = Vector.sub (children, idx)
                 in PREFIX_FOUND (trieKey, node)
                 end)
          end
      | NONE => NO_PREFIX_FOUND
    end

  and helpGetPrefixSubtrie (prefix, keyPos, trie) =
    case trie of
      CHILDREN {keys, children} =>
        helpGetPrefixSubtrieChildren (prefix, keyPos, keys, children, trie)
    | FOUND_WITH_CHILDREN {keys, children} =>
        helpGetPrefixSubtrieChildren (prefix, keyPos, keys, children, trie)
    | FOUND => NO_PREFIX_FOUND

  fun getPrefixSubtrie (prefix, trie) =
    if isEmpty trie then NO_PREFIX_FOUND
    else if String.size prefix > 0 then helpGetPrefixSubtrie (prefix, 0, trie)
    else PREFIX_MATCHES_WHOLE_TRIE

  fun recurseHelpGetPrefixList (pos, keys, children, acc) =
    if pos < 0 then
      acc
    else
      let
        val curChild = Vector.sub (children, pos)
        val acc = helpGetPrefixList (curChild, acc)
        val acc =
          if isFoundNode curChild then Vector.sub (keys, pos) :: acc else acc
      in
        recurseHelpGetPrefixList (pos - 1, keys, children, acc)
      end

  and helpGetPrefixList (trie, acc) =
    case trie of
      CHILDREN {keys, children} =>
        recurseHelpGetPrefixList (Vector.length keys - 1, keys, children, acc)
    | FOUND_WITH_CHILDREN {keys, children} =>
        recurseHelpGetPrefixList (Vector.length keys - 1, keys, children, acc)
    | FOUND => acc

  fun getPrefixList (prefix, trie) =
    case getPrefixSubtrie (prefix, trie) of
      PREFIX_FOUND (prefix, subtrie) =>
        let val lst = helpGetPrefixList (subtrie, [])
        in if isFoundNode subtrie then prefix :: lst else lst
        end
    | NO_PREFIX_FOUND => []
    | PREFIX_MATCHES_WHOLE_TRIE => helpGetPrefixList (trie, [])

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
        linearSearch (findChr, keyPos, if mid >= 0 then mid else 0, children)
    end

  fun insertBinSearch (findChr, keyPos, children) =
    helpInsertBinSearch
      (findChr, keyPos, children, 0, Vector.length children - 1)

  fun insertDifferenceFoundAt
    ( insKey
    , insIdx
    , splitTrieKeyStart
    , trieChild
    , childKeys
    , childChildren
    , parentKeys
    , parentChildren
    , parentConstructor
    ) =
    let
      (* child node should always have CHILDREN case,
       * because we are splitting prefix into two,
       * when neither trieKey nor insKey match the prefix. *)
      val childNode = CHILDREN {keys = childKeys, children = childChildren}
      val keys =
        Vector.mapi
          (fn (idx, key) => if idx <> insIdx then key else splitTrieKeyStart)
          parentKeys

      val children =
        Vector.mapi (fn (idx, elt) => if idx <> insIdx then elt else childNode)
          parentChildren
    in
      parentConstructor {keys = keys, children = children}
    end

  fun insertNewChild (keys, insIdx, insKey, children, constructor) =
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
    in
      constructor {keys = newKeys, children = newChildren}
    end

  fun appendNewChild (keys, insKey, children, constructor) =
    let
      val newKeys = Vector.concat [keys, Vector.fromList [insKey]]
      val newChildren = Vector.concat [children, Vector.fromList [FOUND]]
    in
      constructor {keys = newKeys, children = newChildren}
    end

  fun foundInsertPos (keys, children, keyPos, insKey, insIdx, trie, constructor) =
    let
      val trieKey = Vector.sub (keys, insIdx)
    in
      (case insertKeyMatch (insKey, trieKey, keyPos + 1) of
       (* may need to split string if difference found but prefix matched *)
         DIFFERENCE_FOUND_AT diffIdx =>
           let
             val splitTrieKeyStart = String.substring (trieKey, 0, diffIdx)
             val trieChild = Vector.sub (children, insIdx)
           in
             if String.sub (trieKey, diffIdx) > String.sub (insKey, diffIdx) then
               (* place insKey before trieKey *)
               let
                 val childKeys = Vector.fromList [insKey, trieKey]
                 val childChildren = Vector.fromList [FOUND, trieChild]
               in
                 insertDifferenceFoundAt
                   ( insKey
                   , insIdx
                   , splitTrieKeyStart
                   , trieChild
                   , childKeys
                   , childChildren
                   , keys
                   , children
                   , constructor
                   )
               end
             else
               (* place trieKey before insKey *)
               let
                 val childKeys = Vector.fromList [trieKey, insKey]
                 val childChildren = Vector.fromList [trieChild, FOUND]
               in
                 insertDifferenceFoundAt
                   ( insKey
                   , insIdx
                   , splitTrieKeyStart
                   , trieChild
                   , childKeys
                   , childChildren
                   , keys
                   , children
                   , constructor
                   )
               end
           end
       (* may not need to do anything if insert key matched, 
        * as this is a set where only strings are stored. 
        * however, if this is a non-found node, then I need to change
        * the tag/case. *)
       | FULL_INSERT_MATCH =>
           (* in case of a full match, 
            * returned node should always be FOUND_WITH_CHILDREN,
            * because full match means this key was inserted into the trie. *)
           FOUND_WITH_CHILDREN {keys = keys, children = children}
       (* if insert key contains trie key, need to recurse down node *)
       | INSERT_KEY_CONTAINS_TRIE_KEY =>
           let
             val trieChild = Vector.sub (children, insIdx)
             val newTrieChild =
               helpInsert (insKey, String.size trieKey, trieChild)
             val newChildren =
               Vector.mapi
                 (fn (idx, elt) => if idx <> insIdx then elt else newTrieChild)
                 children
           in
             constructor {keys = keys, children = newChildren}
           end
       (* if trie key contains insert key, need to split node *)
       | TRIE_KEY_CONTAINS_INSERT_KEY =>
           let
             val trieChild = Vector.sub (children, insIdx)
             val newKeys =
               Vector.mapi
                 (fn (idx, key) => if idx <> insIdx then key else insKey) keys

             (* newTrieChild should always be FOUND_WITH_CHILDREN,
              * because previous part matches insert key, 
              * and esecond part matches trieKey *)
             val newTrieChild = FOUND_WITH_CHILDREN
               { keys = Vector.fromList [trieKey]
               , children = Vector.fromList [trieChild]
               }

             val newChildren =
               Vector.mapi
                 (fn (idx, elt) => if idx <> insIdx then elt else newTrieChild)
                 children
           in
             constructor {keys = newKeys, children = newChildren}
           end
       | NO_INSERT_MATCH => trie)
    end

  and helpInsert (insKey, keyPos, trie) : t =
    case trie of
      FOUND =>
        if keyPos = String.size insKey then
          FOUND
        else
          FOUND_WITH_CHILDREN
            { keys = Vector.fromList [insKey]
            , children = Vector.fromList [FOUND]
            }
    | CHILDREN {keys, children} =>
        let
          val findChr = String.sub (insKey, keyPos)
        in
          (case insertBinSearch (findChr, keyPos, keys) of
             INSERT_NEW_CHILD insIdx =>
               insertNewChild (keys, insIdx, insKey, children, CHILDREN)
           | FOUND_INSERT_POS insIdx =>
               foundInsertPos
                 (keys, children, keyPos, insKey, insIdx, trie, CHILDREN)
           | APPEND_NEW_CHILD =>
               appendNewChild (keys, insKey, children, CHILDREN))
        end
    | FOUND_WITH_CHILDREN {keys, children} =>
        let
          val findChr = String.sub (insKey, keyPos)
        in
          (case insertBinSearch (findChr, keyPos, keys) of
             INSERT_NEW_CHILD insIdx =>
               insertNewChild
                 (keys, insIdx, insKey, children, FOUND_WITH_CHILDREN)
           | FOUND_INSERT_POS insIdx =>
               foundInsertPos
                 ( keys
                 , children
                 , keyPos
                 , insKey
                 , insIdx
                 , trie
                 , FOUND_WITH_CHILDREN
                 )
           | APPEND_NEW_CHILD =>
               appendNewChild (keys, insKey, children, FOUND_WITH_CHILDREN))
        end

  fun insert (insKey, trie) =
    if isEmpty trie then fromString insKey
    else if String.size insKey > 0 then helpInsert (insKey, 0, trie)
    else trie

  fun helpAddList (str, acc) = insert (str, acc)

  fun addList (lst, trie) =
    List.foldl helpAddList trie lst

  fun fromList (hd :: tl) =
        let val trie = fromString hd
        in addList (tl, trie)
        end
    | fromList ([]) = empty

(* 
 * todo:
 * - Add removal functionality to remove a key from the list,
 *   or to mark it is non-found if the key is a prefix 
 *   of other children.
 *)
end
