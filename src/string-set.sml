signature STRING_SET =
sig
  (* the type of tries *)
  type t

  (* the empty trie *)
  val empty: t

  (* StringSet.isEmpty trie
   * returns true if the trie is empty *)
  val isEmpty: t -> bool

  (* StringSet.fromString "hello world"
   * creates a trie containing just a string *)
  val fromString: string -> t

  (* StringSet.exists ("hello world", trie)
   * returns true if the key was inserted into the trie *)
  val exists: string * t -> bool

  (* StringSet.insert ("myNewString", trie)
   * inserts a new string into the trie, returning a new trie *)
  val insert: string * t -> t

  (* StringSet.remove ("stringToRemove", trie)
   * removes the key from the trie, returning a new trie *)
  val remove: string * t -> t

  (* StringSet.getPrefixList ("myPrefix", trie)
   * returns a list of all keys matching the specified prefix *)
  val getPrefixList: string * t -> string list

  (* StringSet.toList trie
   * returns a list containing all keys in the trie *)
  val toList: t -> string list

  (* StringSet.fromList ["hello", "world"]
   * returns a trie containing all keys in the string list *)
  val fromList: string list -> t

  (* StringSet.foldl (fn (key, acc) => String.size key + acc) 0 trie
   * folds a value through the trie, from lowest to highest. *)
  val foldl: (string * 'b -> 'b) -> 'b -> t -> 'b

  (* StringSet.foldlWithPrefix (fn (key, acc) => String.size key + acc) 0 trie "myPrefix"
   * folds a value through a subset of the trie containing the specified prefix,
   * from lowest to highest. *)
  val foldlWithPrefix: (string * 'b -> 'b) -> 'b -> t -> string -> 'b

  (* StringSet.foldr (fn (key, acc) => String.size key + acc) 0 trie
   * folds a value through the trie, from highest to lowest. *)
  val foldr: (string * 'b -> 'b) -> 'b -> t -> 'b

  (* StringSet.foldrWithPrefix (fn (key, acc) => String.size key + acc) 0 trie "myPrefix"
   * folds a value through a subset of the trie containing the specified prefix,
   * from highest to lowest. *)
  val foldrWithPrefix: (string * 'b -> 'b) -> 'b -> t -> string -> 'b
end

structure StringSet: STRING_SET =
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

  fun checkChildrenExists (searchKey, keyPos, keys, children) =
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
                  in recurseExists (searchKey, String.size trieKey, trieChild)
                  end
              | TRIE_KEY_CONTAINS_SEARCH_KEY => false)
           end
       | NONE => false)
    end

  and recurseExists (searchKey, keyPos, trie) =
    case trie of
      CHILDREN {keys, children} =>
        checkChildrenExists (searchKey, keyPos, keys, children)
    | FOUND_WITH_CHILDREN {keys, children} =>
        checkChildrenExists (searchKey, keyPos, keys, children)
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
    else recurseExists (searchKey, 0, trie)

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
    if String.size prefix > 0 then
      if isEmpty trie then NO_PREFIX_FOUND
      else helpGetPrefixSubtrie (prefix, 0, trie)
    else
      PREFIX_MATCHES_WHOLE_TRIE

  fun helpFoldlTrieVector (f, pos, keys, children, acc) =
    if pos = Vector.length children then
      acc
    else
      let
        val curChild = Vector.sub (children, pos)
        val acc = helpFoldl (f, curChild, acc)
        val acc =
          if isFoundNode curChild then f (Vector.sub (keys, pos), acc) else acc
      in
        helpFoldlTrieVector (f, pos + 1, keys, children, acc)
      end

  and helpFoldl (f, trie, acc) =
    case trie of
      CHILDREN {keys, children} =>
        helpFoldlTrieVector (f, 0, keys, children, acc)
    | FOUND_WITH_CHILDREN {keys, children} =>
        helpFoldlTrieVector (f, 0, keys, children, acc)
    | FOUND => acc

  fun foldl f initial trie = helpFoldl (f, trie, initial)

  fun foldlWithPrefix f initial trie prefix =
    case getPrefixSubtrie (prefix, trie) of
      PREFIX_FOUND (prefix, subtrie) =>
        let val acc = helpFoldl (f, subtrie, initial)
        in if isFoundNode subtrie then f (prefix, acc) else acc
        end
    | NO_PREFIX_FOUND => initial
    | PREFIX_MATCHES_WHOLE_TRIE => helpFoldl (f, trie, initial)

  fun helpFoldrTrieVector (f, pos, keys, children, acc) =
    if pos < 0 then
      acc
    else
      let
        val curChild = Vector.sub (children, pos)
        val acc = helpFoldr (f, curChild, acc)
        val acc =
          if isFoundNode curChild then f (Vector.sub (keys, pos), acc) else acc
      in
        helpFoldrTrieVector (f, pos - 1, keys, children, acc)
      end

  and helpFoldr (f, trie, acc) =
    case trie of
      CHILDREN {keys, children} =>
        helpFoldrTrieVector (f, Vector.length keys - 1, keys, children, acc)
    | FOUND_WITH_CHILDREN {keys, children} =>
        helpFoldrTrieVector (f, Vector.length keys - 1, keys, children, acc)
    | FOUND => acc

  fun foldr f initial trie = helpFoldr (f, trie, initial)

  fun foldrWithPrefix f initial trie prefix =
    case getPrefixSubtrie (prefix, trie) of
      PREFIX_FOUND (prefix, subtrie) =>
        let val acc = helpFoldr (f, subtrie, initial)
        in if isFoundNode subtrie then f (prefix, acc) else acc
        end
    | NO_PREFIX_FOUND => initial
    | PREFIX_MATCHES_WHOLE_TRIE => helpFoldr (f, trie, initial)

  (* recurseHelpGetPrefixList and helpGetPrefixList are basically manually coded
   * foldr functions over the trie, applying the accumuluator to every found
   * node, from right to left.
   * No need to recode it as a usage of a generic foldr function though,
   * because lower dispatch cost this way. *)
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

  fun toList trie = helpGetPrefixList (trie, [])

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
            * need to turn child into FOUND_WITH_CHILDREN
            * or FOUND case, if it not already either
            *)
           (case Vector.sub (children, insIdx) of
              CHILDREN {keys = childKeys, children = childChildren} =>
                let
                  val newTrieChild =
                    FOUND_WITH_CHILDREN
                      {keys = childKeys, children = childChildren}
                  val newParentChildren =
                    Vector.mapi
                      (fn (childIdx, elt) =>
                         if insIdx <> childIdx then elt else newTrieChild)
                      children
                in
                  constructor {keys = keys, children = newParentChildren}
                end
            | _ => trie)
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
    if String.size insKey > 0 then
      if isEmpty trie then fromString insKey else helpInsert (insKey, 0, trie)
    else
      trie

  fun helpAddList (str, acc) = insert (str, acc)

  fun addList (lst, trie) =
    List.foldl helpAddList trie lst

  fun fromList (hd :: tl) =
        let val trie = fromString hd
        in addList (tl, trie)
        end
    | fromList ([]) = empty

  datatype remove_result = UNCHANGED | MADE_EMPTY | CHANGED of t

  (* should be called when there is a FULL_SEARCH_MATCH
   * and child is a terminal FOUND node *)
  fun removeWhenChildIsMadeEmpty
    (idx, keys, children, isFoundWithChildren, parentConstructor) =
    (* if child was made empty, then:
     * -  if the parent only has 1 child, it should be MADE_EMPTY too
     * -  otherwise, just remove the key and child at this idx from parent
     * *)
    if Vector.length keys > 1 then
      if idx > 0 then
        let
          val newKeys = Vector.tabulate (Vector.length keys - 1, fn keyIdx =>
            Vector.sub (keys, if keyIdx >= idx then keyIdx - 1 else keyIdx))

          val newChildren =
            Vector.tabulate (Vector.length keys - 1, fn childIdx =>
              Vector.sub
                (children, if childIdx >= idx then childIdx - 1 else childIdx))

          val newNode =
            parentConstructor {keys = newKeys, children = newChildren}
        in
          CHANGED newNode
        end
      else
        (* if idx = 0, then have to slice first element off from vector *)
        let
          val keySlice = VectorSlice.slice (keys, 1, SOME
            (Vector.length keys - 1))
          val newKeys = VectorSlice.vector keySlice

          val childrenSlice = VectorSlice.slice (children, 1, SOME
            (Vector.length children - 1))
          val newChildren = VectorSlice.vector childrenSlice

          val newNode =
            parentConstructor {keys = newKeys, children = newChildren}
        in
          CHANGED newNode
        end
    else (* if the caller was from the FOUND_WITH_CHILDREN case,
          * then, instead of deleting this node entirely, 
          * we only delete this node's children, and mark this node 
          * as found. 
          * However, in general case where this is a CHILDREN node 
          * whose key was not inserted into the trie, 
          * we should fully delete this node as well. *) if isFoundWithChildren then
      CHANGED FOUND
    else
      MADE_EMPTY

  (* should be called when searchKeyMatch returns FULL_MATCH
   *in helpRemove function *)
  fun removeWhenFullMatch
    (idx, keys, children, isFoundWithChildren, parentConstructor) =
    (* matching over the child at this idx *)
    case Vector.sub (children, idx) of
    (* CHILDREN is a not-found case, so have to leave parent unchanged
     * as there is no key to delete. *)
      CHILDREN _ => UNCHANGED
    (* FOUND_WITH_CHILDREN is a found case containing links to other nodes
     * so we just need to change the tag from FOUND_WITH_CHILDREN to CHILDREN *)
    | FOUND_WITH_CHILDREN {keys = childKeys, children = childChildren} =>
        let
          val newChild = CHILDREN {keys = childKeys, children = childChildren}
          val newParentChildren =
            Vector.mapi
              (fn (mapIdx, elt) => if mapIdx <> idx then elt else newChild)
              children

          val newParent =
            parentConstructor {keys = keys, children = newParentChildren}
        in
          CHANGED newParent
        end
    | FOUND =>
        removeWhenChildIsMadeEmpty
          (idx, keys, children, isFoundWithChildren, parentConstructor)

  fun removeWhenSearchKeyContainsTrieKey
    (childResult, idx, keys, children, isFoundWithChildren, parentConstructor) =
    case childResult of
    (* if result is UNCHANGED, let UNCHANGED bubble to the top.
     * At the top, can return same trie given as input as there was no
     * change. *)
      UNCHANGED => UNCHANGED
    (* if child was changed, allocate new vector where child at this idx
     * is updated with newChild, and use it in parent node. *)
    | CHANGED newChild =>
        let
          val newChildren =
            Vector.mapi
              (fn (childIdx, elt) => if idx <> childIdx then elt else newChild)
              children

          val newNode = parentConstructor {keys = keys, children = newChildren}
        in
          CHANGED newNode
        end
    | MADE_EMPTY =>
        removeWhenChildIsMadeEmpty
          (idx, keys, children, isFoundWithChildren, parentConstructor)

  fun helpRemove (removeKey, keyPos, trie) =
    case trie of
      CHILDREN {keys, children} =>
        let
          val findChr = String.sub (removeKey, keyPos)
        in
          (case findBinSearch (findChr, keyPos, keys) of
             SOME idx =>
               let
                 val trieKey = Vector.sub (keys, idx)
               in
                 (case searchKeyMatch (removeKey, trieKey, keyPos + 1) of
                  (* no search match means nothing to delete *)
                    NO_SEARCH_MATCH => UNCHANGED
                  | FULL_SEARCH_MATCH =>
                      removeWhenFullMatch (idx, keys, children, false, CHILDREN)
                  | SEARCH_KEY_CONTAINS_TRIE_KEY =>
                      removeWhenSearchKeyContainsTrieKey
                        ( helpRemove
                            ( removeKey
                            , String.size trieKey
                            , Vector.sub (children, idx)
                            )
                        , idx
                        , keys
                        , children
                        , false
                        , CHILDREN
                        )
                  | TRIE_KEY_CONTAINS_SEARCH_KEY => UNCHANGED)
               end
           | NONE => UNCHANGED)
        end
    | FOUND_WITH_CHILDREN {keys, children} =>
        let
          val findChr = String.sub (removeKey, keyPos)
        in
          (case findBinSearch (findChr, keyPos, keys) of
             SOME idx =>
               let
                 val trieKey = Vector.sub (keys, idx)
               in
                 (case searchKeyMatch (removeKey, trieKey, keyPos + 1) of
                  (* no search match means nothing to delete *)
                    NO_SEARCH_MATCH => UNCHANGED
                  | FULL_SEARCH_MATCH =>
                      removeWhenFullMatch
                        (idx, keys, children, true, FOUND_WITH_CHILDREN)
                  | SEARCH_KEY_CONTAINS_TRIE_KEY =>
                      removeWhenSearchKeyContainsTrieKey
                        ( helpRemove
                            ( removeKey
                            , String.size trieKey
                            , Vector.sub (children, idx)
                            )
                        , idx
                        , keys
                        , children
                        , true
                        , FOUND_WITH_CHILDREN
                        )
                  | TRIE_KEY_CONTAINS_SEARCH_KEY => UNCHANGED)
               end
           | NONE => UNCHANGED)
        end
    | FOUND =>
        (*
         * This case should only occur if we recurse in a node 
         * when there is a partial, but not full, string match.
         * Since there was no full string match, 
         * key doesn't exist in trie and so we must leave it unchanged.
         *)
        UNCHANGED

  fun remove (removeKey, trie) =
    if String.size removeKey = 0 orelse isEmpty trie then
      trie
    else
      case helpRemove (removeKey, 0, trie) of
        CHANGED trie => trie
      | MADE_EMPTY => empty
      | UNCHANGED => trie
end
