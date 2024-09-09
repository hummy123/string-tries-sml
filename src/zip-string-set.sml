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

structure ZipStringSet =
struct
  datatype t =
    CHILDREN of
      { leftKeys: string vector list
      , leftChildren: t vector list
      , rightKeys: string vector list
      , rightChildren: t vector list
      }
  | FOUND_WITH_CHILDREN of
      { leftKeys: string vector list
      , leftChildren: t vector list
      , rightKeys: string vector list
      , rightChildren: t vector list
      }
  | FOUND

  val maxSize = 32

  val empty = CHILDREN
    {leftKeys = [], leftChildren = [], rightKeys = [], rightChildren = []}

  fun isEmpty trie =
    case trie of
      CHILDREN {leftKeys = [], rightKeys = [], ...} => true
    | _ => false

  fun fromString str =
    if String.size str > 0 then
      CHILDREN
        { leftKeys = [Vector.fromList [str]]
        , leftChildren = [Vector.fromList [FOUND]]
        , rightKeys = []
        , rightChildren = []
        }
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
    helpBinSearch (findChr, keyPos, children, 0, Vector.length children - 1)

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

  fun checkNodeMatch (searchKey, trieKey, keyPos, children, idx) =
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

  and checkExistsLeft (searchKey, searchChr, keyPos, leftKeys, leftChildren) =
    case (leftKeys, leftChildren) of
      (khd :: ktl, chd :: ctl) =>
        let
          val firstNode = Vector.sub (khd, 0)
          val firstNodeChr = String.sub (firstNode, keyPos)
        in
          if firstNodeChr < searchChr then
            (* keep checking leftwards *)
            checkExistsLeft (searchKey, searchChr, keyPos, ktl, ctl)
          else if firstNodeChr = searchChr then
            (* check if there is a full/partial key match at this node *)
            checkNodeMatch (searchKey, firstNode, keyPos, chd, 0)
          else
            (* binary search this node to see if there is a matching chr *)
            (case findBinSearch (searchChr, keyPos, khd) of
               SOME idx =>
                 checkNodeMatch
                   (searchKey, Vector.sub (khd, idx), keyPos, chd, idx)
             | NONE => false)
        end
    | (_, _) => false

  and checkExistsRight (searchKey, searchChr, keyPos, rightKeys, rightChildren) =
    case (rightKeys, rightChildren) of
      (khd :: ktl, chd :: ctl) =>
        let
          val lastNode = Vector.sub (khd, Vector.length khd - 1)
          val lastNodeChr = String.sub (lastNode, keyPos)
        in
          if lastNodeChr > searchChr then
            (* keep checking rightwards *)
            checkExistsRight (searchKey, searchChr, keyPos, ktl, ctl)
          else if lastNodeChr = searchChr then
            (* check for full/partial match at this node *)
            checkNodeMatch
              (searchKey, lastNode, keyPos, chd, Vector.length khd - 1)
          else
            (* binary search this node to see if there is a matching chr *)
            (case findBinSearch (searchChr, keyPos, khd) of
               SOME idx =>
                 checkNodeMatch
                   (searchKey, Vector.sub (khd, idx), keyPos, chd, idx)
             | NONE => false)
        end
    | (_, _) => false

  and decideExistsDirection
    (searchKey, keyPos, leftKeys, leftChildren, rightKeys, rightChildren) =
    case (leftKeys, leftChildren) of
      (khd :: ktl, chd :: ctl) =>
        let
          val searchChr = String.sub (searchKey, keyPos)
          val firstNode = Vector.sub (khd, 0)
          val startNodeChr = String.sub (firstNode, keyPos)
        in
          if searchChr < startNodeChr then
            checkExistsLeft (searchKey, searchChr, keyPos, ktl, ctl)
          else if searchChr = startNodeChr then
            (* check string match on first key/firstNode
             * and recurse if full or partial match *)
            checkNodeMatch (searchKey, firstNode, keyPos, chd, 0)
          else
            (* implicit: searchChr > startNodeChr *)
            let
              val lastNode = Vector.sub (khd, Vector.length khd - 1)
              val lastNodeChr = String.sub (lastNode, keyPos)
            in
              if searchChr > lastNodeChr then
                (* check rightKeys/rightChildren *)
                checkExistsRight
                  (searchKey, searchChr, keyPos, rightKeys, rightChildren)
              else if searchChr = lastNodeChr then
                (* check string match on last key/lastNode
                 * and recurse if full or partial match *)
                checkNodeMatch
                  (searchKey, lastNode, keyPos, chd, Vector.length khd - 1)
              else
                (* implicit: searchChr < lastNodeChr 
                 * should perform binary search at this node 
                 * to find if key exists *)
                (case findBinSearch (searchChr, keyPos, khd) of
                   SOME idx =>
                     checkNodeMatch
                       (searchKey, Vector.sub (khd, idx), keyPos, chd, idx)
                 | NONE => false)
            end
        end
    | (_, _) =>
        (* leftKeys and leftChildren are both empty 
         * so check rightKeys/rightChildren *)
        checkExistsRight
          ( searchKey
          , String.sub (searchKey, keyPos)
          , keyPos
          , rightKeys
          , rightChildren
          )

  and recurseExists (searchKey, keyPos, trie) =
    case trie of
      CHILDREN {leftKeys, leftChildren, rightKeys, rightChildren} =>
        decideExistsDirection
          (searchKey, keyPos, leftKeys, leftChildren, rightKeys, rightChildren)
    | FOUND_WITH_CHILDREN {leftKeys, leftChildren, rightKeys, rightChildren} =>
        decideExistsDirection
          (searchKey, keyPos, leftKeys, leftChildren, rightKeys, rightChildren)
    | FOUND => false

  fun exists (searchKey, trie) =
    if isEmpty trie orelse String.size searchKey = 0 then false
    else recurseExists (searchKey, 0, trie)
end
