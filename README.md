# string-trie

This repository implements a set over strings in Standard ML using a trie/prefix tree.

The signature provided is:

```
signature STRING_SET =
sig
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
```

The reason for implementing a new trie specialised to strings rather than using Chris Okasaki's IntMap data structure is to enable prefix searching, where it is possible to get a list of all keys matching a certain prefix.

# To-do

- [x] Add `foldl`, `foldr`, `foldlWithPrefix`, `foldrWithPrefix` functions to string set
- [ ] Benchmarks (possibly comparing to a set of strings in a balanced binary tree)
- [ ] Use unrolled linked list with zipper in trie, to limit size of vector as allocating large vectors repeatedly is expensive
- [ ] Implement StringMap, containing both keys and values

# Credits

- The tests in `tests/string-set-tests.sml` were ported from [kpol's Trie data structure in C3](https://github.com/kpol/trie).
- The words.txt dataset in `bench/words.txt` is from [this repository](https://github.com/dwyl/english-words).
