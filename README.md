# string-trie

This repository implements a set over strings in Standard ML using a trie/prefix tree.

The signature provided is:

```
signature STRING_SET =
sig
  (* the type of tries *)
  type t

  (* the empty trie *)
  val empty: t

  (* returns true if the trie is empty *)
  val isEmpty: t -> bool

  (* creates a trie containing just a string *)
  val fromString: string -> t

  (* returns true if the key was inserted into the trie *)
  val exists: string * t -> bool

  (* inserts a new string into the trie, returning a new trie *)
  val insert: string * t -> t

  (* removes the key from the trie, returning a new trie *)
  val remove: string * t -> t

  (* returns a list of all keys matching the specified prefix *)
  val getPrefixList: string * t -> string list

  (* returns a list containing all keys in the trie *)
  val toList: t -> string list

  (* returns a trie containing all keys in the string list *)
  val fromList: string list -> t
end
```

The reason for implementing a new trie specialised to strings rather than using Chris Okasaki's IntMap data structure is to enable prefix searching, where it is possible to get a list of all keys matching a certain prefix.

# To-do

- [ ] Add `foldl`, `foldr`, `foldlWithPrefix`, `foldrWithPrefix` functions to string set
- [ ] Benchmarks (possibly comparing to a set of strings in a balanced binary tree)
- [ ] Use unrolled linked list with zipper in trie, to limit size of vector as allocating large vectors repeatedly is expensive
- [ ] Implement StringMap, containing both keys and values

# Credits

The tests in `tests/string-set-tests.sml` were ported from [kpol's Trie data structure in C3](https://github.com/kpol/trie), although this is not true for the files in the `src/` directory.
