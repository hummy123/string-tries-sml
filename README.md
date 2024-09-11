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

# Benchmarks

There are a few benchmarks in the `bench` folder, comparing three operations (insertion, lookup and retrieval of keys matching a prefix).

The two data structures compared include: 

- An implementation of 1-2 Brother Trees described by Ralf Hinze
- The compressed string tries implemented in this repository, not based on an existing paper

## Insertion

- `bench/insert-string-set`
  - 247.5 milliseconds
- `bench/insert-bro-tree`
  - 183.9 milliseconds

The `insertion` benchmarks inserting every word from `bench/words.sml` into the respective data structure, in order.

StringSet is 1.3x slower than BroTree here.

## Exists

- `bench/build-exists-string-set`
  - 48 milliseconds
- `bench/build-exists-bro-tree`
  - 16 milliseconds

These benchmarks involve: 

- Inserting every word from `bench/words.sml` to build a data structure with keys to look for
- Then testing to see if every key from `bench/words.sml` exists in the data structure

The reported times only measure the time taken for the second bullet point; the first bullet point was already measured in the `insertion` benchmark.

StringSet is 3x slower than BroTree here.

## Get prefix list

- `bench/build-get-prefix-string-set`
  - 310,000 nanoseconds
- `bench/build-get-prefix-bro-tree`
  - 3,477,000 nanoseconds

These benchmarks involve: 

- Inserting every word from `bench/words.sml` to build a data structure with keys to look for
- Creating a list containing every word in the data structure that starts with "a"

As with the `exists` benchmark, only the time for the second bullet point is measured.

StringSet is 11x faster than BroTree here.

This result shouldn't be a surprise. 

A binary tree needs to fold over every node in the tree, checking if the keys in node starts with the prefix. That takes O(n) time.

A trie is smarter about this. It only needs to travel to a specific prefix and get the subtrie for that prefix. Then one can fold over the subtrie rather than the whole trie, which takes much less time.

## Benchmarks conclusion

The benchmarks have a clear similarity to those in [Chris Okasaki's paper on Fast Mergeable Integer Maps](https://ia600204.us.archive.org/0/items/djoyner-papers/SHA256E-s118221--efee082ebebce89bebdbc041ab9bf8cbd2bcb91e48809a204318e1a89bf15435.pdf).

- The insertion and lookup/exists operations are both faster on balanced binary trees
- The trie-specific operation (in this repository: search by prefix, in the paper: merge tries together) is much faster for tries than for binary trees.

Like the paper says, it's probably worth using a trie only if you care about using the trie-specific operation a lot.

The description of Data.IntMap for Haskell seems to disagree with the first bullet point, stating:

> my benchmarks show that it is also (much) faster on insertions and deletions when compared to a generic size-balanced map implementation (see Data.Map). 

This statement surprises me. It's not the case that IntMap was faster for insertion and lookup in the aforementioned paper, and an IntMap implementation I coded in F# was also slower for these operations. 

I would be interested in whether it is true for Haskell that these operations were faster. Lazy evaluation might help somehow, or the Haskell implementation might use tricks not described in the paper.

# Credits

- The tests in `tests/string-set-tests.sml` were ported from [kpol's Trie data structure in C3](https://github.com/kpol/trie).
- The words.txt dataset in `bench/words.txt` is from [this repository](https://github.com/dwyl/english-words).
