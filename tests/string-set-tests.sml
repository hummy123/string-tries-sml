structure StringSetTests =
struct
  fun assertTrue (isTrue, msg) =
    if isTrue then ()
    else
      (print (msg ^ "\n"); raise Empty)

  fun assertFalse (isFalse, msg) = assertTrue (not isFalse, msg)

  (* below tests ported from https://github.com/kpol/trie/blob/master/src/KTrie.Tests/TrieTests.cs *)

  fun testExists () =
    let 
      val trie = StringSet.fromList ["abc", "abde"]

      val _ = assertTrue (StringSet.exists ("abc", trie), "abc should exist")
      val _ = assertTrue (StringSet.exists ("abde", trie), "abde should exist")

      val _ = assertFalse (StringSet.exists ("a", trie), "a should not exist")
      val _ = assertFalse (StringSet.exists ("ab", trie), "ab should not exist")
      val _ = assertFalse (StringSet.exists ("abcd", trie), "abcd should not exist")
      val _ = assertFalse (StringSet.exists ("abce", trie), "abce should not exist")
      val _ = assertFalse (StringSet.exists ("x", trie), "x should not exist")
    in
      print "StringSet.exists passed all tests\n"
    end

  fun testGetPrefixList () =
    let
      val trie = StringSet.fromList ["abc", "abde", "abx", "abxx"]

      val aMatches = StringSet.getPrefixList ("a", trie) = ["abc", "abde", "abx", "abxx"]
      val _ = assertTrue (aMatches, "a matches")

      val xMatches = StringSet.getPrefixList ("x", trie) = []
      val _ = assertTrue (xMatches, "x matches")

      val abMatches = StringSet.getPrefixList ("ab", trie) = ["abc", "abde", "abx", "abxx"]
      val _ = assertTrue (abMatches, "ab matches")

      val abcMatches = StringSet.getPrefixList ("abc", trie) = ["abc"]
      val _ = assertTrue (abcMatches, "abc matches")

      val abccMatches = StringSet.getPrefixList ("abcc", trie) = []
      val _ = assertTrue (abccMatches, "abcc matches")

      val abxMatches = StringSet.getPrefixList ("abx", trie) = ["abx", "abxx"]
      val _ = assertTrue (abxMatches, "abx matches")
    in
      print "StringSet.getPrefixList passed all tests\n"
    end

  fun run () =
    let
      val _ = testExists ()
      val _ = testGetPrefixList ()
    in
      ()
    end
end

val _ = StringSetTests.run ()
