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

  fun remove1 () =
    let
      val trie = StringSet.fromList ["a", "ab", "abc"]

      val _ = assertTrue (StringSet.exists ("a", trie), "a exists before remove1")

      val trie = StringSet.remove ("a", trie)
      val _ = assertFalse (StringSet.exists ("a", trie), "a does not exist after remove1")

      val _ = assertTrue (StringSet.exists ("ab", trie), "ab still exists after remove1")
      val _ = assertTrue (StringSet.exists ("abc", trie), "abc still exists after remove1")
    in
      print "StringSet.remove: passed remove1\n"
    end

  fun remove2 () =
    let
      val trie = StringSet.fromList ["a", "ab", "abc", "abd"]

      val _ = assertTrue (StringSet.exists ("ab", trie), "ab exists before remove2")
      val trie = StringSet.remove ("ab", trie)
      val _ = assertFalse (StringSet.exists ("ab", trie), "ab no longer exists after remove2")

      val _ = assertTrue (StringSet.exists ("a", trie), "remove2 contains a")
      val _ = assertTrue (StringSet.exists ("abc", trie), "remove2 contains abc")
      val _ = assertTrue (StringSet.exists ("abd", trie), "remove2 contains abd")
    in
      print "StringSet.remove: passed remove2\n"
    end

  fun remove3 () =
    let
      val trie = StringSet.fromList ["abc"]

      val _ = assertTrue (StringSet.exists ("abc", trie), "abc exists before remove3")
      val trie = StringSet.remove ("abc", trie)
      val _ = assertFalse (StringSet.exists ("abc", trie), "abc no longer exists after remove3")

      val _ = assertTrue (StringSet.isEmpty trie, "trie is empty after remove3")

      val trie = StringSet.insert ("abc", trie)
      val _ = assertTrue (StringSet.exists ("abc", trie), "abc exists after insertion in remove3")
    in
      print "StringSet.remove: passed remove3\n"
    end

  fun remove4 () =
    let
      val trie = StringSet.fromList ["abc", "abcd"]
      val _ = assertTrue (StringSet.exists ("abc", trie), "abc exists before remove4")

      val trie = StringSet.remove ("abc", trie)
      val _ = assertTrue (StringSet.exists ("abcd", trie), "abcd still exists after removing abc in remove4")
      val _ = assertFalse (StringSet.exists ("abc", trie), "abc doesn't exist after remove in remove4")
    in
      print "StringSet.remove: passed remove4\n"
    end

  fun remove5 () =
    let
      val trie = StringSet.fromList ["abc", "ab", "ade", "abcde", "x"]

      val trie2 = StringSet.remove ("xy", trie)
      val _ = assertTrue (trie = trie2, "removing key (xy) which doesn't exist in trie returns same trie")

      val trie3 = StringSet.remove ("abcd", trie)
      val _ = assertTrue (trie = trie3, "removing key (abcd) which doesn't exist in trie returns same trie")

      val _ = assertTrue (StringSet.exists ("abcde", trie), "abcde exists before remove in remove5")
      val trie = StringSet.remove ("abcde", trie)
      val _ = assertFalse (StringSet.exists ("abcde", trie), "abcde does not exist after remove in remove5")

      val _ = assertTrue (StringSet.exists ("x", trie), "x exists before remove in remove5")
      val trie = StringSet.remove ("x", trie)
      val _ = assertFalse (StringSet.exists ("x", trie), "x does not exist after remove in remove5")

      val _ = assertTrue (StringSet.exists ("abc", trie), "abc exists before remove in remove5")
      val trie = StringSet.remove ("abc", trie)
      val _ = assertFalse (StringSet.exists ("abc", trie), "abc does not exist after remove in remove5")

      val _ = assertTrue (StringSet.exists ("ab", trie), "trie still contains ab after removals in remove5")
      val _ = assertTrue (StringSet.exists ("ade", trie), "trie still contains ade after removals in remove5")
    in
      print "StringSet.remove: passed remove5\n"
    end

  fun insert1 () = 
    let
      val trie = StringSet.empty
      val _ = assertFalse (StringSet.exists ("abc", trie), "abc does not exist before it is added to trie")

      val trie = StringSet.insert ("abc", trie)
      val _ = assertTrue (StringSet.exists ("abc", trie), "abc exists after being addedd to trie")
    in
      print "StringSet.insert: passed insert1\n"
    end

  fun insert2 () =
    let
      val trie = StringSet.fromList ["abc"]
      val trie2 = StringSet.insert ("abc", trie)

      val _ = assertTrue (trie = trie2, "trie handles duplicate insertion by returning same trie")
    in
      print "StringSet.insert: passed insert2\n"
    end

    fun insert3 () =
      let
        val trie = StringSet.empty
        val _ = assertFalse (StringSet.exists ("abcd", trie), "empty trie does not contain abcd")

        val trie = StringSet.insert ("abcd", trie)
        val _ = assertTrue (StringSet.exists ("abcd", trie), "abcd exists after inserting it into empty trie")
        val _ = assertFalse (StringSet.exists ("abc", trie), "abc does not exist before inserting it")

        val trie = StringSet.insert ("abc", trie)
        val _ = assertTrue (StringSet.exists ("abc", trie), "abc exists after inserting it into trie")

        val trie = StringSet.remove ("abc", trie)
        val _ = assertFalse (StringSet.exists ("abc", trie), "abc does not exist after removing it from trie")
      in
        print "StringSet.insert: passed insert3\n"
      end

  fun getPrefixList () =
    let
      val trie = StringSet.fromList ["z", "xc", "x", "zzz", "abc", "abcde"]
      val lst = StringSet.getPrefixList ("ab", trie)

      val _ = assertTrue (lst = ["abc", "abcde"], "prefix list contains abc and abcde when prefix is 'ab'")
    in
      print "StringSet.getPrefixList: passed\n"
    end

  fun run () =
    let
      val _ = testExists ()
      val _ = testGetPrefixList ()

      val _ = remove1 ()
      val _ = remove2 ()
      val _ = remove3 ()
      val _ = remove4 ()
      val _ = remove5 ()

      val _ = insert1 ()
      val _ = insert2 ()
      val _ = insert3 ()

      val _ = getPrefixList ()
    in
      ()
    end
end

val _ = StringSetTests.run ()
