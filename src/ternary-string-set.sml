(* An attempt at implementing terneary search trees, 
 * except that each node contains a full string instead of a char *)
structure TernaryStringSet =
struct
  datatype t =
    NODE of {left: t, key: string, follow: t, right: t}
  (* follow is if insString contains nodeString *)
  | FOUND_NODE of {left: t, key: string, follow: t, right: t}
  | LEAF of string
  | EMPTY

  val empty = EMPTY

  fun isEmpty t = t = EMPTY

  fun fromString str =
    if String.size str > 0 then LEAF str else EMPTY

  val nodeStringContainsInsString = ~1
  val insStringContainsNodeString = ~2

  fun getBreakPos (pos, insString, nodeString) =
    if pos = String.size insString andalso pos = String.size nodeString then
      pos
    else if pos = String.size insString then
      (* if nodeString contains insString, return ~1 *)
      nodeStringContainsInsString
    else if pos = String.size nodeString then
      (* if insString contains nodeString, return ~2 *)
      insStringContainsNodeString
    else
      let
        val insChr = String.sub (insString, pos)
        val nodeChr = String.sub (nodeString, pos)
      in
        if insChr = nodeChr then
          (* continue *)
          getBreakPos (pos + 1, insString, nodeString)
        else
          (* return break position *)
          pos
      end

  fun helpIns (pos, insString, t) =
    case t of
      NODE {left, key = nodeString, follow, right} =>
        let
          val breakPos = getBreakPos (pos, insString, nodeString)
        in
          if breakPos = String.size insString then
            (* change node tag *)
            FOUND_NODE
              {left = left, key = nodeString, follow = follow, right = right}
          else if breakPos = nodeStringContainsInsString then
            (* add new node at current position, 
             * making current node a child of the new node *)
            FOUND_NODE
              {left = EMPTY, key = insString, follow = t, right = EMPTY}
          else if breakPos = insStringContainsNodeString then
            (* follow *)
            let
              val follow = helpIns (breakPos, insString, follow)
            in
              NODE
                {left = left, key = nodeString, follow = follow, right = right}
            end
          else
            (* we have a difference: do we want to go left or right? *)
            let
              val insChr = String.sub (insString, breakPos)
              val nodeChr = String.sub (nodeString, breakPos)
            in
              if insChr < nodeChr then
                let
                  val left = helpIns (breakPos, insString, left)
                in
                  NODE
                    { left = left
                    , key = nodeString
                    , follow = follow
                    , right = right
                    }
                end
              else
                let
                  val right = helpIns (breakPos, insString, right)
                in
                  NODE
                    { left = left
                    , key = nodeString
                    , follow = follow
                    , right = right
                    }
                end
            end
        end
    | FOUND_NODE {left, key = nodeString, follow, right} =>
        let
          val breakPos = getBreakPos (pos, insString, nodeString)
        in
          if breakPos = String.size insString then
            (* return original tree as nothing to do, since key already exists *)
            t
          else if breakPos = nodeStringContainsInsString then
            (* add new node at current position, 
             * making current node a child of the new node *)
            FOUND_NODE
              {left = EMPTY, key = insString, follow = t, right = EMPTY}
          else if breakPos = insStringContainsNodeString then
            (* follow *)
            let
              val follow = helpIns (breakPos, insString, follow)
            in
              FOUND_NODE
                {left = left, key = nodeString, follow = follow, right = right}
            end
          else
            (* we have a difference: do we want to go left or right? *)
            let
              val insChr = String.sub (insString, breakPos)
              val nodeChr = String.sub (nodeString, breakPos)
            in
              if insChr < nodeChr then
                let
                  val left = helpIns (breakPos, insString, left)
                in
                  FOUND_NODE
                    { left = left
                    , key = nodeString
                    , follow = follow
                    , right = right
                    }
                end
              else
                let
                  val right = helpIns (breakPos, insString, right)
                in
                  FOUND_NODE
                    { left = left
                    , key = nodeString
                    , follow = follow
                    , right = right
                    }
                end
            end
        end
    | EMPTY => LEAF insString
    | LEAF nodeString =>
        let
          val breakPos = getBreakPos (pos, insString, nodeString)
        in
          if breakPos = String.size insString then
            (* no change as user tried to insert string that already exists *)
            t
          else if breakPos = nodeStringContainsInsString then
            (* add new node at current position, 
             * making current node a child of the new node *)
            FOUND_NODE
              {left = EMPTY, key = insString, follow = t, right = EMPTY}
          else if breakPos = insStringContainsNodeString then
            (* transform this node to a FOUND_NODE, 
             * and add a new LEAF at follow position *)
            let
              val newLeaf = LEAF insString
            in
              FOUND_NODE
                { key = nodeString
                , follow = newLeaf
                , left = EMPTY
                , right = EMPTY
                }
            end
          else
            (* we have a difference: break string and add a new NODE.
             * The way we insert has implications for searching. 
             * Search should descend down on all three children
             * if searchKey contains nodeKey.
             * *)
            let
              val breakString = String.substring (insString, 0, breakPos)
              val insChr = String.sub (insString, breakPos)
              val nodeChr = String.sub (nodeString, breakPos)
            in
              if insChr < nodeChr then
                (* insString on left *)
                NODE
                  { left = LEAF insString
                  , right = t
                  , key = breakString
                  , follow = EMPTY
                  }
              else
                (* insString on right *)
                NODE
                  { left = t
                  , right = LEAF insString
                  , key = breakString
                  , follow = EMPTY
                  }
            end
        end
end
