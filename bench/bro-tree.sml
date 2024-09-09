structure BroTree =
struct
  (* implementation of 1-2 brother tree ported from:
   * https://www.cl.cam.ac.uk/research/hvg/Isabelle/dist/library/HOL/HOL-Data_Structures/document.pdf
   * *)
  datatype bro =
    N0
  | N1 of bro
  | N2 of bro * string * bro
  | L2 of string
  | N3 of bro * string * bro * string * bro

  val empty = N0

  fun n1 bro =
    case bro of
      L2 str => N2 (N0, str, N0)
    | N3 (t1, a1, t2, a2, t3) => N2 (N2 (t1, a1, t2), a2, N1 t3)
    | t => N1 t

  fun n2Left (t1, str, t2) =
    case (t1, str, t2) of
      (L2 a1, a2, t) => N3 (N0, a2, N0, a2, t)
    | (N3 (t1, a1, t2, a2, t3), a3, N1 t4) =>
        N2 (N2 (t1, a1, t2), a2, N2 (t2, a3, t4))
    | (N3 (t1, a1, t2, a2, t3), a3, t4) =>
        N3 (N2 (t1, a1, t2), a2, N1 t3, a3, t4)
    | (t1, a1, t2) => N2 (t1, a1, t2)

  fun n2Right (t1, str, t2) =
    case (t1, str, t2) of
      (t1, a1, L2 a2) => N3 (t1, a2, N0, a2, N0)
    | (N1 t1, a1, N3 (t2, a2, t3, a3, t4)) =>
        N2 (N2 (t1, a1, t2), a2, N2 (t3, a3, t4))
    | (t1, a1, N3 (t2, a2, t3, a3, t4)) =>
        N3 (t1, a2, N1 t2, a2, N2 (t3, a3, t4))
    | (t1, a1, t2) => N2 (t1, a1, t2)

  fun ins (str, tree) =
    case tree of
      N0 => L2 str
    | N1 t => n1 (ins (str, t))
    | N2 (l, a, r) =>
        if str < a then n2Left (ins (str, l), a, r)
        else if str > a then n2Right (l, a, ins (str, r))
        else N2 (l, a, r)
    | _ => raise Match (*impossible case*)

  fun insRoot tree =
    case tree of
      L2 str => N2 (N0, str, N0)
    | N3 (t1, a1, t2, a2, t3) => N2 (N2 (t1, a2, t2), a2, N1 t3)
    | tree => tree

  fun insert (str, tree) =
    insRoot (ins (str, tree))

  fun exists (str, tree) =
    case tree of
      N0 => false
    | N1 t => exists (str, t)
    | N2 (l, k, r) =>
        if str < k then exists (str, l)
        else if str > k then exists (str, r)
        else true
    | _ => raise Match
end
