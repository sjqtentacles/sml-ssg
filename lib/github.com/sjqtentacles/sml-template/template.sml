(* template.sml

   A logic-light Mustache/Handlebars-style templating engine in pure Standard
   ML. Interpolated values are HTML-escaped by default via sml-html's
   `Escape.text`; triple braces emit raw output.

   The parser is a small hand-written scanner that splits the source into a
   list of nodes, recursively nesting `{{#..}}`/`{{^..}}` ... `{{/..}}`
   sections. The renderer walks the node list against a stack of scopes. *)

structure Template :> TEMPLATE =
struct
  datatype value =
      Str  of string
    | Bool of bool
    | Num  of int
    | List of value list
    | Map  of (string * value) list
    | Null

  datatype node =
      Text of string
    | Var of string        (* {{ key }}  -- HTML escaped *)
    | Raw of string        (* {{{ key }}} -- unescaped *)
    | Section of string * node list      (* {{#key}}..{{/key}} *)
    | Inverted of string * node list     (* {{^key}}..{{/key}} *)

  type template = node list

  exception Parse of string

  (* ---- Tokens (flat, before nesting) ---- *)
  datatype token =
      TText of string
    | TVar of string
    | TRaw of string
    | TComment
    | TOpen of string      (* #key *)
    | TInverted of string  (* ^key *)
    | TClose of string     (* /key *)

  fun trim s =
    let
      val n = size s
      fun isWs c = c = #" " orelse c = #"\t" orelse c = #"\n" orelse c = #"\r"
      fun fwd i = if i < n andalso isWs (String.sub (s, i)) then fwd (i + 1) else i
      fun bwd i = if i >= 0 andalso isWs (String.sub (s, i)) then bwd (i - 1) else i
      val a = fwd 0
      val b = bwd (n - 1)
    in
      if a > b then "" else String.substring (s, a, b - a + 1)
    end

  (* Find the next occurrence of `pat` in `s` starting at index `from`.
     Returns SOME index or NONE. *)
  fun findFrom (s, pat, from) =
    let
      val n = size s
      val m = size pat
      fun matchAt i =
        let
          fun go j = j >= m orelse (String.sub (s, i + j) = String.sub (pat, j)
                                    andalso go (j + 1))
        in go 0 end
      fun loop i =
        if i + m > n then NONE
        else if matchAt i then SOME i
        else loop (i + 1)
    in
      if m = 0 then SOME from else loop from
    end

  (* Tokenize the raw source into a flat token list. *)
  fun tokenize (src : string) : token list =
    let
      val n = size src
      fun classifyTag (body : string) : token =
        (* body is the text between {{ and }} (triple already handled) *)
        if size body = 0 then TComment  (* {{}} -> treat as no-op comment *)
        else
          (case String.sub (body, 0) of
               #"!" => TComment
             | #"#" => TOpen (trim (String.extract (body, 1, NONE)))
             | #"^" => TInverted (trim (String.extract (body, 1, NONE)))
             | #"/" => TClose (trim (String.extract (body, 1, NONE)))
             | #"&" => TRaw (trim (String.extract (body, 1, NONE)))
             | _    => TVar (trim body))
      (* i = scan position, acc = reversed tokens *)
      fun loop (i, acc) =
        if i >= n then List.rev acc
        else
          (case findFrom (src, "{{", i) of
               NONE =>
                 let val txt = String.extract (src, i, NONE)
                 in List.rev (TText txt :: acc) end
             | SOME open2 =>
                 let
                   val pre = if open2 > i
                             then [TText (String.substring (src, i, open2 - i))]
                             else []
                   (* triple brace? {{{ ... }}} *)
                   val isTriple =
                     open2 + 2 < n andalso String.sub (src, open2 + 2) = #"{"
                 in
                   if isTriple then
                     (case findFrom (src, "}}}", open2 + 3) of
                          NONE => raise Parse "unclosed {{{ tag"
                        | SOME close3 =>
                            let
                              val body = String.substring
                                           (src, open2 + 3, close3 - (open2 + 3))
                              val tok = TRaw (trim body)
                            in
                              loop (close3 + 3, tok :: (List.revAppend (pre, acc)))
                            end)
                   else
                     (case findFrom (src, "}}", open2 + 2) of
                          NONE => raise Parse "unclosed {{ tag"
                        | SOME close2 =>
                            let
                              val body = String.substring
                                           (src, open2 + 2, close2 - (open2 + 2))
                              val tok = classifyTag body
                            in
                              loop (close2 + 2, tok :: (List.revAppend (pre, acc)))
                            end)
                 end)
    in
      loop (0, [])
    end

  (* Build the nested node tree from the flat token list. *)
  fun parseNodes (toks : token list) : node list =
    let
      (* Returns (nodes, remaining-tokens). `stop` = SOME key when we are
         inside a section and should stop at {{/key}}. *)
      fun go (toks, stop) =
        case toks of
            [] =>
              (case stop of
                   NONE => ([], [])
                 | SOME k => raise Parse ("unclosed section: " ^ k))
          | t :: rest =>
              (case t of
                   TText s => let val (ns, r) = go (rest, stop)
                              in (Text s :: ns, r) end
                 | TVar k => let val (ns, r) = go (rest, stop)
                             in (Var k :: ns, r) end
                 | TRaw k => let val (ns, r) = go (rest, stop)
                             in (Raw k :: ns, r) end
                 | TComment => go (rest, stop)
                 | TOpen k =>
                     let
                       val (inner, r1) = go (rest, SOME k)
                       val (ns, r2) = go (r1, stop)
                     in
                       (Section (k, inner) :: ns, r2)
                     end
                 | TInverted k =>
                     let
                       val (inner, r1) = go (rest, SOME k)
                       val (ns, r2) = go (r1, stop)
                     in
                       (Inverted (k, inner) :: ns, r2)
                     end
                 | TClose k =>
                     (case stop of
                          SOME want =>
                            if want = k then ([], rest)
                            else raise Parse
                              ("mismatched closing tag: expected /" ^ want
                               ^ " but found /" ^ k)
                        | NONE => raise Parse ("unexpected closing tag: /" ^ k)))
      val (nodes, leftover) = go (toks, NONE)
    in
      case leftover of
          [] => nodes
        | _ => raise Parse "trailing tokens after parse"
    end

  fun parse (src : string) : template = parseNodes (tokenize src)

  (* ---- Rendering ---- *)

  (* A scope stack: innermost first. Lookups try each Map in turn. *)
  type scope = value list

  fun isTruthy v =
    case v of
        Bool b => b
      | List xs => not (List.null xs)
      | Null => false
      | Str "" => false
      | Str _ => true
      | Num _ => true
      | Map _ => true

  fun lookupMap (pairs, key) =
    case List.find (fn (k, _) => k = key) pairs of
        SOME (_, v) => SOME v
      | NONE => NONE

  (* Resolve a single (non-dotted) key against one scope frame. *)
  fun lookupFrame (v, key) =
    case v of
        Map pairs => lookupMap (pairs, key)
      | _ => NONE

  (* Walk the scope stack for a single key. *)
  fun lookupKey ([], _) = NONE
    | lookupKey (frame :: rest, key) =
        (case lookupFrame (frame, key) of
             SOME v => SOME v
           | NONE => lookupKey (rest, key))

  fun splitDots key =
    String.tokens (fn c => c = #".") key

  (* Resolve a possibly-dotted key against the scope stack. The first segment
     is looked up on the stack; subsequent segments descend into the resolved
     value's map. "." refers to the current (innermost) scope value. *)
  fun resolve (scopes : scope, key : string) : value option =
    if key = "." then
      (case scopes of [] => NONE | v :: _ => SOME v)
    else
      (case splitDots key of
           [] => NONE
         | first :: parts =>
             (case lookupKey (scopes, first) of
                  NONE => NONE
                | SOME v0 =>
                    let
                      fun descend (v, []) = SOME v
                        | descend (v, p :: ps) =
                            (case lookupFrame (v, p) of
                                 NONE => NONE
                               | SOME v' => descend (v', ps))
                    in
                      descend (v0, parts)
                    end))

  fun valueToString v =
    case v of
        Str s => s
      | Bool b => Bool.toString b
      | Num i => Int.toString i
      | Null => ""
      | List _ => ""
      | Map _ => ""

  fun renderNodes (nodes, scopes, acc) =
    List.foldl (fn (node, acc) => renderNode (node, scopes, acc)) acc nodes

  and renderNode (node, scopes, acc) =
    case node of
        Text s => acc ^ s
      | Var key =>
          (case resolve (scopes, key) of
               NONE => acc
             | SOME v => acc ^ Escape.text (valueToString v))
      | Raw key =>
          (case resolve (scopes, key) of
               NONE => acc
             | SOME v => acc ^ valueToString v)
      | Section (key, inner) =>
          (case resolve (scopes, key) of
               NONE => acc
             | SOME v =>
                 (case v of
                      List xs =>
                        List.foldl
                          (fn (item, a) =>
                             renderNodes (inner, item :: scopes, a))
                          acc xs
                    | _ =>
                        if isTruthy v
                        then renderNodes (inner, v :: scopes, acc)
                        else acc))
      | Inverted (key, inner) =>
          let
            val falsy =
              case resolve (scopes, key) of
                  NONE => true
                | SOME v => not (isTruthy v)
          in
            if falsy then renderNodes (inner, scopes, acc) else acc
          end

  fun render (tmpl : template) (ctx : value) : string =
    renderNodes (tmpl, [ctx], "")

  fun renderString s v = render (parse s) v
end
