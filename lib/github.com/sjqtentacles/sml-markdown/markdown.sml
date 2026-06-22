(* markdown.sml -- CommonMark-subset Markdown -> Html.node tree.

   Two-phase parse: split source into logical blocks (headings, code blocks,
   blockquotes, lists, thematic breaks, paragraphs), then parse inline spans
   within leaf text (emphasis, strong, code, links, images, autolinks, hard
   breaks, backslash escapes). *)

structure Markdown :> MARKDOWN =
struct
  structure H = Html

  (* ---------- small string helpers ---------- *)

  fun isBlankLine s = CharVector.all Char.isSpace s

  fun rtrim s =
    let
      val n = size s
      fun go i = if i > 0 andalso Char.isSpace (String.sub (s, i - 1))
                 then go (i - 1) else i
    in String.substring (s, 0, go n) end

  fun ltrim s =
    let
      val n = size s
      fun go i = if i < n andalso Char.isSpace (String.sub (s, i))
                 then go (i + 1) else i
    in String.extract (s, go 0, NONE) end

  fun trim s = rtrim (ltrim s)

  (* Count leading spaces (tabs count as one for our purposes). *)
  fun leadingSpaces s =
    let
      val n = size s
      fun go (i, acc) =
        if i < n andalso String.sub (s, i) = #" " then go (i + 1, acc + 1)
        else acc
    in go (0, 0) end

  fun splitLines s = String.fields (fn c => c = #"\n") s

  fun startsWith (pre, s) =
    size pre <= size s andalso String.substring (s, 0, size pre) = pre

  (* ---------- inline parsing ---------- *)

  (* Inline parser operates on a char list; emits Html.node list. *)

  fun explode s = String.explode s

  (* Read until a delimiter char run of length `n` (matching marker), used for
     code spans. Returns (content, rest-after-closing) or NONE if unclosed. *)

  fun takeWhileEq (c, cs) =
    let
      fun go (n, []) = (n, [])
        | go (n, x :: xs) = if x = c then go (n + 1, xs) else (n, x :: xs)
    in go (0, cs) end

  (* Find a closing backtick run of exactly length n. Returns
     (collected chars, rest) where rest is after the closing run. *)
  fun findCodeClose (n, cs) =
    let
      fun go (acc, []) = NONE
        | go (acc, x :: xs) =
            if x = #"`" then
              let val (run, rest) = takeWhileEq (#"`", x :: xs)
              in if run = n then SOME (rev acc, rest)
                 else go (List.revAppend (List.tabulate (run, fn _ => #"`"), acc), rest)
              end
            else go (x :: acc, xs)
    in go ([], cs) end

  (* Normalize code span content: trim a single leading/trailing space if the
     content is not all spaces (CommonMark rule, simplified). *)
  fun codeSpanText cs =
    let
      val s = String.implode cs
      val s2 =
        if size s >= 2
           andalso String.sub (s, 0) = #" "
           andalso String.sub (s, size s - 1) = #" "
           andalso not (CharVector.all (fn c => c = #" ") s)
        then String.substring (s, 1, size s - 2)
        else s
    in s2 end

  (* Scan a bracketed run for links/images: given chars after '[', return
     (label-chars, url, rest) on success. Expects "label](url)". *)
  fun scanLink cs =
    let
      fun label (acc, []) = NONE
        | label (acc, #"]" :: #"(" :: xs) = url (rev acc, [], xs)
        | label (acc, #"\\" :: x :: xs) = label (x :: acc, xs)
        | label (acc, x :: xs) = label (x :: acc, xs)
      and url (lab, uacc, []) = NONE
        | url (lab, uacc, #")" :: xs) = SOME (lab, String.implode (rev uacc), xs)
        | url (lab, uacc, x :: xs) = url (lab, x :: uacc, xs)
    in label ([], cs) end

  (* Read a delimiter run of char c starting at head; return run length and rest. *)
  fun delimRun (c, cs) = takeWhileEq (c, cs)

  (* Find a closing emphasis run of char c of length >= want. Returns
     (inner chars, rest) consuming exactly `want` closing delimiters. *)
  fun findEmphClose (c, want, cs) =
    let
      fun go (acc, []) = NONE
        | go (acc, x :: xs) =
            if x = c then
              let val (run, rest) = takeWhileEq (c, x :: xs)
              in if run >= want
                 then
                   (* consume `want` of the run; push back leftover *)
                   let val leftover = run - want
                       val rest' = List.tabulate (leftover, fn _ => c) @ rest
                   in SOME (rev acc, rest') end
                 else go (List.revAppend (List.tabulate (run, fn _ => c), acc), rest)
              end
            else if x = #"\\" then
              (case xs of
                   y :: ys => go (y :: #"\\" :: acc, ys)
                 | [] => go (x :: acc, xs))
            else go (x :: acc, xs)
    in go ([], cs) end

  fun isUrlScheme cs =
    (* crude: scheme chars then ':' *)
    let
      fun go [] = false
        | go (#":" :: _) = true
        | go (x :: xs) =
            (Char.isAlphaNum x orelse x = #"+" orelse x = #"-" orelse x = #".")
            andalso go xs
    in case cs of (x :: _) => Char.isAlpha x andalso go cs | [] => false end

  (* Parse inline content (char list) into Html.node list. *)
  fun inline cs =
    let
      (* Accumulate plain text chars, flushing into Text nodes. *)
      fun flush (textAcc, nodes) =
        if null textAcc then nodes
        else H.text (String.implode (rev textAcc)) :: nodes

      fun go (textAcc, nodes, []) = rev (flush (textAcc, nodes))
        | go (textAcc, nodes, cs as c :: rest) =
            (case c of
                 #"\\" =>
                   (case rest of
                        e :: rest' =>
                          if Char.isPunct e
                          then go (e :: textAcc, nodes, rest')
                          else go (c :: textAcc, nodes, rest)
                      | [] => go (c :: textAcc, nodes, rest))
               | #"`" =>
                   let val (n, after) = takeWhileEq (#"`", cs)
                   in case findCodeClose (n, after) of
                          SOME (inner, rest') =>
                            let val node = H.el "code" [] [H.text (codeSpanText inner)]
                            in go ([], node :: flush (textAcc, nodes), rest') end
                        | NONE =>
                            (* literal backticks *)
                            go (List.revAppend (List.tabulate (n, fn _ => #"`"), textAcc),
                                nodes, after)
                   end
               | #"!" =>
                   (case rest of
                        #"[" :: rest' =>
                          (case scanLink rest' of
                               SOME (lab, url, rest'') =>
                                 let val alt = String.implode lab
                                     val node = H.void "img" [("src", url), ("alt", alt)]
                                 in go ([], node :: flush (textAcc, nodes), rest'') end
                             | NONE => go (c :: textAcc, nodes, rest))
                      | _ => go (c :: textAcc, nodes, rest))
               | #"[" =>
                   (case scanLink rest of
                        SOME (lab, url, rest') =>
                          let val inner = inline lab
                              val node = H.el "a" [("href", url)] inner
                          in go ([], node :: flush (textAcc, nodes), rest') end
                      | NONE => go (c :: textAcc, nodes, rest))
               | #"<" =>
                   (* autolink <scheme:...> *)
                   let
                     fun grab (acc, #">" :: xs) = SOME (rev acc, xs)
                       | grab (acc, x :: xs) =
                           if x = #" " orelse x = #"<" then NONE
                           else grab (x :: acc, xs)
                       | grab (_, []) = NONE
                   in case grab ([], rest) of
                          SOME (inner, rest') =>
                            if isUrlScheme inner
                            then
                              let val url = String.implode inner
                                  val node = H.el "a" [("href", url)] [H.text url]
                              in go ([], node :: flush (textAcc, nodes), rest') end
                            else go (c :: textAcc, nodes, rest)
                        | NONE => go (c :: textAcc, nodes, rest)
                   end
               | #"*" => emph (#"*", textAcc, nodes, cs)
               | #"_" => emph (#"_", textAcc, nodes, cs)
               | _ => go (c :: textAcc, nodes, rest))

      and emph (ch, textAcc, nodes, cs) =
        let
          val (n, after) = takeWhileEq (ch, cs)
        in
          if n >= 2 then
            (case findEmphClose (ch, 2, after) of
                 SOME (inner, rest') =>
                   let val node = H.el "strong" [] (inline inner)
                   in go ([], node :: flush (textAcc, nodes), rest') end
               | NONE =>
                   go (List.revAppend (List.tabulate (n, fn _ => ch), textAcc), nodes, after))
          else
            (case findEmphClose (ch, 1, after) of
                 SOME (inner, rest') =>
                   let val node = H.el "em" [] (inline inner)
                   in go ([], node :: flush (textAcc, nodes), rest') end
               | NONE =>
                   go (ch :: textAcc, nodes, after))
        end
    in
      go ([], [], cs)
    end

  (* Parse inline for one line of text, handling hard line breaks across the
     joined block text. We handle hard breaks at the block level instead. *)
  fun inlineString s = inline (explode s)

  fun paragraphInline lines =
    let
      fun endsHard s =
        size s >= 2
        andalso String.sub (s, size s - 1) = #" "
        andalso String.sub (s, size s - 2) = #" "
      fun build [] = []
        | build [ln] = inlineString (trim ln)
        | build (ln :: rest) =
            let
              val hard = endsHard ln
              val sep = if hard then H.raw "<br>" else H.text " "
            in inlineString (trim ln) @ (sep :: build rest) end
    in build lines end

  fun isThematicBreak s =
    let
      val t = trim s
      fun allOf c = t <> "" andalso size t >= 3
                    andalso CharVector.all (fn x => x = c) t
    in allOf #"-" orelse allOf #"*" orelse allOf #"_" end

  fun atxHeading s =
    let
      val t = ltrim s
      val n = size t
      fun hashes i = if i < n andalso String.sub (t, i) = #"#" then hashes (i + 1) else i
      val h = hashes 0
    in
      if h >= 1 andalso h <= 6
         andalso (h = n orelse String.sub (t, h) = #" ")
      then
        let
          val body = if h < n then String.extract (t, h, NONE) else ""
          val body' = rtrim (trim body)
          val body'' =
            let
              val m = size body'
              fun trail i = if i > 0 andalso String.sub (body', i - 1) = #"#"
                            then trail (i - 1) else i
              val k = trail m
            in if k < m andalso (k = 0 orelse String.sub (body', k - 1) = #" ")
               then rtrim (String.substring (body', 0, k))
               else body'
            end
        in SOME (h, body'') end
      else NONE
    end

  fun unorderedItem s =
    let val t = ltrim s
    in if size t >= 2
          andalso (String.sub (t, 0) = #"-" orelse String.sub (t, 0) = #"*"
                   orelse String.sub (t, 0) = #"+")
          andalso String.sub (t, 1) = #" "
       then SOME (String.extract (t, 2, NONE))
       else NONE
    end

  fun orderedItem s =
    let
      val t = ltrim s
      val n = size t
      fun digits i = if i < n andalso Char.isDigit (String.sub (t, i))
                     then digits (i + 1) else i
      val d = digits 0
    in
      if d >= 1 andalso d < n
         andalso (String.sub (t, d) = #"." orelse String.sub (t, d) = #")")
         andalso d + 1 < n andalso String.sub (t, d + 1) = #" "
      then SOME (String.extract (t, d + 2, NONE))
      else if d >= 1 andalso d + 1 = n
              andalso (String.sub (t, d) = #"." orelse String.sub (t, d) = #")")
      then SOME ""
      else NONE
    end

  fun isIndentedCode s = leadingSpaces s >= 4 andalso not (isBlankLine s)

  fun fenceInfo s =
    let val t = ltrim s
    in if startsWith ("```", t)
       then SOME (trim (String.extract (t, 3, NONE)))
       else NONE
    end

  fun isFence s = case fenceInfo s of SOME _ => true | NONE => false

  fun parseBlocks lines =
    let
      fun loop ([], acc) = rev acc
        | loop (lines as line :: rest, acc) =
            if isBlankLine line then loop (rest, acc)
            else if isFence line then fenced (lines, acc)
            else (case atxHeading line of
                      SOME (lvl, body) =>
                        let val tag = "h" ^ Int.toString lvl
                            val node = H.el tag [] (inlineString body)
                        in loop (rest, node :: acc) end
                    | NONE =>
                  if isThematicBreak line then loop (rest, H.void "hr" [] :: acc)
                  else if Option.isSome (unorderedItem line) then bulletList (lines, acc)
                  else if Option.isSome (orderedItem line) then numberList (lines, acc)
                  else if startsWith (">", ltrim line) then blockquote (lines, acc)
                  else if isIndentedCode line then indentedCode (lines, acc)
                  else paragraph (lines, acc))

      and fenced (line :: rest, acc) =
            let
              val info = valOf (fenceInfo line)
              fun gather (acc2, []) = (rev acc2, [])
                | gather (acc2, l :: ls) =
                    if isFence l then (rev acc2, ls)
                    else gather (l :: acc2, ls)
              val (body, after) = gather ([], rest)
              val content = String.concatWith "\n" body ^ (if null body then "" else "\n")
              val codeAttrs = if info = "" then []
                              else [("class", "language-" ^ info)]
              val code = H.el "code" codeAttrs [H.text content]
              val node = H.el "pre" [] [code]
            in loop (after, node :: acc) end
        | fenced ([], acc) = rev acc

      and indentedCode (lines, acc) =
            let
              fun gather (acc2, []) = (rev acc2, [])
                | gather (acc2, l :: ls) =
                    if isIndentedCode l
                    then gather (String.extract (l, 4, NONE) :: acc2, ls)
                    else (rev acc2, l :: ls)
              val (body, after) = gather ([], lines)
              val content = String.concatWith "\n" body ^ "\n"
              val node = H.el "pre" [] [H.el "code" [] [H.text content]]
            in loop (after, node :: acc) end

      and blockquote (lines, acc) =
            let
              fun strip l =
                let val t = ltrim l
                in if startsWith ("> ", t) then String.extract (t, 2, NONE)
                   else if startsWith (">", t) then String.extract (t, 1, NONE)
                   else t
                end
              fun gather (acc2, []) = (rev acc2, [])
                | gather (acc2, l :: ls) =
                    if startsWith (">", ltrim l)
                    then gather (strip l :: acc2, ls)
                    else if isBlankLine l then (rev acc2, ls)
                    else gather (strip l :: acc2, ls)
              val (inner, after) = gather ([], lines)
              val node = H.el "blockquote" [] (parseBlocks inner)
            in loop (after, node :: acc) end

      and bulletList (lines, acc) = listBlock (unorderedItem, "ul", lines, acc)
      and numberList (lines, acc) = listBlock (orderedItem, "ol", lines, acc)

      and listBlock (matcher, tag, lines, acc) =
            let
              fun gather (items, []) = (rev items, [])
                | gather (items, l :: ls) =
                    if isBlankLine l then (rev items, ls)
                    else
                      (case matcher l of
                           SOME content =>
                             let
                               fun sub (sacc, []) = (rev sacc, [])
                                 | sub (sacc, x :: xs) =
                                     if not (isBlankLine x)
                                        andalso leadingSpaces x >= 2
                                        andalso (Option.isSome (unorderedItem x)
                                                 orelse Option.isSome (orderedItem x))
                                     then sub (x :: sacc, xs)
                                     else (rev sacc, x :: xs)
                               val (nested, rest2) = sub ([], ls)
                             in gather ((content, nested) :: items, rest2) end
                         | NONE => (rev items, l :: ls))
              val (items, after) = gather ([], lines)
              fun itemNode (content, nested) =
                let
                  val inlineNodes = inlineString content
                  val nestedNodes =
                    if null nested then []
                    else
                      let
                        val dedented = List.map (fn x =>
                          let val k = Int.min (leadingSpaces x, 2)
                          in String.extract (x, k, NONE) end) nested
                      in parseBlocks dedented end
                in H.el "li" [] (inlineNodes @ nestedNodes) end
              val node = H.el tag [] (List.map itemNode items)
            in loop (after, node :: acc) end

      and paragraph (lines, acc) =
            let
              fun gather (acc2, []) = (rev acc2, [])
                | gather (acc2, l :: ls) =
                    if isBlankLine l orelse isFence l
                       orelse Option.isSome (atxHeading l)
                       orelse isThematicBreak l
                       orelse Option.isSome (unorderedItem l)
                       orelse Option.isSome (orderedItem l)
                       orelse startsWith (">", ltrim l)
                    then (rev acc2, l :: ls)
                    else gather (l :: acc2, ls)
              val (plines, after) = gather ([], lines)
              val node = H.el "p" [] (paragraphInline plines)
            in loop (after, node :: acc) end
    in
      loop (lines, [])
    end

  fun parse src = parseBlocks (splitLines src)

  fun toHtml src = H.renderList (parse src)
end
