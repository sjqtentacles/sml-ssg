(* ssg.sml -- pure static-site-generator core.

   frontmatter + markdown + template -> HTML, with no filesystem IO. Markdown
   rendering is delegated to the vendored sml-markdown and templating to the
   vendored sml-template; this module only wires them together and parses a
   tiny `key: value` frontmatter block. Everything is pure and deterministic. *)

structure Ssg :> SSG =
struct
  fun trim s =
    Substring.string
      (Substring.dropr Char.isSpace (Substring.dropl Char.isSpace (Substring.full s)))

  (* Index of the first occurrence of `sub` in `s` at or after `from`. *)
  fun findSub (sub, s, from) =
    let
      val n = size s
      val m = size sub
      fun go i =
        if i + m > n then NONE
        else if String.substring (s, i, m) = sub then SOME i
        else go (i + 1)
    in if m = 0 then SOME from else go from end

  (* Parse a single "key: value" frontmatter line; NONE for blank/colon-less. *)
  fun parseLine line =
    let
      val (k, r) = Substring.splitl (fn c => c <> #":") (Substring.full line)
    in
      if Substring.isEmpty r then NONE
      else
        let
          val key = trim (Substring.string k)
          val value = trim (Substring.string (Substring.triml 1 r))
        in
          if key = "" then NONE else SOME (key, value)
        end
    end

  fun parsePairs block =
    List.mapPartial parseLine (String.fields (fn c => c = #"\n") block)

  fun parseFrontmatter source =
    if not (String.isPrefix "---\n" source) then
      { frontmatter = [], body = source }
    else
      let
        val rest = String.extract (source, 4, NONE)  (* drop opening "---\n" *)
      in
        (* Closing delimiter is a line "---". Handle: immediate close, a
           "\n---\n" in the middle, or a trailing "\n---" / "...---" at EOF. *)
        if String.isPrefix "---\n" rest then
          { frontmatter = [], body = String.extract (rest, 4, NONE) }
        else if rest = "---" then
          { frontmatter = [], body = "" }
        else
          (case findSub ("\n---\n", rest, 0) of
               SOME i =>
                 { frontmatter = parsePairs (String.substring (rest, 0, i))
                 , body = String.extract (rest, i + 5, NONE) }
             | NONE =>
                 if String.isSuffix "\n---" rest then
                   { frontmatter =
                       parsePairs (String.substring (rest, 0, size rest - 4))
                   , body = "" }
                 else
                   (* No closing delimiter: treat as having no frontmatter. *)
                   { frontmatter = [], body = source })
      end

  val markdownToHtml = Markdown.toHtml

  fun renderPage { template, source } =
    let
      val { frontmatter, body } = parseFrontmatter source
      val html = markdownToHtml body
      (* `content` is listed first so the rendered HTML always wins over any
         frontmatter key that happens to be named "content". *)
      val pairs = ("content", Template.Str html)
                  :: List.map (fn (k, v) => (k, Template.Str v)) frontmatter
    in
      Template.renderString template (Template.Map pairs)
    end

  fun renderSite { template } pages =
    List.map (fn (name, source) => (name, renderPage { template = template, source = source }))
             pages
end
