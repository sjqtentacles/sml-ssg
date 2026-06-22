(* test.sml -- canonical SSG pipeline vectors.

   Expected HTML strings are pinned exactly (the markdown/template output is
   stable), so the suite verifies byte-identical behaviour across MLton and
   Poly/ML. *)

structure Tests =
struct
  open Harness

  fun fmStr pairs = String.concatWith ";" (List.map (fn (k, v) => k ^ "=" ^ v) pairs)

  (* The headline canonical vector from the spec. *)
  val page =
    "---\ntitle: Hello\nauthor: me\n---\n# Heading\n\nSome **bold** text and a [link](https://x)."
  val pageBody = "# Heading\n\nSome **bold** text and a [link](https://x)."
  val pageBodyHtml =
    "<h1>Heading</h1><p>Some <strong>bold</strong> text and a <a href=\"https://x\">link</a>.</p>"
  val docTemplate = "<!doctype html><title>{{title}}</title><body>{{{content}}}</body>"
  val pageHtml =
    "<!doctype html><title>Hello</title><body>" ^ pageBodyHtml ^ "</body>"

  fun suiteFrontmatter () =
    let
      val { frontmatter, body } = Ssg.parseFrontmatter page
      val none = Ssg.parseFrontmatter "no frontmatter here\njust text"
      val empty = Ssg.parseFrontmatter "---\n---\nbody text"
      val colon = Ssg.parseFrontmatter "---\nurl: http://x:8080/p\n---\nx"
    in
      section "frontmatter parsing";
      checkString "pairs parsed in order" ("title=Hello;author=me", fmStr frontmatter);
      checkString "body separated" (pageBody, body);
      checkString "no frontmatter -> empty pairs" ("", fmStr (#frontmatter none));
      checkString "no frontmatter -> body unchanged"
        ("no frontmatter here\njust text", #body none);
      checkString "empty block -> empty pairs" ("", fmStr (#frontmatter empty));
      checkString "empty block -> body" ("body text", #body empty);
      checkString "value may contain colons" ("url=http://x:8080/p", fmStr (#frontmatter colon))
    end

  fun suiteMarkdown () =
    ( section "markdown rendering"
    ; checkString "markdownToHtml of body" (pageBodyHtml, Ssg.markdownToHtml pageBody)
    ; checkString "markdownToHtml heading" ("<h1>A</h1>", Ssg.markdownToHtml "# A")
    ; checkString "markdownToHtml paragraph" ("<p>plain</p>", Ssg.markdownToHtml "plain") )

  fun suiteRenderPage () =
    ( section "renderPage pipeline"
    ; checkString "full page rendered"
        (pageHtml, Ssg.renderPage { template = docTemplate, source = page })
    ; checkString "triple-brace injects content raw"
        ("A &amp; B|<p>plain</p>",
         Ssg.renderPage { template = "{{title}}|{{{content}}}"
                        , source = "---\ntitle: A & B\n---\nplain" })
    ; checkString "no frontmatter still renders body"
        ("<p>no fm here</p>",
         Ssg.renderPage { template = "{{{content}}}", source = "no fm here" }) )

  fun suiteRenderSite () =
    let
      val tpl = "<t>{{title}}</t>{{{content}}}"
      val pages =
        [ ("a", "---\ntitle: A\n---\n# A")
        , ("b", "---\ntitle: B\n---\n# B")
        , ("c", "---\ntitle: C\n---\n# C") ]
      val out = Ssg.renderSite { template = tpl } pages
    in
      section "renderSite";
      checkStringList "names preserved in order" (["a","b","c"], List.map #1 out);
      checkStringList "html bodies in order"
        ([ "<t>A</t><h1>A</h1>", "<t>B</t><h1>B</h1>", "<t>C</t><h1>C</h1>" ],
         List.map #2 out);
      checkInt "site length matches input" (3, List.length out)
    end

  fun run () =
    ( reset ()
    ; suiteFrontmatter ()
    ; suiteMarkdown ()
    ; suiteRenderPage ()
    ; suiteRenderSite ()
    ; Harness.run () )
end
