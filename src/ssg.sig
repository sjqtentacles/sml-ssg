(* ssg.sig

   A static-site-generator CORE: a pure, deterministic transformation from
   (frontmatter + Markdown + template) to HTML. There is NO filesystem IO in
   this library; it is a pure string -> string pipeline so it behaves
   identically under MLton and Poly/ML.

   It does not implement its own Markdown parser or templating engine: Markdown
   is rendered by the vendored sml-markdown, and templates by the vendored
   sml-template (a Mustache-style engine).

   Frontmatter is a leading `---\n ... \n---\n` block of simple `key: value`
   lines (no YAML); the rendered Markdown HTML is exposed to the template under
   the `content` key. Because sml-template HTML-escapes `{{...}}`, templates
   must use a triple-brace `{{{content}}}` to inject the rendered HTML raw. *)

signature SSG =
sig
  (* Parse a leading frontmatter block. If the source does not begin with a
     well-formed `---` block, `frontmatter` is empty and `body` is the whole
     source unchanged. *)
  val parseFrontmatter : string -> { frontmatter : (string * string) list
                                   , body : string }

  (* Render Markdown source to an HTML string (thin wrapper over sml-markdown). *)
  val markdownToHtml : string -> string

  (* Full single-page pipeline: parse frontmatter from `source`, render its
     Markdown body to HTML, build a template context from the frontmatter pairs
     plus a `content` key (the rendered HTML, as a string), and render
     `template` against it with sml-template. Use `{{{content}}}` in templates. *)
  val renderPage : { template : string, source : string } -> string

  (* Render a list of (name, source) pages through one shared template,
     preserving input order, returning (name, html) pairs. Pure/deterministic. *)
  val renderSite : { template : string }
                   -> (string * string) list -> (string * string) list
end
