# sml-ssg

[![CI](https://github.com/sjqtentacles/sml-ssg/actions/workflows/ci.yml/badge.svg)](https://github.com/sjqtentacles/sml-ssg/actions/workflows/ci.yml)

A static-site-generator **core** in pure Standard ML: a deterministic
transformation from **frontmatter + Markdown + template** to HTML. There is
**no filesystem IO** in the library — it is a pure `string -> string` pipeline,
so it behaves identically under **MLton** and **Poly/ML**.

It does not reinvent Markdown or templating: Markdown is rendered by the
vendored [sml-markdown](https://github.com/sjqtentacles/sml-markdown), and
templates by the vendored
[sml-template](https://github.com/sjqtentacles/sml-template) (a Mustache-style
engine). sml-ssg just parses a tiny frontmatter block and wires the two
together.

No FFI, no threads, no clock, no randomness: the same inputs always produce the
same outputs, and Markdown output ordering is stable, so generated pages are
byte-identical across compilers.

## Pipeline

1. **Frontmatter** — a leading `---\n ... \n---\n` block of simple `key: value`
   lines (not YAML). Parsed into a `(string * string) list`; everything after
   the closing `---` is the Markdown body.
2. **Markdown** — the body is rendered to an HTML string by sml-markdown.
3. **Template** — a sml-template context `Map` is built from the frontmatter
   pairs plus a `content` key holding the rendered HTML, then the template is
   rendered against it.

> **Use `{{{content}}}` (triple brace), not `{{content}}`.** sml-template
> HTML-escapes `{{...}}` interpolations; the triple-brace form emits the value
> raw, which is what you want for already-rendered HTML. The same applies to any
> frontmatter value you want to inject as raw HTML.

## API

```sml
structure Ssg : sig
  val parseFrontmatter : string -> { frontmatter : (string * string) list
                                   , body : string }
  val markdownToHtml   : string -> string
  val renderPage       : { template : string, source : string } -> string
  val renderSite       : { template : string }
                         -> (string * string) list -> (string * string) list
end
```

`renderSite` maps a list of `(name, source)` pages to `(name, html)` using one
shared template, preserving input order.

## Example

```sml
val source =
  "---\ntitle: Hello\nauthor: me\n---\n\
  \# Heading\n\nSome **bold** text and a [link](https://x)."

val template = "<!doctype html><title>{{title}}</title><body>{{{content}}}</body>"

val html = Ssg.renderPage { template = template, source = source }
(* html =
   "<!doctype html><title>Hello</title><body>\
   \<h1>Heading</h1><p>Some <strong>bold</strong> text and a \
   \<a href=\"https://x\">link</a>.</p></body>" *)
```

Running [`examples/demo.sml`](examples/demo.sml) with `make example` prints:

```
sml-ssg demo
============

parseFrontmatter of the index page:
  title = Home
  author = ada
  body starts: Welcome to **my site...

----- index.html -----
<!doctype html>
<html><head><title>Home</title></head>
<body><h1>Home</h1><p>by ada</p>
<p>Welcome to <strong>my site</strong>. See the <a href="about.html">about</a> page.</p><ul><li>fast</li><li>pure</li><li>deterministic</li></ul>
</body></html>

----- about.html -----
<!doctype html>
<html><head><title>About</title></head>
<body><h1>About</h1><p>by ada</p>
<h2>About</h2><p>Built with <code>sml-ssg</code>, a pure SML static-site core.</p>
</body></html>
```

## A note on the filesystem

The core is intentionally pure: it never touches disk. Walking a content
directory, reading sources, and writing the rendered HTML out are left to the
caller (a thin shell over `OS.FileSys` / `TextIO`), so the transformation stays
deterministic and trivially testable. `renderSite` is the natural seam: feed it
`(name, source)` pairs read however you like, and write the `(name, html)`
results wherever you like.

## Build & test

Requires [MLton](http://mlton.org/) and/or [Poly/ML](https://polyml.org/).

```sh
make test        # build + run the suite under MLton
make test-poly   # run the suite under Poly/ML
make all-tests   # both
make example     # build + run the demo
make clean
```

## Installing with smlpkg

```sh
smlpkg add github.com/sjqtentacles/sml-ssg
smlpkg sync
```

Reference `src/ssg.mlb` from your own `.mlb` (MLton / MLKit); it pulls in the
vendored sml-markdown and sml-template. For Poly/ML, `use` the sources in
dependency order (see the `test-poly` target in the [Makefile](Makefile)).

## Layout

```
sml.pkg                                       smlpkg manifest (markdown + template)
Makefile                                      MLton + Poly/ML targets
.github/workflows/ci.yml                      CI: MLton + Poly/ML
src/
  ssg.sig         SSG signature
  ssg.sml         frontmatter parser + markdown/template wiring
  ssg.mlb         library basis (brings vendored markdown + template)
lib/github.com/sjqtentacles/                  VENDORED dependencies
  sml-markdown/   CommonMark-subset -> Html.node
  sml-template/   Mustache-style templating
  sml-html/       HTML AST + safe renderer (shared by both)
  sml-buffer/     string buffer (sml-html's dep)
  sml-color/ sml-image/ sml-inflate/          markdown's full vendored tree
examples/
  demo.sml        two-page site rendered through one template
test/
  harness.sml     shared assertion harness
  test.sml        frontmatter + pipeline vectors (16 checks)
  entry.sml / main.sml
```

The `sml-color`, `sml-image`, and `sml-inflate` directories are part of
sml-markdown's vendored tree and are copied faithfully; the SSG build itself
only needs Markdown, Template, HTML, and Buffer.

## Tests

16 deterministic checks: frontmatter parsing (ordered pairs, body separation,
missing/empty blocks, colon-bearing values), `markdownToHtml`, the full
`renderPage` pipeline (including raw `{{{content}}}` injection vs. escaped
`{{...}}`), and `renderSite` order preservation. Run `make all-tests` to verify
identical output under both compilers.

## License

MIT. See [LICENSE](LICENSE).
