(* sml-ssg demo: render a tiny two-page "site" from frontmatter + Markdown
   through one shared template. All inputs are fixed, so the output is fully
   deterministic and byte-identical across MLton and Poly/ML. *)

fun line s = print (s ^ "\n")

val template =
  "<!doctype html>\n\
  \<html><head><title>{{title}}</title></head>\n\
  \<body><h1>{{title}}</h1><p>by {{author}}</p>\n\
  \{{{content}}}\n\
  \</body></html>"

val pages =
  [ ("index", "---\ntitle: Home\nauthor: ada\n---\n\
              \Welcome to **my site**. See the [about](about.html) page.\n\n\
              \- fast\n- pure\n- deterministic")
  , ("about", "---\ntitle: About\nauthor: ada\n---\n\
              \## About\n\nBuilt with `sml-ssg`, a pure SML static-site core.") ]

val () = line "sml-ssg demo"
val () = line "============"
val () = line ""

val () = line "parseFrontmatter of the index page:"
val { frontmatter, body } = Ssg.parseFrontmatter (#2 (hd pages))
val () = List.app (fn (k, v) => line ("  " ^ k ^ " = " ^ v)) frontmatter
val () = line ("  body starts: " ^ String.substring (body, 0, 20) ^ "...")
val () = line ""

val rendered = Ssg.renderSite { template = template } pages

val () = List.app
  (fn (name, html) =>
     ( line ("----- " ^ name ^ ".html -----")
     ; line html
     ; line "" ))
  rendered
