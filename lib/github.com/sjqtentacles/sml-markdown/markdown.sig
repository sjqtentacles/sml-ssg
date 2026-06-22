(* markdown.sig

   A CommonMark-subset Markdown parser. Produces an `Html.node` tree from the
   sml-html library, plus a convenience that renders straight to an HTML string.

   Supported blocks: ATX headings, paragraphs, fenced/indented code blocks,
   blockquotes, unordered/ordered lists (flat, with nesting), thematic breaks.
   Supported inline: emphasis, strong, code spans, links, images, autolinks,
   hard line breaks, backslash escapes. *)

signature MARKDOWN =
sig
  (* Parse Markdown source into a list of block-level HTML nodes. *)
  val parse : string -> Html.node list

  (* Render Markdown source straight to an HTML string.
     Equal to `Html.renderList o parse`. *)
  val toHtml : string -> string
end
