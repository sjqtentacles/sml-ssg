(* template.sig

   A logic-light Mustache/Handlebars-style templating engine in pure Standard
   ML. Interpolated values are HTML-escaped by default (via sml-html's
   `Escape.text`); a triple-brace tag emits raw, unescaped output.

   Supported tags:
     {{var}}            interpolate `var`, HTML-escaped.
     {{{var}}}          interpolate `var`, raw (unescaped).
     {{#section}}..{{/section}}   section: render inner when truthy; iterate
                                  over a list; skip when false/absent.
     {{^section}}..{{/section}}   inverted section: render inner only when the
                                  value is falsy/empty/absent.
     {{! comment }}     comment, removed from output.

   Lookups walk a stack of `Map` scopes (inner scope shadows outer). A missing
   key renders as the empty string. Dotted keys `{{a.b}}` are supported as a
   bonus: each path segment is resolved against the current map value. *)

signature TEMPLATE =
sig
  (* A context value. Sections treat values for truthiness as follows:
       Bool b   -> b
       List xs  -> not (null xs)
       Null     -> false
       Str ""   -> false, any other Str -> true
       Num _    -> true
       Map _    -> true *)
  datatype value =
      Str  of string
    | Bool of bool
    | Num  of int
    | List of value list
    | Map  of (string * value) list
    | Null

  (* Compiled template AST (abstract). *)
  type template

  (* Parse template source into a compiled template. Raises `Parse` on
     malformed input (e.g. an unclosed or mismatched section). *)
  exception Parse of string

  val parse        : string -> template
  val render       : template -> value -> string
  val renderString : string -> value -> string
end
