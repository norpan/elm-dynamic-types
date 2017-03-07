# Dynamic Types in Elm

This is an experiment in implementing dynamic data and types in Elm.
One rationale for this is to be able to write something like this:
```elm
Dynamic.Json.decoder #Type : Json.Decode.Decoder Type
```

However, there is obviosly a lot of other things that can be done too.

## Compiler extension
I started experimenting with a compiler extension, and it seems feasible
to hook into the "Canonicalize" stage and just generate ValidExpr from a raw
"#Type" expression. We have the canonical type there and generating valid
expressions seems straightforward enough.

But it's not done yet.

## Example
See the `Example.elm` file for an example on how the generated code would
look and how it could be used.

```
Example.run Example.test
```
