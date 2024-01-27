# Changelog

## 1.0.3 (????-??-??)

* Enhancements
  * Add support for JSON encode/decode of `argo_wire_type` and `argo_wire_type_store`.
* Fixes
  * Fix `argo_typer` for `interface` based inline fragments (see [msolomon/argo#7](https://github.com/msolomon/argo/issues/7)).

## 1.0.2 (2024-01-26)

* Fixes
  * Fix path value transformation to match [`PathToWirePath()` and `WirePathToPath()` in Argo 1.1](https://msolomon.github.io/argo/versions/1.1/spec#sec-Path-value-transformation).

## 1.0.1 (2024-01-22)

* Fixes
  * Fix cases where `@include(if: $var)` and `@skip(if: $var)` affect the "omittable" setting of a field when dealing with `FragmentSpread` and `InlineFragment`.

## 1.0.0 (2024-01-03)

* Initial release.
