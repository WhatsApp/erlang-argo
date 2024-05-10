# Changelog

## 1.0.9 (2024-05-10)

* Enhancements
  * Add `argo_limits` and `argo_varbit` for controlling the maximum number of bits supported in `HasUserFlags` (defaults to 700-bits).
  * Add new functions to `argo_header`:
    * `argo_header:from_http_argo_mode/1` and `argo_header:to_http_argo_mode/1`
      * For use with HTTP header `Argo-Mode`.
      * For example:
        ```
        Argo-Mode: InlineEverything;SelfDescribing;OutOfBandFieldErrors;SelfDescribingErrors;NullTerminatedStrings;NoDeduplication;HasUserFlags:10
        ```
    * `argo_header:from_u64/1` and `argo_header:to_u64/1`
      * For use with encode/decode of `ArgoHeader` to an unsigned 64-bit integer.
      * For example:
        ```erlang
        argo_header:from_u64(32640).
        ```
    * `argo_header:from_uint/1` and `argo_header:to_uint/1`
      * For use with encode/decode of `ArgoHeader` to an unsigned integer (may be larger than 64-bits).
      * For example:
        ```erlang
        argo_header:from_uint(2586086763326778769917682175246592).
        ```

## 1.0.8 (2024-04-29)

* Fixes
  * Fix edge case where `NON_NULL(DESC(NULL))` is the same as `NULL` (see [msolomon/argo#18](https://github.com/msolomon/argo/issues/18)).

## 1.0.7 (2024-04-28)

* Enhancements
  * Support `DESC` type for scalars as described in [Argo 1.2.0](https://msolomon.github.io/argo/versions/1.2/spec#sec-v1-2-0)).

## 1.0.6 (2024-04-27)

* Enhancements
  * Add new functions to `argo_index_map`:
    * `argo_index_map:groups_from_list/2`
    * `argo_index_map:groups_from_list/3`
  * Add new function to `argo_graphql_field`:
    * `argo_graphql_field:get_response_key/1`
  * Reorganize `argo_typer` to better match upstream.
* Fixes
  * Minor correction to "Field Selection Merging" from [GraphQL Spec: 5.3.2 Field Selection Merging](https://spec.graphql.org/draft/#sec-Field-Selection-Merging) (see [msolomon/argo#19](https://github.com/msolomon/argo/pull/19)).

## 1.0.5 (2024-04-23)

* Enhancements
  * Add new functions to `argo_index_map`:
    * `argo_index_map:filter/2`
    * `argo_index_map:filtermap/2`
    * `argo_index_map:sort/1`
    * `argo_index_map:sort/2`
  * Add new functions to `argo_index_set`:
    * `argo_index_set:sort/1`
    * `argo_index_set:sort/2`
* Fixes
  * Unwrap nested `NULLABLE(NULLABLE(...))` and treat a `NON_NULL(DESC(null))` the same as `NULL` (see [msolomon/argo#18](https://github.com/msolomon/argo/issues/18)).
  * Implement "Field Selection Merging" from [GraphQL Spec: 5.3.2 Field Selection Merging](https://spec.graphql.org/draft/#sec-Field-Selection-Merging) (see [msolomon/argo#19](https://github.com/msolomon/argo/pull/19)).

## 1.0.4 (2024-04-17)

* Fixes
  * Various fixes to conform with [Argo 1.1.4](https://msolomon.github.io/argo/versions/1.1/spec#sec-v1-1-4)).

## 1.0.3 (2024-02-14)

* Enhancements
  * Add support for JSON encode/decode of `argo_wire_type` and `argo_wire_type_store`.
  * Add an `EXTENSIONS` wire type as a specialization of `DESC` specific to JSON Object.
  * Add `argo` module which can be used to quickly display or format types for debugging.
  * Add support for `BYTES` and `FIXED` encode/decode when dealing with JSON values so that implementers may customize the scalar encode/decode behavior.
* Fixes
  * Fix `argo_typer` for `interface` based inline fragments (see [msolomon/argo#7](https://github.com/msolomon/argo/issues/7)) (now conforms with [Argo 1.1.1](https://msolomon.github.io/argo/versions/1.1/spec#sec-v1-1-1)).
  * Fix `argo_typer` to have a more strict type derivation for `errors` and `extensions`.

## 1.0.2 (2024-01-26)

* Fixes
  * Fix path value transformation to match [`PathToWirePath()` and `WirePathToPath()` in Argo 1.1](https://msolomon.github.io/argo/versions/1.1/spec#sec-Path-value-transformation).

## 1.0.1 (2024-01-22)

* Fixes
  * Fix cases where `@include(if: $var)` and `@skip(if: $var)` affect the "omittable" setting of a field when dealing with `FragmentSpread` and `InlineFragment`.

## 1.0.0 (2024-01-03)

* Initial release.
