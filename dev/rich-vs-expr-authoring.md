# Rich vs. expression authoring in JS-driven blocks

**Staging note.** This lives in `blockr.stats/dev/` while the `formula-input`
widget proves it out (spec: `blockr.design/open/formula-input/`). Once stable,
**promote to blockr.docs** so other packages can rely on it as shared block-API
guidance — blockr.docs is the source of truth for block patterns.

## The problem

A JS-driven input block often wants two ways to author the same thing: a
point-and-click **rich** UI (chips / dropdowns / role boxes) and a raw
**expression** escape hatch (typed code). The question is how the two relate.
Two architectures answer it differently.

### Architecture A — text-preserving / tagged rows

State is a list of rows, each permanently tagged with its mode. A rich row stays
rich, an expr row stays expr; there is no parse-back from typed expression into
rich controls. The builder rows *are* the state; an expression is just another
row type, kept verbatim. **blockr.dplyr** filter/summarize do this
(`type = "values" | "numeric" | "expr"`, `"simple" | "expr"`).

### Architecture B — symmetric / single source of truth

One canonical structure is the truth; the rich builder and the text field are
both projections of it. Editing either updates the one structure and both views
re-render. **formula-input** does this. (Single-document-model principle —
ProseMirror / CodeMirror / Lexical; Toast UI Editor 3.0 is the clearest "two
synced buffers failed → rebuilt on one model" writeup.)

## The deciding test

> Does the grammar have **(1)** a parser (text → structure) and **(2)** a
> principled canonical normal form — an equivalence relation — so that
> normalizing typed text is *honest* rather than destructive?

- **Yes → Architecture B.** Eager sync is safe, you get one surface, lossless
  toggling, and the two views can never disagree. An **R formula** qualifies:
  `stats::terms()` is *both* the parser and the normal-form definition, and the
  normalization is gentle and honest —
  `x + x → x` (dedup, not arithmetic), `a*b → a + b + a:b` (shorthand expansion),
  `0 + a` → intercept attribute, main-effect order preserved, and
  `poly(x,2)` / `I(x^2)` / `ns(z,3)` / `log(x)` **kept verbatim as single
  terms**. So normalization only ever shows the model that is *actually fitted*;
  there is little to "protect" an expression author from.

- **No → Architecture A.** Preserve authored text; rich and expr coexist as
  tagged rows; no parse-back. A **dplyr filter/summarize body** fails the test —
  open-ended R, a boolean *tree* with precedence, no `terms()`-equivalent and no
  canonical equivalence — so parsing-back would risk rewriting what the user
  deliberately typed.

This is why the two packages decide oppositely. It is **not an inconsistency** —
it is a consequence of one grammar having a `terms()` and the other not.

## Rules that hold under both architectures

1. **Never silently drop input.** Out-of-grammar input survives as an opaque
   expression chip/row, round-trips untouched. (The Joplin "preserve unrecognized
   markup" data-loss trap.)
2. **In-scope grammar gets rich controls — including the powerful constructs.**
   If the parser keeps `poly()`/`ns()`/`I()` as single terms, each maps cleanly
   to one rich chip with a parameter popover. The opaque fallback is for
   *genuinely out-of-grammar* R (user functions, etc.), never an excuse to
   under-build in-scope features. The boundary of "in-scope" is the parity
   contract and must be stated explicitly.
3. **One model, many projections.** Don't maintain two synced text buffers; keep
   one structure and render both views from it.

## One-line heuristic

Can you parse it back, and is the normal form honest? **Yes → unify (B). No →
preserve text (A).**

## Cross-references

- Spec: `blockr.design/open/formula-input/` (see `architecture-dual-mode.md`).
- Consumer: `new_model_block` in `blockr.stats/R/model-block.R` (currently
  hand-rolls the formula from selectize inputs — the widget replaces that).
