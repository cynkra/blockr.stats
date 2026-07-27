/**
 * Ambient types for blockr.stats' hand-written JS.
 *
 * Dev-tooling only: type-checked via tsconfig.json / `tsc`, never referenced
 * by an htmlDependency and never run in the browser.
 *
 * blockr.stats reuses blockr.dplyr's shared JS (blockr-core.js +
 * blockr-select.js) at runtime, so the Blockr namespace below declares only
 * the slice this package consumes — it is intentionally a subset of
 * blockr.dplyr's own types.d.ts, and should grow only as the JS here does.
 */

/* --- Blockr.Select (blockr-select.js, blockr.dplyr) --- */

/** Option entry: a bare value string, or {value, label} for a muted label. */
type BlockrSelectOption = string | { value: string; label?: string };

interface BlockrSelectSingleHandle {
  /** Root element (already appended to the container). */
  el: HTMLDivElement;
  getValue(): string;
  setOptions(
    opts: BlockrSelectOption[] | BlockrSelectOption | null | undefined,
    sel?: string | null
  ): void;
  destroy(): void;
}

interface BlockrSelectSingleConfig {
  options?: BlockrSelectOption[];
  /** Initial value (null/undefined: first option). */
  selected?: string | null;
  placeholder?: string;
  onChange?: (value: string) => void;
  [opt: string]: unknown;
}

interface BlockrSelectStatic {
  single(
    container: HTMLElement,
    config: BlockrSelectSingleConfig
  ): BlockrSelectSingleHandle;
}

/* --- Blockr.checkbox (settings-band.js, vendored from blockr.viz) --- */

/** Handle returned by Blockr.checkbox. */
interface BlockrCheckboxHandle {
  el: HTMLLabelElement;
  input: HTMLInputElement;
  set(v: boolean): void;
  get(): boolean;
}

interface BlockrNamespace {
  /** Shared select component; absent until blockr-select.js has loaded. */
  Select?: BlockrSelectStatic;
  /** Design-system checkbox factory (settings-band.js). */
  checkbox(
    label: string,
    checked: boolean,
    onChange: (checked: boolean) => void
  ): BlockrCheckboxHandle;
  /** The namespace carries members this package does not consume. */
  [member: string]: unknown;
}

declare var Blockr: BlockrNamespace;

/* --- Ambient Shiny (no @types dependency) --- */

declare const Shiny: {
  setInputValue(
    name: string,
    value: unknown,
    opts?: { priority?: 'event' | 'immediate' | 'deferred' }
  ): void;
};

interface Window {
  // Optional: the mount code guards on their presence before using them.
  Blockr?: BlockrNamespace;
  Shiny?: typeof Shiny;
}
