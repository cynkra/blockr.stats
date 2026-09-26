/**
 * Ambient types for blockr.stats' hand-written JS.
 *
 * Dev-tooling only: type-checked via tsconfig.json / `tsc`, never referenced
 * by an htmlDependency and never run in the browser.
 *
 * The shared controls come from blockr.ui (blockr-ui.js, blockr-select.js,
 * loaded by blockr.ui::controls_dep()); Blockr.Input, the code field with
 * autocomplete, still comes from blockr.dplyr. The Blockr namespace below
 * declares only the slice this package consumes: a subset of blockr.ui's
 * own types.d.ts, to grow only as the JS here does.
 */

/* --- Blockr.Select (blockr-select.js, blockr.ui) --- */

/** Option entry: a bare value string, or {value, label} for a muted label. */
type BlockrSelectOption = string | { value: string; label?: string };

interface BlockrSelectHandleBase {
  /** Root element (already appended to the container). */
  el: HTMLDivElement;
  setOptions(
    opts: BlockrSelectOption[] | BlockrSelectOption | null | undefined,
    sel?: string | string[] | null
  ): void;
  updateOptions(
    opts: BlockrSelectOption[] | BlockrSelectOption | null | undefined,
    sel?: string | string[] | null
  ): void;
  setValue(value: string | string[] | null): void;
  destroy(): void;
}

interface BlockrSelectSingleHandle extends BlockrSelectHandleBase {
  getValue(): string;
}

interface BlockrSelectMultiHandle extends BlockrSelectHandleBase {
  getValue(): string[];
}

interface BlockrSelectConfig {
  options?: BlockrSelectOption[];
  selected?: string | string[] | null;
  placeholder?: string;
  bordered?: boolean;
  allowEmpty?: boolean;
  onChange?: (value: any) => void;
  [opt: string]: unknown;
}

interface BlockrSelectStatic {
  single(container: HTMLElement, config: BlockrSelectConfig): BlockrSelectSingleHandle;
  multi(container: HTMLElement, config: BlockrSelectConfig): BlockrSelectMultiHandle;
}

/* --- Small controls (blockr-ui.js, blockr.ui) --- */

interface BlockrCheckboxHandle {
  el: HTMLLabelElement;
  input: HTMLInputElement;
  set(v: boolean): void;
  get(): boolean;
}

interface BlockrSegmentedHandle {
  el: HTMLDivElement;
  set(value: string): void;
  get(): string;
}

interface BlockrGearTrayHandle {
  set(open: boolean): void;
  toggle(): void;
  isOpen(): boolean;
}

interface BlockrTextCommitHandle {
  chip: HTMLButtonElement;
  commit(): void;
  sync(value: string): void;
}

interface BlockrNamespace {
  /** Shared select component; absent until blockr-select.js has loaded. */
  Select?: BlockrSelectStatic;
  icons: Record<string, string>;
  checkbox(
    label: string,
    checked: boolean,
    onChange: (checked: boolean) => void
  ): BlockrCheckboxHandle;
  segmented(
    options: { value: string; label: string; title?: string }[],
    selected: string,
    onChange: (value: string) => void,
    opts?: { size?: 'xs'; label?: string }
  ): BlockrSegmentedHandle;
  gearTray(
    band: HTMLElement,
    gear: HTMLButtonElement,
    opts?: { label?: string }
  ): BlockrGearTrayHandle;
  textCommit(
    input: HTMLInputElement,
    opts: { onCommit: (value: string) => void }
  ): BlockrTextCommitHandle;
  tooltip: {
    set(el: Element, content: string): void;
    clear(el: Element): void;
  };
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
  Blockr?: BlockrNamespace;
  Shiny?: typeof Shiny;
}
