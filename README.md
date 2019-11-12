# God Mode — no more RSI

***NOTE***: Emacs 24.3 is required for this package to work well!

This is a global minor mode for entering Emacs commands without
modifier keys. It's similar to Vim's separation of commands and
insertion mode.

## What's new

- Added `god-mode-low-priority-keys` that allows certain keys to
  trigger local bindings in special modes like `dired`. See [Working
  with special modes](#working-with-special-modes).
- Added `god-mode-translate-alist`. This allows exceptions from our
  regular mapping. See [Translation Map](#translation-map).
- Added `god-mode-can-omit-literal-key`. When non-nil, you can omit
  the `SPC` key in unambiguous cases. See [Omitting the `SPC`
  key](#omitting-the-spc-key).
- Pressing `C-h` (see `help-char`) will show help on key bindings from
  the prefix already entered, akin to regular Emacs.
- `god-mode-describe-key` now bound to `C-h k` in god-mode.

## Example

In the example below you can see how much effort is reduced:

    Before: C-p C-k C-n M-^ C-j C-y M-r C-x z z M-2 C-x C-s
    After:    p   k   n   ^   j   y g r     z z   2   x   s

(Regarding `^` and `z` see [nice keybindings](#nice-keybindings) section.)

You'll find that this mode comes surprisingly naturally and that you
already know how to run your existing Emacs commands.

See the Mapping section for a complete rundown of the transformations.

## Activation

Load it up:

``` lisp
(require 'god-mode)
```

Activate for all future buffers by running `M-x god-mode`. Although the
activation is buffer-local.

Toggle between God mode and non-God mode using `ESC`:

``` lisp
(global-set-key (kbd "<escape>") 'god-local-mode)
```

If you want to enable/disable on *all active and future buffers*, use
this:

``` lisp
(global-set-key (kbd "<escape>") 'god-mode-all)
```

If you are using the global mode, you might want to make no buffers
exempt:

``` lisp
(setq god-exempt-major-modes nil)
(setq god-exempt-predicates nil)
```

This means that e.g. magit-mode or dired-mode will also enter god-mode
when you activate it globally, and vise-verse. It means you can always
reliably use god-mode commands in any buffer as long as it is globally
activated.

Also, you can add this to your `.xmodmap` to rebind Caps Lock to
Escape:

``` lisp
remove Lock = Caps_Lock
keysym Caps_Lock = Escape
```

And run `xmodmap .xmodmap` for the changes to take effect immediately.

Or use dconf:

    dconf write /org/gnome/desktop/input-sources/xkb-options "['caps:escape']"

See [here](http://askubuntu.com/questions/363346/how-to-permanently-switch-caps-lock-and-esc) for more details.

## Mapping

This library defines the following mapping:

* All commands are assumed to be `C-<something>` unless otherwise
   indicated. Examples:

   * `a`    → `C-a`
   * `s`    → `C-s`
   * `akny` → `C-a C-k C-n C-y`
   * `xs`   → `C-x C-s`
   * `x s`  → `C-x s`

   Note the use of space to produce `C-x s`.

* `g` is a special key to indicate `M-<something>`. This means that
   there is no way to write `C-g` in this mode, you must therefore
   type `C-g` directly. Examples:

   * `gf` → `M-f`
   * `gx` → `M-x`

* `G` is a special key to indicate `C-M-<something>`. Example:

   * `Gx` → `C-M-x`

* Digit arguments:

  * `12f` → `M-12 C-f`

* Repetition (with `.` keybinding):

  * `gf..` → `M-f M-f M-f`

* Universal boolean argument:

  * `uco` → `C-u C-c C-o`

## Omitting the `SPC` key

Optionally, you can omit the literal key (`SPC`) in unambiguous situations:

```lisp
(setq god-mode-can-omit-literal-key 't)
```

With this setting, key sequence `x1` will be interpreted to `C-x 1`
instead of `C-x C-1`, provided the latter is not bound to anything
while the former is. This also works on prefixes, e.g. `x4o` is
interpreted as `C-x 4 o`.

## Translation map

In addition to the default mapping, you can add exceptions to
this rule by customizing `god-mode-translate-alist`. The key of this
list is an ordinary key sequence. For example,
if you have `("C-x C-1" "C-x 1")` in the list, press `x 1` will
trigger command bound to `C-x 1`, instead of `C-x C-1`.

This rebinding also works on prefixes. If a third element is `t`, for
the remainder of sequence keys will be interpreted literally. For
example, if you have `("C-x C-4" "C-x 4" t)` in the alist, press
`x 4 d` will trigger command `dired-other-window`, bound to `C-x 4 d`.

It can also be useful to change the default modifier to some keys,
for example:

* `("C-;" "M-;")` allows you to press `;` for `comment-dwim`, bound to `M-;`.
* `("C-#" "C-x #")` allows you to press `#` for `server-edit`.

## Cursor style to indicate mode

You can change the cursor style indicate whether you're in God mode or
not.

``` lisp
(defun my-update-cursor ()
  (setq cursor-type (if (or god-local-mode buffer-read-only)
                        'box
                      'bar)))

(add-hook 'god-mode-enabled-hook 'my-update-cursor)
(add-hook 'god-mode-disabled-hook 'my-update-cursor)
```

## Change modeline color

You can use the following function to switch the entire modeline's foreground and background:

``` lisp
(defun c/god-mode-update-cursor ()
  (let ((limited-colors-p (> 257 (length (defined-colors)))))
    (cond (god-local-mode (progn
                            (set-face-background 'mode-line (if limited-colors-p "white" "#e9e2cb"))
                            (set-face-background 'mode-line-inactive (if limited-colors-p "white" "#e9e2cb"))))
          (t (progn
               (set-face-background 'mode-line (if limited-colors-p "black" "#0a2832"))
               (set-face-background 'mode-line-inactive (if limited-colors-p "black" "#0a2832")))))))
```

## Overwrite mode

You can pause `god-mode` when `overwrite-mode` is enabled and resume
when `overwrite-mode` is disabled.

``` lisp
(defun god-toggle-on-overwrite ()
  "Toggle god-mode on overwrite-mode."
  (if (bound-and-true-p overwrite-mode)
      (god-local-mode-pause)
    (god-local-mode-resume)))

(add-hook 'overwrite-mode-hook 'god-toggle-on-overwrite)
```

## isearch integration

There is a small module for providing god-mode-like behaviour for
isearch: You can hit <escape> while in isearch, for example:

    s hello <escape> s s s RET

For

    C-s hello C-s C-s C-s RET

Activate and configure with the following:

``` lisp
(require 'god-mode-isearch)
(define-key isearch-mode-map (kbd "<escape>") 'god-mode-isearch-activate)
(define-key god-mode-isearch-map (kbd "<escape>") 'god-mode-isearch-disable)
```

Configure `god-mode-isearch-map` for additional keybindings.

## Using with org-mode fast keys

If you want to use god-mode with fast-keys, you can use a rebinding of
self-insert like this:

``` lisp
(define-key god-local-mode-map [remap self-insert-command] 'my-god-mode-self-insert)

(defun my-god-mode-self-insert ()
  (interactive)
  (if (and (bolp)
           (eq major-mode 'org-mode))
      (call-interactively 'org-self-insert-command)
    (call-interactively 'god-mode-self-insert)))
```

## Nice keybindings

The following customizations are popular:

``` lisp
(define-key god-local-mode-map (kbd "z") 'repeat)
(define-key god-local-mode-map (kbd "i") 'god-local-mode)
```

Although this can be more relatable for some people:

``` lisp
(define-key god-local-mode-map (kbd ".") 'repeat)
```

Alternatively, to obey `god-mode-low-priority-keys`, use

```lisp
(add-to-list god-mode-translate-alist '("C-z" "C-x z"))
```
for the same effect of binding `z` to `repeat`.

Feel free to alter and customize as you prefer.

Also consider `(setq god-mode-can-omit-literal-key 't)`,
so that you can run `x1`/`x2`/`x3`/`x0` in god-mode.

Emacs generally do not bind to "Ctrl-symbol" keys, as they do not work
over terminal. It is useful to translate these to "M-symbol" keys
instead. The config I use:

```lisp
(setq god-mode-translate-alist
      '(;; C-[ is a prefix key (ESC), remap here.
        ("C-x C-[" "C-x [") ;;("C-x C-]" "C-x ]")
        ;; use bracket for navigation
        ("C-[" "C-M-a") ("C-]" "C-M-e")
        ;; one-key command that makes most sense
        ("C-`" "C-x `") ("C-#" "C-x #") ("C-z" "C-x z")
        ;; C-i is interpreted as TAB, remap here.
        ("M-g C-i" "M-g i")
        ;; one-key command that maps to M-?
        ("C-~" "M-~") ("C-!" "M-!") ("C-@" "M-@") ("C-$" "M-$") ("C-%" "M-%")
        ("C-^" "M-^") ("C-&" "M-&") ("C-*" "M-*") ("C-{" "M-{") ("C-}" "M-}")
        ("C-<" "M-<") ("C->" "M->") ("C-:" "M-:") ("C-|" "M-|") ("C-\\" "M-\\")
        ("C-+" "M-+") ("C-=" "M-=") ("C-?" "M-?")))
```

## Working with special modes

In special modes like `dired`, it would be helpful if local bindings
are respected for some keys. This functionality is provided by
`god-mode-low-priority-keys`. For example:

``` lisp
(setq god-mode-low-priority-keys '(?q))
```
Then in `dired` mode, key `q` will call `quit-window` (bound to `q`),
instead of `quoted-insert` (bound to `C-q`).

It can also be helpful to bind `x` to `god-mode-self-insert` for all
special modes, so that commands like `x 1` work without switching on
`god-mode`:

``` lisp
(define-key special-mode-map (kbd "x") 'god-mode-self-insert)
;; tabulated-list-mode initializes weirdly.
(define-key tabulated-list-mode-map (kbd "x") 'god-mode-self-insert)
```

Some modes like `dired` or `package-list-mode` use `x` for an
"execution" command, which will shadow the binding above.

## Global god-mode and exempt major modes

**Note:** This is less necessary in recent god-mode, as god-mode
  overrides all printable single byte keys, so it will override
  dired-mode or magit-mode.

If you do `M-x god-mode`, then all buffers will be started in God
mode. If you don't like that behavior, just use the `god-local-mode`
toggler with a keybinding.

Sometimes `god-mode` is enabled in buffers where it makes no sense. In
that case you can add the major mode to `god-exempt-major-modes`:

``` lisp
(add-to-list 'god-exempt-major-modes 'dired-mode)
```

Since `dired-mode` is already in the list, that's a noop, but you get
the idea. Consider opening an issue or pull request if you find a
major mode that should be on the official list.

Another option to control god-mode's global behavior is to provide a
function with no arguments that must return non-nil if god-mode should
be disabled for the current buffer. See the `god-exempt-predicates`
variable and its default members `god-exempt-mode-p`,
`god-comint-mode-p`, `god-view-mode-p` and `god-special-mode-p` for
further details.
