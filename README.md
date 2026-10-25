# emacs.d

Built-in-first config for **Emacs 31**. Clone into `~/.emacs.d`; packages,
tree-sitter grammars, and the ghostel module install themselves on first
start.

## Prerequisites

### Emacs

```sh
brew install emacs-plus@31
brew services start emacs-plus@31   # daemon via launchd: starts at login, respawns
ditto /opt/homebrew/opt/emacs-plus@31/"Emacs Client.app" /Applications/"Emacs Client.app"
osacompile -o "/Applications/Emacs Client.app/Contents/Resources/Scripts/main.scpt" etc/emacs-client.applescript
codesign --force --sign - "/Applications/Emacs Client.app"
```

The daemon is the only Emacs. Connect with `Emacs Client.app`
(Dock/Spotlight); GUI frames only. Never launch `Emacs.app` directly and never use `emacsclient
-a ''`: both spawn a second Emacs outside launchd's control. The
shipped `Emacs Client.app` applet hardcodes a Cellar version path and
an `-a ''` fallback; `etc/emacs-client.applescript` replaces it with a
plain no-fallback client. After `brew upgrade emacs-plus@31`, run
`brew services restart emacs-plus@31`.

### Fonts

| Font | Use | Install |
|---|---|---|
| AeonikMono Nerd Font Mono (Light) | default / fixed-pitch / icons | in `fonts/` (untracked; commercial, do not distribute). Register once: open the `.otf` files in Font Book. To rebuild from Aeonik Mono: Nerd Fonts FontPatcher, `fontforge -script font-patcher --complete --mono AeonikMono-<Weight>.otf`, then `fontforge -script fonts/fix-symbols.py` (copies uniform symbol glyphs from Iosevka NFM, see the script) |
| Vollkorn | eww + markdown preview serif | `brew install --cask font-vollkorn` |

### CLI tools

```sh
brew install ripgrep     # consult-ripgrep, xref, project search
brew install pandoc      # markdown preview rendering
brew install git         # magit, package-vc, treesit grammar builds
brew install mermaid-cli # mmdc: ob-mermaid renders org mermaid blocks
```

Tree-sitter grammars build on first use and need git plus a C compiler
(Xcode Command Line Tools: `xcode-select --install`).

### Language servers (eglot, optional per language)

```sh
brew install gopls bash-language-server marksman
pipx install python-lsp-server   # pylsp
```

### Claude Code (optional)

The `claude` CLI must be on PATH for claude-code-ide (`C-c a`).
The ghostel terminal (`C-c o t`) downloads its module into `ghostel/` on first use.
In a Claude session, `C-g` opens the prompt in an Emacs buffer (meow
works there); `C-c C-c` or `C-x #` sends it back. For long pastes,
paste inside that buffer: Claude's `[Pasted text]` placeholder cannot
be expanded. `PgUp`/`PgDn` and `Ctrl+O` scroll the conversation.
Set `"preferredNotifChannel": "iterm2"` in `~/.claude.json` so Claude
Code emits OSC 9 notifications; these turn a session's name red
in the buffer sidebar (`C-c e`) when it needs input off screen.
The sidebar also lists the five newest dormant sessions from
`~/.claude/projects/`; `RET` on one resumes it in its directory.

### Text to speech (optional)

`C-c t t` (or `s-r`) speaks the region with
[kokoro-tts](https://github.com/nazdridoy/kokoro-tts);
`C-c t =` / `C-c t -` adjust the speed (repeatable with bare `=` / `-`). The
code lives in `lisp/vp-tts.el` and loads on first use. It calls the
`kokoro` wrapper from the kokoro-tts-config repo
(`~/Git/public/kokoro-tts-config`, symlinked to `~/.local/bin/kokoro`),
which points the CLI at the model weights in `~/.local/share/kokoro-tts`;
see that repo's README for setup on a new machine.

## Notes

- Keys: all user commands are on `C-c <letter>` (the map is in the Keys
  section of `init.el`).
- Modal editing: meow with the official Colemak layout. `SPC` is the
  stock meow keypad: `SPC <letter>` is `C-c <letter>`, and `SPC x`,
  `SPC c`, `SPC h`, `SPC m`, `SPC g` type `C-x C-`, `C-c C-`, `C-h`,
  `M-`, `C-M-` chords (`SPC x f` find file, `SPC m x` M-x). `U` is
  undo-redo. Shells open in insert state.
- Notes/agenda live in `~/Notes` (created automatically; `inbox.org` and
  `agenda.org` are the agenda anchors, other files opt in with
  `#+filetags: :agenda:`).
- Eshell (`C-c o e`): two-line prompt (dir and git branch, then `❯`, red
  after a failed command), live syntax colors, grey history suggestions as you type (TAB takes
  all, `M-f` one word), `C-r` fuzzy history, `z <words>` jumps to a
  recent dir (`z` alone picks one, `z TAB` completes). History (10k) is
  written after each command. Aliases (`ll`, `,` for `cd ..`, `gg msg`
  to commit, ...) live in `etc/eshell-aliases`.
- Shells: `C-c o t` / `C-c o e` show the terminal or eshell for this
  directory; pressed inside one, they start one more.
- Scratchpad: `M-x remember-notes` (`*notes*`, first row of the sidebar)
  visits `notes` here and saves on idle; killing it only buries it.
- `C-c n a` opens the overview agenda; `C-u C-c n a` the dispatcher.
- Nix machines: `/etc/profiles/per-user/vp/bin` joins PATH when present.
