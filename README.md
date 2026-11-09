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
| AeonikMono Nerd Font Mono (Light) | default / fixed-pitch / icons | in `fonts/` (untracked; commercial, do not distribute). Register once: open the `.otf` files in Font Book. To rebuild from Aeonik Mono: Nerd Fonts FontPatcher, `fontforge -script font-patcher --complete --mono AeonikMono-<Weight>.otf`, then `fontforge -script fonts/fix-symbols.py` (Iosevka NFM symbols) |
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
The ghostel terminal (`C-x p t`) downloads its module into `ghostel/` on first use.
In a Claude session, `C-g` opens the prompt in an Emacs buffer;
`C-x #` sends it back. For long pastes,
paste inside that buffer: Claude's `[Pasted text]` placeholder cannot
be expanded. `PgUp`/`PgDn` and `Ctrl+O` scroll the conversation.
Set `"preferredNotifChannel": "iterm2"` in `~/.claude.json` so Claude
Code emits OSC 9 notifications; these turn a session's name red
in the buffer sidebar (`C-c e`) when it needs input off screen.

## Notes

- Keys: all user commands are on `C-c <letter>` (the map is in the Keys
  section of `init.el`).
- Notes/agenda live in `~/Notes` (created automatically; `inbox.org` and
  `agenda.org` are the agenda anchors, other files opt in with
  `#+filetags: :agenda:`).
- Eshell (`C-x p e`): stock prompt (`[N]` after a failed command), live
  syntax colors, grey history suggestions as you type (TAB takes all,
  `M-f` one word), `C-r` fuzzy history, `cd =regexp` jumps to a recent
  dir (`cd =` lists them). History (10k) is written after each command.
  In a remote dir, a leading `/` means the remote root. Aliases (`ll`,
  `,` for `cd ..`, ...) live in `etc/eshell-aliases`.
- Shells: `C-x p t` / `C-x p e` show the project's terminal or eshell;
  `C-u` starts one more.
- Scratchpad: `*scratch*` is persistent (`remember-notes`, file `notes`
  here); killing it only buries it.
- `C-c n a` opens the overview agenda; `C-u C-c n a` the dispatcher.
- Nix machines: `/etc/profiles/per-user/vp/bin` joins PATH when present.
