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

The daemon is the only Emacs. Connect with `emacsclient -nw` (the `e`
alias) in a terminal, or `Emacs Client.app` (Dock/Spotlight) for GUI
frames. Never launch `Emacs.app` directly and never use `emacsclient
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
brew install ripgrep     # xref / project search
brew install pandoc      # markdown preview rendering
brew install git         # magit, package-vc, treesit grammar builds
```

Tree-sitter grammars build on first use and need git plus a C compiler
(Xcode Command Line Tools: `xcode-select --install`).

### Language servers (eglot, optional per language)

```sh
brew install gopls bash-language-server marksman
pipx install python-lsp-server   # pylsp
```

### Claude Code (optional)

The `claude` CLI must be on PATH for claude-code-ide (`C-c i`).
The ghostel terminal module downloads itself into `ghostel/` on first use.

### Text to speech (optional)

`C-c t` (or `s-'` in GUI frames) speaks the region with
[kokoro-tts](https://github.com/nazdridoy/kokoro-tts) (`~/.local/bin/kokoro-tts`);
`C-c +` / `C-c -` adjust the speed (repeatable with bare `+` / `-`). The model files `kokoro-v1.0.onnx` and
`voices-v1.0.bin` must be in `~/Git/kokoro-tts/` (see `vp/tts-model-directory`).

### Terminal frames (optional)

kkp needs a kitty-keyboard-protocol terminal (ghostty, wezterm) for full
modifier keys in `emacsclient -t`.

## Notes

- Notes/agenda live in `~/Notes` (created automatically; `inbox.org` and
  `agenda.org` are the agenda anchors, other files opt in with
  `#+filetags: :agenda:`).
- Nix machines: `/etc/profiles/per-user/vp/bin` joins PATH when present.
