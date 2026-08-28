# emacs.d

Built-in-first config for **Emacs 31**. Clone into `~/.emacs.d`; packages,
tree-sitter grammars, and the ghostel module install themselves on first
start.

## Prerequisites

### Emacs

```sh
brew install emacs-plus@31        # GUI + daemon build
brew services start emacs-plus@31 # daemon via launchd
```

Connect with `emacsclient -c` (GUI) or `emacsclient -t` (terminal).

### Fonts

| Font | Use | Install |
|---|---|---|
| Aeonik Mono (Medium) | default / fixed-pitch | commercial, install manually |
| Symbols Nerd Font | symbol fallback, dired/ibuffer icons | `brew install --cask font-symbols-only-nerd-font` |
| Vollkorn | eww + markdown preview serif | `brew install --cask font-vollkorn` |

### CLI tools

```sh
brew install coreutils   # gls: dired --group-directories-first
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

### Terminal frames (optional)

kkp needs a kitty-keyboard-protocol terminal (ghostty, wezterm) for full
modifier keys in `emacsclient -t`.

## Notes

- Notes/agenda live in `~/Notes` (created automatically; `inbox.org` and
  `agenda.org` are the agenda anchors, other files opt in with
  `#+filetags: :agenda:`).
- Nix machines: `/etc/profiles/per-user/vp/bin` joins PATH when present.
