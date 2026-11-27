# emacs.d

Built-in-first config for **Emacs 31**. Clone into `~/.emacs.d`; packages,
tree-sitter grammars, and the ghostel module install themselves on first
start.

## Prerequisites

### Emacs

```sh
brew install emacs-plus@31
```

Run `Emacs.app`; no daemon. It starts a server, so `emacsclient` and
`$EDITOR` reach it. Quit and reopen to apply changes: the desktop
restores files, windows, and Claude sessions.

### Fonts

| Font | Use | Install |
|---|---|---|
| AeonikMono Nerd Font Mono (Light) | default / fixed-pitch / icons | in `fonts/` (untracked; commercial, do not distribute). Register once: open the `.otf` files in Font Book. To rebuild from Aeonik Mono: Nerd Fonts FontPatcher, `fontforge -script font-patcher --complete --mono AeonikMono-<Weight>.otf`, then `fontforge -script fonts/fix-symbols.py` (Iosevka NFM symbols) |
| Vollkorn | markdown preview serif | `brew install --cask font-vollkorn` |

### CLI tools

```sh
brew install ripgrep     # consult-ripgrep, xref, project search
brew install fd          # consult-fd (C-c f)
brew install pandoc      # markdown preview rendering
brew install git         # magit, package-vc, treesit grammar builds
brew install mermaid-cli # mmdc: markdown preview renders mermaid fences
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
The sidebar spinner and activity need async command hooks in
`~/.claude/settings.json` that call `vp/claude-hook` with `$PPID`:
`UserPromptSubmit` sends `start`; `Stop`, `StopFailure` and
`Notification` (matcher `idle_prompt`) send `stop`, as in

```sh
emacsclient --eval "(vp/claude-hook $PPID 'start)" >/dev/null 2>&1 || true
```

`PreToolUse` sends the tool and its target, using jq (`brew install jq`):

```sh
jq -r --arg pid "$PPID" '"(vp/claude-hook \($pid|tonumber) (quote tool) \(.tool_name|@json) \((.tool_input.file_path // .tool_input.command // .tool_input.pattern // .tool_input.url // .tool_input.query // "")|tostring|.[0:200]|@json) \((.tool_input.description // "")|tostring|.[0:200]|@json))"' | { read -r f; emacsclient --eval "$f"; } >/dev/null 2>&1 || true
```

Rename a session with `/rename` to shorten its sidebar title.

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
- Sidebar (`C-c e`): click a row to visit, a group title (or `s-f`) to
  fold; middle-click kills, right-click opens a menu. Marks: `●` after a
  name = unsaved, `*` after a group = uncommitted git changes, Claude
  spinner = the session is working (dim robot = idle).
- Shells: `C-x p t` / `C-x p e` show the project's terminal or eshell;
  `C-u` starts one more.
- Scratchpad: `*scratch*` is persistent (`remember-notes`, file `notes`
  here); killing it only buries it.
- `C-c n a` opens the agenda dispatcher; `o` is the overview.
- PATH: emacs-plus stores the shell PATH at `brew install` time
  (`EMACS_PLUS_PATH`). After a PATH change, `brew reinstall emacs-plus@31`.
