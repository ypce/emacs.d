-- Render ```mermaid fences to inline SVG via mermaid-cli (mmdc).
-- Used by markdown-mode's preview/export (markdown-command in init.el).
-- SVGs are cached by content hash in ~/.cache/mermaid-pandoc/, so a
-- preview refresh only re-renders diagrams that changed.

local MMDC = '/opt/homebrew/bin/mmdc'
local CACHE = os.getenv('HOME') .. '/.cache/mermaid-pandoc'

local function read_file(path)
  local f = io.open(path, 'r')
  if not f then return nil end
  local s = f:read('a')
  f:close()
  return s
end

function CodeBlock(el)
  if not el.classes:includes('mermaid') then return nil end
  local hash = pandoc.utils.sha1(el.text)
  local svg_path = CACHE .. '/' .. hash .. '.svg'
  local svg = read_file(svg_path)
  if not svg then
    os.execute(string.format('mkdir -p %q', CACHE))
    local mmd_path = os.tmpname() .. '.mmd'
    local w = io.open(mmd_path, 'w')
    w:write(el.text)
    w:close()
    -- Unique svg id per diagram so several inline SVGs cannot collide.
    os.execute(string.format('%q -q -b white -I m%s -i %q -o %q >/dev/null 2>&1',
                             MMDC, hash:sub(1, 8), mmd_path, svg_path))
    os.remove(mmd_path)
    svg = read_file(svg_path)
  end
  if not svg then
    return nil -- mmdc failed: keep the code block visible instead of losing it
  end
  -- Drop anything before the <svg> root (XML declaration etc.).
  svg = svg:gsub('^.-(<svg)', '%1', 1)
  return pandoc.RawBlock('html', '<div class="mermaid-diagram">' .. svg .. '</div>')
end
