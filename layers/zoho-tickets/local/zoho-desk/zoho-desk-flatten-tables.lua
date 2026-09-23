-- Flatten HTML-email layout tables into a run of ordinary blocks.
--
-- Email signatures (and much other mail HTML) use <table> purely for
-- layout: a logo cell beside a name/title cell, no header row.  Pandoc
-- renders those as literal org tables, which is unreadable — the image
-- link swells one cell to hundreds of columns, and <br>s inside cells
-- become mid-line \\ that the org side cannot strip.  A table is
-- treated as layout, and its cells emitted as normal stacked blocks,
-- when it contains an image or has no header and at most one row.
-- Genuine data tables (multiple rows or a header) stay tables.
--
-- Org also has no syntax for a link whose description is an image
-- ([[url][[[img]]]] does not parse), so image-only links are unwrapped
-- to just their image.
--
-- Two smaller mail-HTML cleanups ride along: icon-font glyphs
-- (private-use codepoints that only render with fonts like
-- FluentSystemIcons) are dropped, along with the paragraphs holding
-- nothing else, and hard breaks are moved out of bold/italic/underline
-- spans — org cannot break an emphasis across lines, so pandoc would
-- otherwise leave an opening * stranded on each line.

local function rows_are_empty(rows)
  for _, row in ipairs(rows) do
    for _, cell in ipairs(row.cells) do
      if #cell.contents > 0 then
        return false
      end
    end
  end
  return true
end

local function collect_rows(rows, blocks)
  for _, row in ipairs(rows) do
    for _, cell in ipairs(row.cells) do
      blocks:extend(cell.contents)
    end
  end
end

function Table(tbl)
  local has_image = false
  tbl:walk({ Image = function() has_image = true end })
  local nrows = 0
  for _, body in ipairs(tbl.bodies) do
    nrows = nrows + #body.head + #body.body
  end
  if not (has_image or (rows_are_empty(tbl.head.rows) and nrows <= 1)) then
    return nil
  end
  local cells = pandoc.Blocks({})
  collect_rows(tbl.head.rows, cells)
  for _, body in ipairs(tbl.bodies) do
    collect_rows(body.head, cells)
    collect_rows(body.body, cells)
  end
  collect_rows(tbl.foot.rows, cells)
  -- One paragraph with hard breaks, not stacked blocks: the org side
  -- strips the trailing \\ these become, so the signature lines sit
  -- directly under one another without blank lines between.
  if #cells == 0 then
    return pandoc.Blocks({})
  end
  local inlines = pandoc.utils.blocks_to_inlines(cells,
                                                 { pandoc.LineBreak() })
  -- Signature tables draw a rule over themselves (border-style:
  -- solid none none).  Org's own ----- rule spans the whole window,
  -- so draw a fixed-width line of box-drawing characters instead —
  -- plain text to org, as wide as the signature.
  local style = tbl.attr.attributes["style"] or ""
  if style:find("border%-style:%s*solid") then
    inlines:insert(1, pandoc.LineBreak())
    inlines:insert(1, pandoc.Str(string.rep(utf8.char(0x2500), 28)))
  end
  return pandoc.Para(inlines)
end

function Link(link)
  if #link.content == 1 and link.content[1].t == "Image" then
    return link.content[1]
  end
end

function Str(str)
  -- Mail HTML leans on &nbsp; for spacing; Emacs underlines the
  -- no-break spaces (nobreak-char-display) and the org side's
  -- trailing-whitespace cleanup does not match them, so make them
  -- plain spaces.
  local text = str.text:gsub("\194\160", " ")
  if text:find("^ *$") then
    return text == "" and {} or pandoc.Space()
  end
  local private_use = true
  for _, cp in utf8.codes(text) do
    if cp < 0xE000 or cp > 0xF8FF then
      private_use = false
      break
    end
  end
  if private_use then
    return {}
  end
  if text ~= str.text then
    return pandoc.Str(text)
  end
end

local function trim_trailing_breaks(el)
  local moved = pandoc.Inlines({})
  while #el.content > 0 do
    local last = el.content[#el.content].t
    if last ~= "LineBreak" and last ~= "SoftBreak" and last ~= "Space" then
      break
    end
    moved:insert(1, el.content:remove(#el.content))
  end
  if #moved == 0 then
    return nil
  elseif #el.content == 0 then
    return moved
  end
  moved:insert(1, el)
  return moved
end

Strong = trim_trailing_breaks
Emph = trim_trailing_breaks
Underline = trim_trailing_breaks

local function drop_when_empty(block)
  if #block.content == 0 then
    return {}
  end
end

Para = drop_when_empty
Plain = drop_when_empty
