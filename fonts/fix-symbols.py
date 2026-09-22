# Transplant all symbol glyphs from Iosevka Nerd Font Mono into the
# patched AeonikMono Nerd Font Mono files. Aeonik Mono places symbols at
# inconsistent heights and lacks many; Iosevka has the same metrics
# (1000 em, 800/200 ascent/descent), very wide symbol coverage, and its
# glyphs drop in cleanly (centered from the 500 advance into 620).
#
# Rules, for each codepoint that Iosevka NFM contains:
#   - copy it when Aeonik lacks it (fills Cyrillic, shapes, ...)
#   - replace it when it is a symbol or number form (S*, No categories)
#   - keep Aeonik's letters, marks and prose punctuation
#   - skip the Nerd Font PUA icons (identical patcher output in both)
#   - skip box drawing and block elements Aeonik has (must fill the cell)
#
# Run after the Nerd Fonts patcher: fontforge -script fonts/fix-symbols.py
# Iosevka Nerd Font Mono must be installed in ~/Library/Fonts.
import fontforge, psMat, os, unicodedata

HOME = os.path.expanduser("~")
FONTS = os.path.join(HOME, ".emacs.d", "fonts")


def want(cp, in_dst):
    if cp < 0x00A0:
        return False
    if 0xE000 <= cp <= 0xF8FF or cp >= 0xF0000:
        return False
    if 0x2500 <= cp <= 0x259F and in_dst:
        return False
    if not in_dst:
        return True
    cat = unicodedata.category(chr(cp))
    return cat.startswith("S") or cat == "No"


for w in ["Light", "Regular", "Medium", "Bold"]:
    src = fontforge.open(os.path.join(HOME, "Library", "Fonts", f"IosevkaNerdFontMono-{w}.ttf"))
    dst_path = os.path.join(FONTS, f"AeonikMonoNerdFontMono-{w}.otf")
    dst = fontforge.open(dst_path)
    assert src.em == dst.em == 1000

    cps = set()
    for g in src.glyphs():
        if g.unicode > 0:
            cps.add(g.unicode)
        if g.altuni:
            cps.update(u for u, *_ in g.altuni if u > 0)

    n = 0
    for cp in sorted(cps):
        if not want(cp, cp in dst):
            continue
        src[cp].unlinkRef()
        src.selection.select(("unicode",), cp)
        src.copy()
        dst.selection.select(("unicode",), cp)
        dst.paste()
        g = dst[cp]
        # Center the 500 wide Iosevka glyph in the 620 wide Aeonik advance.
        shift = (620 - g.width) / 2
        if shift:
            g.transform(psMat.translate(shift, 0))
        g.width = 620
        n += 1
    dst.generate(dst_path)
    print(f"{w}: {n} glyphs transplanted")
    src.close()
    dst.close()
