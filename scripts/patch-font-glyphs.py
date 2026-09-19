# Bake terminal-UI glyphs into the AeonikMono Nerd Font Mono OTFs.
#
# Claude Code and Codex draw status glyphs (U+23FA, spinner stars,
# U+23BF, braille spinners) that the patched Aeonik does not contain.
# This script copies the missing glyphs from JuliaMono (SIL OFL),
# scales them to the Aeonik advance width, and rewrites the OTFs in
# place. The zip in fonts/ keeps the unpatched originals.
#
# Run with fontforge:
#   fontforge -lang=py scripts/patch-font-glyphs.py <juliamono-dir> [fonts-dir]

import os
import sys

import fontforge
import psMat

WEIGHTS = {
    "AeonikMonoNerdFontMono-Light.otf": "JuliaMono-Light.ttf",
    "AeonikMonoNerdFontMono-Regular.otf": "JuliaMono-Regular.ttf",
    "AeonikMonoNerdFontMono-Medium.otf": "JuliaMono-Medium.ttf",
    "AeonikMonoNerdFontMono-Bold.otf": "JuliaMono-Bold.ttf",
}

# Candidate codepoints; only the ones missing from Aeonik are copied.
CANDIDATES = (
    [
        0x21B3,  # down-right arrow
        0x23BF,  # tool-result elbow
        0x23F5, 0x23F6, 0x23F7, 0x23F8,  # play/pause triangles
        0x23FA,  # record circle (Claude bullet)
        0x25AA, 0x25AB,  # small squares
        0x25B8, 0x25BE,  # small triangles
        0x25C9, 0x25CB, 0x25CE, 0x25CF,  # circles
        0x2714, 0x2717, 0x2718,  # check and ballot marks
        0x2722, 0x2726, 0x2727, 0x2733, 0x2736, 0x273B, 0x273D,  # spinner stars
    ]
    + list(range(0x2800, 0x2900))  # braille block (spinners)
)


def has_glyph(font, cp):
    return cp in font and font[cp].isWorthOutputting()


def patch(target_path, donor_path):
    target = fontforge.open(target_path)
    donor = fontforge.open(donor_path)
    adv = target[ord("x")].width
    scale = psMat.scale(adv / donor[ord("x")].width)
    copied = []
    for cp in CANDIDATES:
        if has_glyph(target, cp) or not has_glyph(donor, cp):
            continue
        donor.selection.select(("unicode",), cp)
        donor.copy()
        target.createChar(cp)
        target.selection.select(("unicode",), cp)
        target.paste()
        glyph = target[cp]
        glyph.transform(scale)
        glyph.width = adv
        copied.append(cp)
    target.generate(target_path)
    target.close()
    donor.close()
    return copied


def main():
    donor_dir = sys.argv[1]
    fonts_dir = sys.argv[2] if len(sys.argv) > 2 else os.path.join(
        os.path.dirname(os.path.dirname(os.path.abspath(__file__))), "fonts")
    for target_name, donor_name in WEIGHTS.items():
        copied = patch(os.path.join(fonts_dir, target_name),
                       os.path.join(donor_dir, donor_name))
        print(f"{target_name}: copied {len(copied)} glyphs")


main()
