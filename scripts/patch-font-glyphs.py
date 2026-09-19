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


def fit_scale(base, ymin, ymax, xmin, xmax, asc, desc, adv):
    """Shrink BASE until the scaled bbox fits the target line box."""
    s = base
    if ymax > 0:
        s = min(s, asc / ymax)
    if ymin < 0:
        s = min(s, desc / -ymin)
    if xmax - xmin > 0:
        s = min(s, adv / (xmax - xmin))
    return s


def patch(target_path, donor_path):
    target = fontforge.open(target_path)
    donor = fontforge.open(donor_path)
    adv = target[ord("x")].width
    # Leave a small margin so ink never touches the cell edge.
    asc, desc = target.ascent - 10, target.descent - 5
    base = adv / donor[ord("x")].width
    # The braille block must share one scale, or the dot grid drifts
    # between spinner frames. Fit the block's total extent.
    bxmin, bymin, bxmax, bymax = donor[0x28FF].boundingBox()
    s_braille = fit_scale(base, bymin, bymax, bxmin, bxmax, asc, desc, adv)
    copied = []
    # Overwrite candidates already present: the unpatched Aeonik has
    # none of them, so any existing one is a previous transplant.
    for cp in CANDIDATES:
        if not has_glyph(donor, cp):
            continue
        xmin, ymin, xmax, ymax = donor[cp].boundingBox()
        if 0x2800 <= cp <= 0x28FF:
            s = s_braille
        else:
            s = fit_scale(base, ymin, ymax, xmin, xmax, asc, desc, adv)
        donor.selection.select(("unicode",), cp)
        donor.copy()
        target.createChar(cp)
        target.selection.select(("unicode",), cp)
        target.paste()
        glyph = target[cp]
        glyph.transform(psMat.scale(s))
        # Center the ink in the cell.
        glyph.transform(psMat.translate(adv / 2 - s * (xmin + xmax) / 2, 0))
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
