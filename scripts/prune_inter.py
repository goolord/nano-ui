# /// script
# dependencies = ["fonttools"]
# ///

from fontTools import subset
from fontTools.ttLib import TTFont

# Printable ASCII only. No box drawing, icons, or extended symbols.
BASIC_UNICODE = list(range(0x20, 0x7F))

# The browser backend's fallback font: what the ASCII subset lacks and a
# page has no system font for. Latin beyond ASCII, Greek, Cyrillic,
# punctuation (the ellipsis, dashes, quotes), currency, arrows, maths and
# common symbols. Precomposed letters only: without the positioning tables
# combining marks would not sit right.
WEB_RANGES = [
    (0x20, 0x7E),      # ASCII, so the file stands on its own
    (0xA0, 0x24F),     # Latin-1 Supplement, Latin Extended-A and -B
    (0x2B0, 0x2FF),    # spacing modifier letters
    (0x370, 0x3FF),    # Greek
    (0x400, 0x52F),    # Cyrillic and its supplement
    (0x1E00, 0x1EFF),  # Latin Extended Additional
    (0x2000, 0x206F),  # General Punctuation
    (0x2070, 0x20CF),  # super- and subscripts, currency
    (0x2100, 0x218F),  # letterlike symbols, number forms
    (0x2190, 0x23FF),  # arrows, mathematical operators, technical
    (0x2500, 0x27BF),  # box drawing, shapes, symbols, dingbats
]
WEB_UNICODE = [c for lo, hi in WEB_RANGES for c in range(lo, hi + 1)]


def main():
    import sys
    from pathlib import Path

    args = sys.argv[1:]
    web = args[:1] == ["--web"]
    if web:
        args = args[1:]
    if len(args) != 1:
        sys.exit("usage: prune_inter.py [--web] path/to/Inter.ttf")
    root = Path(__file__).resolve().parent.parent
    src = Path(args[0])
    if web:
        out = root / "packages" / "nano-ui-sdl-web" / "web" / "fonts" / "inter-extended.ttf"
    else:
        out = root / "packages" / "nano-ui-ttf" / "data" / "inter.ttf"

    options = subset.Options()
    options.drop_tables += [
        "DSIG",
        "hdmx",
        "LTSH",
        "VDMX",
        "vmtx",
        "vhea",
        "PCLT",
        "FFTM",
        "MATH",
        "GDEF",
        "GPOS",
        "GSUB",
        "kern",
    ]
    # Keep the full name table, the licence records (IDs 13 and 14) among
    # it. Filtering name_IDs can drop every record and produces a TTF
    # Windows and SDL_ttf reject.
    options.name_IDs = ["*"]

    font = subset.load_font(str(src), options)
    subsetter = subset.Subsetter(options)
    subsetter.populate(unicodes=WEB_UNICODE if web else BASIC_UNICODE)
    subsetter.subset(font)
    subset.save_font(font, str(out), options)

    check = TTFont(str(out))
    cmap = next(t.cmap for t in check["cmap"].tables)
    print(f"Generated {out} ({len(check.getGlyphOrder())} glyphs, {len(cmap)} codepoints, {out.stat().st_size} bytes)")


if __name__ == "__main__":
    main()
