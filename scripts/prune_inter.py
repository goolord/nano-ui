# /// script
# dependencies = ["fonttools"]
# ///

from fontTools import subset
from fontTools.ttLib import TTFont

# Printable ASCII only. No box drawing, icons, or extended symbols.
BASIC_UNICODE = list(range(0x20, 0x7F))


def main():
    import sys
    from pathlib import Path

    root = Path(__file__).resolve().parent.parent
    src = Path(sys.argv[1]) if len(sys.argv) > 1 else Path(r"C:\Users\zach\Downloads\Inter.ttf")
    out = root / "packages" / "nano-ui-sdl" / "data" / "inter.ttf"

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
    # Keep the full name table. Filtering name_IDs can drop every record
    # and produces a TTF Windows and SDL_ttf reject.

    font = subset.load_font(str(src), options)
    subsetter = subset.Subsetter(options)
    subsetter.populate(unicodes=BASIC_UNICODE)
    subsetter.subset(font)
    subset.save_font(font, str(out), options)

    check = TTFont(str(out))
    cmap = next(t.cmap for t in check["cmap"].tables)
    print(f"Generated {out} ({len(check.getGlyphOrder())} glyphs, {len(cmap)} codepoints, {out.stat().st_size} bytes)")


if __name__ == "__main__":
    main()
