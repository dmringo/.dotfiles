#!/usr/bin/env python3
"""List Windows system monospace fonts using standard library ctypes (Win32 GDI).

Zero external dependencies (pure Python standard library).
Enumeration runtime: ~3ms (process total ~80-100ms).
"""

import ctypes
from ctypes import wintypes
import sys

if sys.platform != "win32":
    print("This script is for Windows only.", file=sys.stderr)
    sys.exit(1)


class LOGFONTW(ctypes.Structure):
    _fields_ = [
        ("lfHeight", wintypes.LONG),
        ("lfWidth", wintypes.LONG),
        ("lfEscapement", wintypes.LONG),
        ("lfOrientation", wintypes.LONG),
        ("lfWeight", wintypes.LONG),
        ("lfItalic", wintypes.BYTE),
        ("lfUnderline", wintypes.BYTE),
        ("lfStrikeOut", wintypes.BYTE),
        ("lfCharSet", wintypes.BYTE),
        ("lfOutPrecision", wintypes.BYTE),
        ("lfClipPrecision", wintypes.BYTE),
        ("lfQuality", wintypes.BYTE),
        ("lfPitchAndFamily", wintypes.BYTE),
        ("lfFaceName", wintypes.WCHAR * 32),
    ]


class TEXTMETRICW(ctypes.Structure):
    _fields_ = [
        ("tmHeight", wintypes.LONG),
        ("tmAscent", wintypes.LONG),
        ("tmDescent", wintypes.LONG),
        ("tmInternalLeading", wintypes.LONG),
        ("tmExternalLeading", wintypes.LONG),
        ("tmAveCharWidth", wintypes.LONG),
        ("tmMaxCharWidth", wintypes.LONG),
        ("tmWeight", wintypes.LONG),
        ("tmOverhang", wintypes.LONG),
        ("tmDigitizedAspectX", wintypes.LONG),
        ("tmDigitizedAspectY", wintypes.LONG),
        ("tmFirstChar", wintypes.WCHAR),
        ("tmLastChar", wintypes.WCHAR),
        ("tmDefaultChar", wintypes.WCHAR),
        ("tmBreakChar", wintypes.WCHAR),
        ("tmItalic", wintypes.BYTE),
        ("tmUnderlined", wintypes.BYTE),
        ("tmStrikeOut", wintypes.BYTE),
        ("tmPitchAndFamily", wintypes.BYTE),
        ("tmCharSet", wintypes.BYTE),
    ]


TMPF_FIXED_PITCH = 0x01
DEFAULT_CHARSET = 1

FONTENUMPROC = ctypes.WINFUNCTYPE(
    ctypes.c_int,
    ctypes.POINTER(LOGFONTW),
    ctypes.POINTER(TEXTMETRICW),
    wintypes.DWORD,
    wintypes.LPARAM,
)

SUFFIXES = (
    " ExtraLight",
    " Light",
    " SemiLight",
    " SemiBold",
    " ExtraBold",
    " Bold",
    " Medium",
    " Black",
)


def normalize(name: str) -> str:
    for s in SUFFIXES:
        if name.endswith(s):
            return name[: -len(s)]
    return name


def get_monospace_fonts() -> list[str]:
    user32 = ctypes.windll.user32
    gdi32 = ctypes.windll.gdi32

    seen = set()
    fonts = []

    def enum_font_proc(lpelfe, lpntme, font_type, lparam):
        lf = lpelfe.contents
        tm = lpntme.contents
        # In Win32 GDI, (tmPitchAndFamily & TMPF_FIXED_PITCH) == 0 means fixed-pitch
        if (tm.tmPitchAndFamily & TMPF_FIXED_PITCH) == 0:
            name = lf.lfFaceName
            if not name.startswith("@"):
                family = normalize(name)
                if family not in seen:
                    seen.add(family)
                    fonts.append(family)
        return 1

    cb = FONTENUMPROC(enum_font_proc)

    hdc = user32.GetDC(None)
    if not hdc:
        return []

    try:
        lf = LOGFONTW()
        lf.lfCharSet = DEFAULT_CHARSET
        gdi32.EnumFontFamiliesExW(hdc, ctypes.byref(lf), cb, 0, 0)
    finally:
        user32.ReleaseDC(None, hdc)

    fonts.sort()
    return fonts


if __name__ == "__main__":
    for font in get_monospace_fonts():
        print(font)
