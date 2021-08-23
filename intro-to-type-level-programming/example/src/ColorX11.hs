{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
module ColorX11 where
import Color

data AliceBlue = AliceBlue
instance IsColor AliceBlue where
  toRGB = const $ RGB 0xF0 0xF8 0xFF

instance NamedColor AliceBlue where
  type ColorName _ = "AliceBlue"

data AntiqueWhite = AntiqueWhite
instance IsColor AntiqueWhite where
  toRGB = const $ RGB 0xFA 0xEB 0xD7

instance NamedColor AntiqueWhite where
  type ColorName AntiqueWhite = "AntiqueWhite"

data Aqua = Aqua
instance IsColor Aqua where
  toRGB = const $ RGB 0x00 0xFF 0xFF

instance NamedColor Aqua where
  type ColorName Aqua = "Aqua"

data Aquamarine = Aquamarine
instance IsColor Aquamarine where
  toRGB = const $ RGB 0x7F 0xFF 0xD4

instance NamedColor Aquamarine where
  type ColorName Aquamarine = "Aquamarine"

data Azure = Azure
instance IsColor Azure where
  toRGB = const $ RGB 0xF0 0xFF 0xFF

instance NamedColor Azure where
  type ColorName Azure = "Azure"

data Beige = Beige
instance IsColor Beige where
  toRGB = const $ RGB 0xF5 0xF5 0xDC

instance NamedColor Beige where
  type ColorName Beige = "Beige"

data Bisque = Bisque
instance IsColor Bisque where
  toRGB = const $ RGB 0xFF 0xE4 0xC4

instance NamedColor Bisque where
  type ColorName Bisque = "Bisque"

data Black = Black
instance IsColor Black where
  toRGB = const $ RGB 0x00 0x00 0x00

instance NamedColor Black where
  type ColorName Black = "Black"

data BlanchedAlmond = BlanchedAlmond
instance IsColor BlanchedAlmond where
  toRGB = const $ RGB 0xFF 0xEB 0xCD

instance NamedColor BlanchedAlmond where
  type ColorName BlanchedAlmond = "BlanchedAlmond"

data Blue = Blue
instance IsColor Blue where
  toRGB = const $ RGB 0x00 0x00 0xFF

instance NamedColor Blue where
  type ColorName Blue = "Blue"

data BlueViolet = BlueViolet
instance IsColor BlueViolet where
  toRGB = const $ RGB 0x8A 0x2B 0xE2

instance NamedColor BlueViolet where
  type ColorName BlueViolet = "BlueViolet"

data Brown = Brown
instance IsColor Brown where
  toRGB = const $ RGB 0xA5 0x2A 0x2A

instance NamedColor Brown where
  type ColorName Brown = "Brown"

data Burlywood = Burlywood
instance IsColor Burlywood where
  toRGB = const $ RGB 0xDE 0xB8 0x87

instance NamedColor Burlywood where
  type ColorName Burlywood = "Burlywood"

data CadetBlue = CadetBlue
instance IsColor CadetBlue where
  toRGB = const $ RGB 0x5F 0x9E 0xA0

instance NamedColor CadetBlue where
  type ColorName CadetBlue = "CadetBlue"

data Chartreuse = Chartreuse
instance IsColor Chartreuse where
  toRGB = const $ RGB 0x7F 0xFF 0x00

instance NamedColor Chartreuse where
  type ColorName Chartreuse = "Chartreuse"

data Chocolate = Chocolate
instance IsColor Chocolate where
  toRGB = const $ RGB 0xD2 0x69 0x1E

instance NamedColor Chocolate where
  type ColorName Chocolate = "Chocolate"

data Coral = Coral
instance IsColor Coral where
  toRGB = const $ RGB 0xFF 0x7F 0x50

instance NamedColor Coral where
  type ColorName Coral = "Coral"

data CornflowerBlue = CornflowerBlue
instance IsColor CornflowerBlue where
  toRGB = const $ RGB 0x64 0x95 0xED

instance NamedColor CornflowerBlue where
  type ColorName CornflowerBlue = "CornflowerBlue"

data Cornsilk = Cornsilk
instance IsColor Cornsilk where
  toRGB = const $ RGB 0xFF 0xF8 0xDC

instance NamedColor Cornsilk where
  type ColorName Cornsilk = "Cornsilk"

data Crimson = Crimson
instance IsColor Crimson where
  toRGB = const $ RGB 0xDC 0x14 0x3C

instance NamedColor Crimson where
  type ColorName Crimson = "Crimson"

data Cyan = Cyan
instance IsColor Cyan where
  toRGB = const $ RGB 0x00 0xFF 0xFF

instance NamedColor Cyan where
  type ColorName Cyan = "Cyan"

data DarkBlue = DarkBlue
instance IsColor DarkBlue where
  toRGB = const $ RGB 0x00 0x00 0x8B

instance NamedColor DarkBlue where
  type ColorName DarkBlue = "DarkBlue"

data DarkCyan = DarkCyan
instance IsColor DarkCyan where
  toRGB = const $ RGB 0x00 0x8B 0x8B

instance NamedColor DarkCyan where
  type ColorName DarkCyan = "DarkCyan"

data DarkGoldenrod = DarkGoldenrod
instance IsColor DarkGoldenrod where
  toRGB = const $ RGB 0xB8 0x86 0x0B

instance NamedColor DarkGoldenrod where
  type ColorName DarkGoldenrod = "DarkGoldenrod"

data DarkGray = DarkGray
instance IsColor DarkGray where
  toRGB = const $ RGB 0xA9 0xA9 0xA9

instance NamedColor DarkGray where
  type ColorName DarkGray = "DarkGray"

data DarkGreen = DarkGreen
instance IsColor DarkGreen where
  toRGB = const $ RGB 0x00 0x64 0x00

instance NamedColor DarkGreen where
  type ColorName DarkGreen = "DarkGreen"

data DarkKhaki = DarkKhaki
instance IsColor DarkKhaki where
  toRGB = const $ RGB 0xBD 0xB7 0x6B

instance NamedColor DarkKhaki where
  type ColorName DarkKhaki = "DarkKhaki"

data DarkMagenta = DarkMagenta
instance IsColor DarkMagenta where
  toRGB = const $ RGB 0x8B 0x00 0x8B

instance NamedColor DarkMagenta where
  type ColorName DarkMagenta = "DarkMagenta"

data DarkOlive = DarkOlive
instance IsColor DarkOlive where
  toRGB = const $ RGB 0x55 0x6B 0x2F

instance NamedColor DarkOlive where
  type ColorName DarkOlive = "DarkOlive"

data DarkOrange = DarkOrange
instance IsColor DarkOrange where
  toRGB = const $ RGB 0xFF 0x8C 0x00

instance NamedColor DarkOrange where
  type ColorName DarkOrange = "DarkOrange"

data DarkOrchid = DarkOrchid
instance IsColor DarkOrchid where
  toRGB = const $ RGB 0x99 0x32 0xCC

instance NamedColor DarkOrchid where
  type ColorName DarkOrchid = "DarkOrchid"

data DarkRed = DarkRed
instance IsColor DarkRed where
  toRGB = const $ RGB 0x8B 0x00 0x00

instance NamedColor DarkRed where
  type ColorName DarkRed = "DarkRed"

data DarkSalmon = DarkSalmon
instance IsColor DarkSalmon where
  toRGB = const $ RGB 0xE9 0x96 0x7A

instance NamedColor DarkSalmon where
  type ColorName DarkSalmon = "DarkSalmon"

data DarkSea = DarkSea
instance IsColor DarkSea where
  toRGB = const $ RGB 0x8F 0xBC 0x8F

instance NamedColor DarkSea where
  type ColorName DarkSea = "DarkSea"

data DarkSlateBlue = DarkSlateBlue
instance IsColor DarkSlateBlue where
  toRGB = const $ RGB 0x48 0x3D 0x8B

instance NamedColor DarkSlateBlue where
  type ColorName DarkSlateBlue = "DarkSlateBlue"

data DarkSlateGray = DarkSlateGray
instance IsColor DarkSlateGray where
  toRGB = const $ RGB 0x2F 0x4F 0x4F

instance NamedColor DarkSlateGray where
  type ColorName DarkSlateGray = "DarkSlateGray"

data DarkTurquoise = DarkTurquoise
instance IsColor DarkTurquoise where
  toRGB = const $ RGB 0x00 0xCE 0xD1

instance NamedColor DarkTurquoise where
  type ColorName DarkTurquoise = "DarkTurquoise"

data DarkViolet = DarkViolet
instance IsColor DarkViolet where
  toRGB = const $ RGB 0x94 0x00 0xD3

instance NamedColor DarkViolet where
  type ColorName DarkViolet = "DarkViolet"

data DeepPink = DeepPink
instance IsColor DeepPink where
  toRGB = const $ RGB 0xFF 0x14 0x93

instance NamedColor DeepPink where
  type ColorName DeepPink = "DeepPink"

data DeepSkyBlue = DeepSkyBlue
instance IsColor DeepSkyBlue where
  toRGB = const $ RGB 0x00 0xBF 0xFF

instance NamedColor DeepSkyBlue where
  type ColorName DeepSkyBlue = "DeepSkyBlue"

data DimGray = DimGray
instance IsColor DimGray where
  toRGB = const $ RGB 0x69 0x69 0x69

instance NamedColor DimGray where
  type ColorName DimGray = "DimGray"

data DodgerBlue = DodgerBlue
instance IsColor DodgerBlue where
  toRGB = const $ RGB 0x1E 0x90 0xFF

instance NamedColor DodgerBlue where
  type ColorName DodgerBlue = "DodgerBlue"

data Firebrick = Firebrick
instance IsColor Firebrick where
  toRGB = const $ RGB 0xB2 0x22 0x22

instance NamedColor Firebrick where
  type ColorName Firebrick = "Firebrick"

data FloralWhite = FloralWhite
instance IsColor FloralWhite where
  toRGB = const $ RGB 0xFF 0xFA 0xF0

instance NamedColor FloralWhite where
  type ColorName FloralWhite = "FloralWhite"

data ForestGreen = ForestGreen
instance IsColor ForestGreen where
  toRGB = const $ RGB 0x22 0x8B 0x22

instance NamedColor ForestGreen where
  type ColorName ForestGreen = "ForestGreen"

data Fuchsia = Fuchsia
instance IsColor Fuchsia where
  toRGB = const $ RGB 0xFF 0x00 0xFF

instance NamedColor Fuchsia where
  type ColorName Fuchsia = "Fuchsia"

data Gainsboro = Gainsboro
instance IsColor Gainsboro where
  toRGB = const $ RGB 0xDC 0xDC 0xDC

instance NamedColor Gainsboro where
  type ColorName Gainsboro = "Gainsboro"

data GhostWhite = GhostWhite
instance IsColor GhostWhite where
  toRGB = const $ RGB 0xF8 0xF8 0xFF

instance NamedColor GhostWhite where
  type ColorName GhostWhite = "GhostWhite"

data Gold = Gold
instance IsColor Gold where
  toRGB = const $ RGB 0xFF 0xD7 0x00

instance NamedColor Gold where
  type ColorName Gold = "Gold"

data Goldenrod = Goldenrod
instance IsColor Goldenrod where
  toRGB = const $ RGB 0xDA 0xA5 0x20

instance NamedColor Goldenrod where
  type ColorName Goldenrod = "Goldenrod"

data Gray = Gray
instance IsColor Gray where
  toRGB = const $ RGB 0xBE 0xBE 0xBE

instance NamedColor Gray where
  type ColorName Gray = "Gray"

data WebGray = WebGray
instance IsColor WebGray where
  toRGB = const $ RGB 0x80 0x80 0x80

instance NamedColor WebGray where
  type ColorName WebGray = "WebGray"

data Green = Green
instance IsColor Green where
  toRGB = const $ RGB 0x00 0xFF 0x00

instance NamedColor Green where
  type ColorName Green = "Green"

data WebGreen = WebGreen
instance IsColor WebGreen where
  toRGB = const $ RGB 0x00 0x80 0x00

instance NamedColor WebGreen where
  type ColorName WebGreen = "WebGreen"

data GreenYellow = GreenYellow
instance IsColor GreenYellow where
  toRGB = const $ RGB 0xAD 0xFF 0x2F

instance NamedColor GreenYellow where
  type ColorName GreenYellow = "GreenYellow"

data Honeydew = Honeydew
instance IsColor Honeydew where
  toRGB = const $ RGB 0xF0 0xFF 0xF0

instance NamedColor Honeydew where
  type ColorName Honeydew = "Honeydew"

data HotPink = HotPink
instance IsColor HotPink where
  toRGB = const $ RGB 0xFF 0x69 0xB4

instance NamedColor HotPink where
  type ColorName HotPink = "HotPink"

data Indian = Indian
instance IsColor Indian where
  toRGB = const $ RGB 0xCD 0x5C 0x5C

instance NamedColor Indian where
  type ColorName Indian = "Indian"

data Indigo = Indigo
instance IsColor Indigo where
  toRGB = const $ RGB 0x4B 0x00 0x82

instance NamedColor Indigo where
  type ColorName Indigo = "Indigo"

data Ivory = Ivory
instance IsColor Ivory where
  toRGB = const $ RGB 0xFF 0xFF 0xF0

instance NamedColor Ivory where
  type ColorName Ivory = "Ivory"

data Khaki = Khaki
instance IsColor Khaki where
  toRGB = const $ RGB 0xF0 0xE6 0x8C

instance NamedColor Khaki where
  type ColorName Khaki = "Khaki"

data Lavender = Lavender
instance IsColor Lavender where
  toRGB = const $ RGB 0xE6 0xE6 0xFA

instance NamedColor Lavender where
  type ColorName Lavender = "Lavender"

data LavenderBlush = LavenderBlush
instance IsColor LavenderBlush where
  toRGB = const $ RGB 0xFF 0xF0 0xF5

instance NamedColor LavenderBlush where
  type ColorName LavenderBlush = "LavenderBlush"

data LawnGreen = LawnGreen
instance IsColor LawnGreen where
  toRGB = const $ RGB 0x7C 0xFC 0x00

instance NamedColor LawnGreen where
  type ColorName LawnGreen = "LawnGreen"

data LemonChiffon = LemonChiffon
instance IsColor LemonChiffon where
  toRGB = const $ RGB 0xFF 0xFA 0xCD

instance NamedColor LemonChiffon where
  type ColorName LemonChiffon = "LemonChiffon"

data LightBlue = LightBlue
instance IsColor LightBlue where
  toRGB = const $ RGB 0xAD 0xD8 0xE6

instance NamedColor LightBlue where
  type ColorName LightBlue = "LightBlue"

data LightCoral = LightCoral
instance IsColor LightCoral where
  toRGB = const $ RGB 0xF0 0x80 0x80

instance NamedColor LightCoral where
  type ColorName LightCoral = "LightCoral"

data LightCyan = LightCyan
instance IsColor LightCyan where
  toRGB = const $ RGB 0xE0 0xFF 0xFF

instance NamedColor LightCyan where
  type ColorName LightCyan = "LightCyan"

data LightGoldenrod = LightGoldenrod
instance IsColor LightGoldenrod where
  toRGB = const $ RGB 0xFA 0xFA 0xD2

instance NamedColor LightGoldenrod where
  type ColorName LightGoldenrod = "LightGoldenrod"

data LightGray = LightGray
instance IsColor LightGray where
  toRGB = const $ RGB 0xD3 0xD3 0xD3

instance NamedColor LightGray where
  type ColorName LightGray = "LightGray"

data LightGreen = LightGreen
instance IsColor LightGreen where
  toRGB = const $ RGB 0x90 0xEE 0x90

instance NamedColor LightGreen where
  type ColorName LightGreen = "LightGreen"

data LightPink = LightPink
instance IsColor LightPink where
  toRGB = const $ RGB 0xFF 0xB6 0xC1

instance NamedColor LightPink where
  type ColorName LightPink = "LightPink"

data LightSalmon = LightSalmon
instance IsColor LightSalmon where
  toRGB = const $ RGB 0xFF 0xA0 0x7A

instance NamedColor LightSalmon where
  type ColorName LightSalmon = "LightSalmon"

data LightSea = LightSea
instance IsColor LightSea where
  toRGB = const $ RGB 0x20 0xB2 0xAA

instance NamedColor LightSea where
  type ColorName LightSea = "LightSea"

data LightSky = LightSky
instance IsColor LightSky where
  toRGB = const $ RGB 0x87 0xCE 0xFA

instance NamedColor LightSky where
  type ColorName LightSky = "LightSky"

data LightSlate = LightSlate
instance IsColor LightSlate where
  toRGB = const $ RGB 0x77 0x88 0x99

instance NamedColor LightSlate where
  type ColorName LightSlate = "LightSlate"

data LightSteel = LightSteel
instance IsColor LightSteel where
  toRGB = const $ RGB 0xB0 0xC4 0xDE

instance NamedColor LightSteel where
  type ColorName LightSteel = "LightSteel"

data LightYellow = LightYellow
instance IsColor LightYellow where
  toRGB = const $ RGB 0xFF 0xFF 0xE0

instance NamedColor LightYellow where
  type ColorName LightYellow = "LightYellow"

data Lime = Lime
instance IsColor Lime where
  toRGB = const $ RGB 0x00 0xFF 0x00

instance NamedColor Lime where
  type ColorName Lime = "Lime"

data LimeGreen = LimeGreen
instance IsColor LimeGreen where
  toRGB = const $ RGB 0x32 0xCD 0x32

instance NamedColor LimeGreen where
  type ColorName LimeGreen = "LimeGreen"

data Linen = Linen
instance IsColor Linen where
  toRGB = const $ RGB 0xFA 0xF0 0xE6

instance NamedColor Linen where
  type ColorName Linen = "Linen"

data Magenta = Magenta
instance IsColor Magenta where
  toRGB = const $ RGB 0xFF 0x00 0xFF

instance NamedColor Magenta where
  type ColorName Magenta = "Magenta"

data Maroon = Maroon
instance IsColor Maroon where
  toRGB = const $ RGB 0xB0 0x30 0x60

instance NamedColor Maroon where
  type ColorName Maroon = "Maroon"

data Web = Web
instance IsColor Web where
  toRGB = const $ RGB 0x80 0x00 0x00

instance NamedColor Web where
  type ColorName Web = "Web"

data MediumAquamarine = MediumAquamarine
instance IsColor MediumAquamarine where
  toRGB = const $ RGB 0x66 0xCD 0xAA

instance NamedColor MediumAquamarine where
  type ColorName MediumAquamarine = "MediumAquamarine"

data MediumBlue = MediumBlue
instance IsColor MediumBlue where
  toRGB = const $ RGB 0x00 0x00 0xCD

instance NamedColor MediumBlue where
  type ColorName MediumBlue = "MediumBlue"

data MediumOrchid = MediumOrchid
instance IsColor MediumOrchid where
  toRGB = const $ RGB 0xBA 0x55 0xD3

instance NamedColor MediumOrchid where
  type ColorName MediumOrchid = "MediumOrchid"

data MediumPurple = MediumPurple
instance IsColor MediumPurple where
  toRGB = const $ RGB 0x93 0x70 0xDB

instance NamedColor MediumPurple where
  type ColorName MediumPurple = "MediumPurple"

data MediumSeaGreen = MediumSeaGreen
instance IsColor MediumSeaGreen where
  toRGB = const $ RGB 0x3C 0xB3 0x71

instance NamedColor MediumSeaGreen where
  type ColorName MediumSeaGreen = "MediumSeaGreen"

data MediumSlateBlue = MediumSlateBlue
instance IsColor MediumSlateBlue where
  toRGB = const $ RGB 0x7B 0x68 0xEE

instance NamedColor MediumSlateBlue where
  type ColorName MediumSlateBlue = "MediumSlateBlue"

data MediumSpringGreen = MediumSpringGreen
instance IsColor MediumSpringGreen where
  toRGB = const $ RGB 0x00 0xFA 0x9A

instance NamedColor MediumSpringGreen where
  type ColorName MediumSpringGreen = "MediumSpringGreen"

data MediumTurquoise = MediumTurquoise
instance IsColor MediumTurquoise where
  toRGB = const $ RGB 0x48 0xD1 0xCC

instance NamedColor MediumTurquoise where
  type ColorName MediumTurquoise = "MediumTurquoise"

data MediumVioletRed = MediumVioletRed
instance IsColor MediumVioletRed where
  toRGB = const $ RGB 0xC7 0x15 0x85

instance NamedColor MediumVioletRed where
  type ColorName MediumVioletRed = "MediumVioletRed"

data MidnightBlue = MidnightBlue
instance IsColor MidnightBlue where
  toRGB = const $ RGB 0x19 0x19 0x70

instance NamedColor MidnightBlue where
  type ColorName MidnightBlue = "MidnightBlue"

data MintCream = MintCream
instance IsColor MintCream where
  toRGB = const $ RGB 0xF5 0xFF 0xFA

instance NamedColor MintCream where
  type ColorName MintCream = "MintCream"

data MistyRose = MistyRose
instance IsColor MistyRose where
  toRGB = const $ RGB 0xFF 0xE4 0xE1

instance NamedColor MistyRose where
  type ColorName MistyRose = "MistyRose"

data Moccasin = Moccasin
instance IsColor Moccasin where
  toRGB = const $ RGB 0xFF 0xE4 0xB5

instance NamedColor Moccasin where
  type ColorName Moccasin = "Moccasin"

data NavajoWhite = NavajoWhite
instance IsColor NavajoWhite where
  toRGB = const $ RGB 0xFF 0xDE 0xAD

instance NamedColor NavajoWhite where
  type ColorName NavajoWhite = "NavajoWhite"

data NavyBlue = NavyBlue
instance IsColor NavyBlue where
  toRGB = const $ RGB 0x00 0x00 0x80

instance NamedColor NavyBlue where
  type ColorName NavyBlue = "NavyBlue"

data OldLace = OldLace
instance IsColor OldLace where
  toRGB = const $ RGB 0xFD 0xF5 0xE6

instance NamedColor OldLace where
  type ColorName OldLace = "OldLace"

data Olive = Olive
instance IsColor Olive where
  toRGB = const $ RGB 0x80 0x80 0x00

instance NamedColor Olive where
  type ColorName Olive = "Olive"

data OliveDrab = OliveDrab
instance IsColor OliveDrab where
  toRGB = const $ RGB 0x6B 0x8E 0x23

instance NamedColor OliveDrab where
  type ColorName OliveDrab = "OliveDrab"

data Orange = Orange
instance IsColor Orange where
  toRGB = const $ RGB 0xFF 0xA5 0x00

instance NamedColor Orange where
  type ColorName Orange = "Orange"

data OrangeRed = OrangeRed
instance IsColor OrangeRed where
  toRGB = const $ RGB 0xFF 0x45 0x00

instance NamedColor OrangeRed where
  type ColorName OrangeRed = "OrangeRed"

data Orchid = Orchid
instance IsColor Orchid where
  toRGB = const $ RGB 0xDA 0x70 0xD6

instance NamedColor Orchid where
  type ColorName Orchid = "Orchid"

data PaleGoldenrod = PaleGoldenrod
instance IsColor PaleGoldenrod where
  toRGB = const $ RGB 0xEE 0xE8 0xAA

instance NamedColor PaleGoldenrod where
  type ColorName PaleGoldenrod = "PaleGoldenrod"

data PaleGreen = PaleGreen
instance IsColor PaleGreen where
  toRGB = const $ RGB 0x98 0xFB 0x98

instance NamedColor PaleGreen where
  type ColorName PaleGreen = "PaleGreen"

data PaleTurquoise = PaleTurquoise
instance IsColor PaleTurquoise where
  toRGB = const $ RGB 0xAF 0xEE 0xEE

instance NamedColor PaleTurquoise where
  type ColorName PaleTurquoise = "PaleTurquoise"

data PaleViolet = PaleViolet
instance IsColor PaleViolet where
  toRGB = const $ RGB 0xDB 0x70 0x93

instance NamedColor PaleViolet where
  type ColorName PaleViolet = "PaleViolet"

data PapayaWhip = PapayaWhip
instance IsColor PapayaWhip where
  toRGB = const $ RGB 0xFF 0xEF 0xD5

instance NamedColor PapayaWhip where
  type ColorName PapayaWhip = "PapayaWhip"

data PeachPuff = PeachPuff
instance IsColor PeachPuff where
  toRGB = const $ RGB 0xFF 0xDA 0xB9

instance NamedColor PeachPuff where
  type ColorName PeachPuff = "PeachPuff"

data Peru = Peru
instance IsColor Peru where
  toRGB = const $ RGB 0xCD 0x85 0x3F

instance NamedColor Peru where
  type ColorName Peru = "Peru"

data Pink = Pink
instance IsColor Pink where
  toRGB = const $ RGB 0xFF 0xC0 0xCB

instance NamedColor Pink where
  type ColorName Pink = "Pink"

data Plum = Plum
instance IsColor Plum where
  toRGB = const $ RGB 0xDD 0xA0 0xDD

instance NamedColor Plum where
  type ColorName Plum = "Plum"

data Powder = Powder
instance IsColor Powder where
  toRGB = const $ RGB 0xB0 0xE0 0xE6

instance NamedColor Powder where
  type ColorName Powder = "Powder"

data Purple = Purple
instance IsColor Purple where
  toRGB = const $ RGB 0xA0 0x20 0xF0

instance NamedColor Purple where
  type ColorName Purple = "Purple"

data WebPurple = WebPurple
instance IsColor WebPurple where
  toRGB = const $ RGB 0x80 0x00 0x80

instance NamedColor WebPurple where
  type ColorName WebPurple = "WebPurple"

data RebeccaPurple = RebeccaPurple
instance IsColor RebeccaPurple where
  toRGB = const $ RGB 0x66 0x33 0x99

instance NamedColor RebeccaPurple where
  type ColorName RebeccaPurple = "RebeccaPurple"

data Red = Red
instance IsColor Red where
  toRGB = const $ RGB 0xFF 0x00 0x00

instance NamedColor Red where
  type ColorName Red = "Red"

data RosyBrown = RosyBrown
instance IsColor RosyBrown where
  toRGB = const $ RGB 0xBC 0x8F 0x8F

instance NamedColor RosyBrown where
  type ColorName RosyBrown = "RosyBrown"

data RoyalBlue = RoyalBlue
instance IsColor RoyalBlue where
  toRGB = const $ RGB 0x41 0x69 0xE1

instance NamedColor RoyalBlue where
  type ColorName RoyalBlue = "RoyalBlue"

data SaddleBrown = SaddleBrown
instance IsColor SaddleBrown where
  toRGB = const $ RGB 0x8B 0x45 0x13

instance NamedColor SaddleBrown where
  type ColorName SaddleBrown = "SaddleBrown"

data Salmon = Salmon
instance IsColor Salmon where
  toRGB = const $ RGB 0xFA 0x80 0x72

instance NamedColor Salmon where
  type ColorName Salmon = "Salmon"

data SandyBrown = SandyBrown
instance IsColor SandyBrown where
  toRGB = const $ RGB 0xF4 0xA4 0x60

instance NamedColor SandyBrown where
  type ColorName SandyBrown = "SandyBrown"

data SeaGreen = SeaGreen
instance IsColor SeaGreen where
  toRGB = const $ RGB 0x2E 0x8B 0x57

instance NamedColor SeaGreen where
  type ColorName SeaGreen = "SeaGreen"

data Seashell = Seashell
instance IsColor Seashell where
  toRGB = const $ RGB 0xFF 0xF5 0xEE

instance NamedColor Seashell where
  type ColorName Seashell = "Seashell"

data Sienna = Sienna
instance IsColor Sienna where
  toRGB = const $ RGB 0xA0 0x52 0x2D

instance NamedColor Sienna where
  type ColorName Sienna = "Sienna"

data Silver = Silver
instance IsColor Silver where
  toRGB = const $ RGB 0xC0 0xC0 0xC0

instance NamedColor Silver where
  type ColorName Silver = "Silver"

data SkyBlue = SkyBlue
instance IsColor SkyBlue where
  toRGB = const $ RGB 0x87 0xCE 0xEB

instance NamedColor SkyBlue where
  type ColorName SkyBlue = "SkyBlue"

data SlateBlue = SlateBlue
instance IsColor SlateBlue where
  toRGB = const $ RGB 0x6A 0x5A 0xCD

instance NamedColor SlateBlue where
  type ColorName SlateBlue = "SlateBlue"

data SlateGray = SlateGray
instance IsColor SlateGray where
  toRGB = const $ RGB 0x70 0x80 0x90

instance NamedColor SlateGray where
  type ColorName SlateGray = "SlateGray"

data Snow = Snow
instance IsColor Snow where
  toRGB = const $ RGB 0xFF 0xFA 0xFA

instance NamedColor Snow where
  type ColorName Snow = "Snow"

data SpringGreen = SpringGreen
instance IsColor SpringGreen where
  toRGB = const $ RGB 0x00 0xFF 0x7F

instance NamedColor SpringGreen where
  type ColorName SpringGreen = "SpringGreen"

data SteelBlue = SteelBlue
instance IsColor SteelBlue where
  toRGB = const $ RGB 0x46 0x82 0xB4

instance NamedColor SteelBlue where
  type ColorName SteelBlue = "SteelBlue"

data Tan = Tan
instance IsColor Tan where
  toRGB = const $ RGB 0xD2 0xB4 0x8C

instance NamedColor Tan where
  type ColorName Tan = "Tan"

data Teal = Teal
instance IsColor Teal where
  toRGB = const $ RGB 0x00 0x80 0x80

instance NamedColor Teal where
  type ColorName Teal = "Teal"

data Thistle = Thistle
instance IsColor Thistle where
  toRGB = const $ RGB 0xD8 0xBF 0xD8

instance NamedColor Thistle where
  type ColorName Thistle = "Thistle"

data Tomato = Tomato
instance IsColor Tomato where
  toRGB = const $ RGB 0xFF 0x63 0x47

instance NamedColor Tomato where
  type ColorName Tomato = "Tomato"

data Turquoise = Turquoise
instance IsColor Turquoise where
  toRGB = const $ RGB 0x40 0xE0 0xD0

instance NamedColor Turquoise where
  type ColorName Turquoise = "Turquoise"

data Violet = Violet
instance IsColor Violet where
  toRGB = const $ RGB 0xEE 0x82 0xEE

instance NamedColor Violet where
  type ColorName Violet = "Violet"

data Wheat = Wheat
instance IsColor Wheat where
  toRGB = const $ RGB 0xF5 0xDE 0xB3

instance NamedColor Wheat where
  type ColorName Wheat = "Wheat"

data White = White
instance IsColor White where
  toRGB = const $ RGB 0xFF 0xFF 0xFF

instance NamedColor White where
  type ColorName White = "White"

data WhiteSmoke = WhiteSmoke
instance IsColor WhiteSmoke where
  toRGB = const $ RGB 0xF5 0xF5 0xF5

instance NamedColor WhiteSmoke where
  type ColorName WhiteSmoke = "WhiteSmoke"

data Yellow = Yellow
instance IsColor Yellow where
  toRGB = const $ RGB 0xFF 0xFF 0x00

instance NamedColor Yellow where
  type ColorName Yellow = "Yellow"

data YellowGreen = YellowGreen
instance IsColor YellowGreen where
  toRGB = const $ RGB 0x9A 0xCD 0x32

instance NamedColor YellowGreen where
  type ColorName YellowGreen = "YellowGreen"

x11Theme = instantiateTheme  $
  AddColor AliceBlue         $
  AddColor AntiqueWhite      $
  AddColor Aqua              $
  AddColor Aquamarine        $
  AddColor Azure             $
  AddColor Beige             $
  AddColor Bisque            $
  AddColor Black             $
  AddColor BlanchedAlmond    $
  AddColor Blue              $
  AddColor BlueViolet        $
  AddColor Brown             $
  AddColor Burlywood         $
  AddColor CadetBlue         $
  AddColor Chartreuse        $
  AddColor Chocolate         $
  AddColor Coral             $
  AddColor CornflowerBlue    $
  AddColor Cornsilk          $
  AddColor Crimson           $
  AddColor Cyan              $
  AddColor DarkBlue          $
  AddColor DarkCyan          $
  AddColor DarkGoldenrod     $
  AddColor DarkGray          $
  AddColor DarkGreen         $
  AddColor DarkKhaki         $
  AddColor DarkMagenta       $
  AddColor DarkOlive         $
  AddColor DarkOrange        $
  AddColor DarkOrchid        $
  AddColor DarkRed           $
  AddColor DarkSalmon        $
  AddColor DarkSea           $
  AddColor DarkSlateBlue     $
  AddColor DarkSlateGray     $
  AddColor DarkTurquoise     $
  AddColor DarkViolet        $
  AddColor DeepPink          $
  AddColor DeepSkyBlue       $
  AddColor DimGray           $
  AddColor DodgerBlue        $
  AddColor Firebrick         $
  AddColor FloralWhite       $
  AddColor ForestGreen       $
  AddColor Fuchsia           $
  AddColor Gainsboro         $
  AddColor GhostWhite        $
  AddColor Gold              $
  AddColor Goldenrod         $
  AddColor Gray              $
  AddColor WebGray           $
  AddColor Green             $
  AddColor WebGreen          $
  AddColor GreenYellow       $
  AddColor Honeydew          $
  AddColor HotPink           $
  AddColor Indian            $
  AddColor Indigo            $
  AddColor Ivory             $
  AddColor Khaki             $
  AddColor Lavender          $
  AddColor LavenderBlush     $
  AddColor LawnGreen         $
  AddColor LemonChiffon      $
  AddColor LightBlue         $
  AddColor LightCoral        $
  AddColor LightCyan         $
  AddColor LightGoldenrod    $
  AddColor LightGray         $
  AddColor LightGreen        $
  AddColor LightPink         $
  AddColor LightSalmon       $
  AddColor LightSea          $
  AddColor LightSky          $
  AddColor LightSlate        $
  AddColor LightSteel        $
  AddColor LightYellow       $
  AddColor Lime              $
  AddColor LimeGreen         $
  AddColor Linen             $
  AddColor Magenta           $
  AddColor Maroon            $
  AddColor Web               $
  AddColor MediumAquamarine  $
  AddColor MediumBlue        $
  AddColor MediumOrchid      $
  AddColor MediumPurple      $
  AddColor MediumSeaGreen    $
  AddColor MediumSlateBlue   $
  AddColor MediumSpringGreen $
  AddColor MediumTurquoise   $
  AddColor MediumVioletRed   $
  AddColor MidnightBlue      $
  AddColor MintCream         $
  AddColor MistyRose         $
  AddColor Moccasin          $
  AddColor NavajoWhite       $
  AddColor NavyBlue          $
  AddColor OldLace           $
  AddColor Olive             $
  AddColor OliveDrab         $
  AddColor Orange            $
  AddColor OrangeRed         $
  AddColor Orchid            $
  AddColor PaleGoldenrod     $
  AddColor PaleGreen         $
  AddColor PaleTurquoise     $
  AddColor PaleViolet        $
  AddColor PapayaWhip        $
  AddColor PeachPuff         $
  AddColor Peru              $
  AddColor Pink              $
  AddColor Plum              $
  AddColor Powder            $
  AddColor Purple            $
  AddColor WebPurple         $
  AddColor RebeccaPurple     $
  AddColor Red               $
  AddColor RosyBrown         $
  AddColor RoyalBlue         $
  AddColor SaddleBrown       $
  AddColor Salmon            $
  AddColor SandyBrown        $
  AddColor SeaGreen          $
  AddColor Seashell          $
  AddColor Sienna            $
  AddColor Silver            $
  AddColor SkyBlue           $
  AddColor SlateBlue         $
  AddColor SlateGray         $
  AddColor Snow              $
  AddColor SpringGreen       $
  AddColor SteelBlue         $
  AddColor Tan               $
  AddColor Teal              $
  AddColor Thistle           $
  AddColor Tomato            $
  AddColor Turquoise         $
  AddColor Violet            $
  AddColor Wheat             $
  AddColor White             $
  AddColor WhiteSmoke        $
  AddColor Yellow            $
  AddColor YellowGreen
  NewTheme
