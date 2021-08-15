module Color where
import Data.Word (Word8)

data RGBColor = RGBColor
  { rgbRed   :: Word8
  , rgbGreen :: Word8
  , rgbBlue  :: Word8
  } deriving (Eq, Show)

class IsColor a where
  toRGB ::
