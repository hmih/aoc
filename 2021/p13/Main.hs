{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.List as L
import qualified Data.Maybe as M
import qualified Data.Text as T
import qualified Data.Text.IO as I

data Display = Display
  { wires :: [T.Text],
    out :: [T.Text]
  }
  deriving (Show)

data Segment
  = BotRight Char
  | Mid Char
  | Top Char
  | Bot Char
  | TopLeft Char
  | BotLeft Char
  | TopRight Char
  deriving (Show)

segmentsToInt :: [Segment] -> Maybe Int
segmentsToInt [Top _, TopRight _, BotRight _, Bot _, BotLeft _, TopLeft _] = Just 0
segmentsToInt [TopRight _, BotRight _] = Just 1
segmentsToInt [Top _, Mid _, Bot _, TopRight _, BotLeft _] = Just 2
segmentsToInt [Top _, Mid _, Bot _, TopRight _, BotRight _] = Just 3
segmentsToInt [Mid _, TopRight _, TopLeft _, BotRight _] = Just 4
segmentsToInt [Top _, Mid _, Bot _, TopLeft _, BotRight _] = Just 5
segmentsToInt [Top _, Mid _, Bot _, TopLeft _, BotLeft _, BotRight _] = Just 6
segmentsToInt [Top _, TopRight _, BotRight _] = Just 7
segmentsToInt [Top _, Mid _, Bot _, TopLeft _, BotLeft _, TopRight _, BotRight _] = Just 8
segmentsToInt [Top _, Mid _, Bot _, TopLeft _, TopRight _, BotRight _] = Just 9
segmentsToInt _ = Nothing

main :: IO ()
main = do
  displays <- readInput

  let results = fmap decodeDisplay displays
      result = (sum . (fmap (\[m, h, t, d] -> 1000 * m + 100 * h + 10 * t + 1 * d))) results

  print result
  print $ T.unpack "hello, world"

extractTopRight :: T.Text -> Segment
extractTopRight x = TopRight (T.index x 0)

-- give it encoded 1
extractBotRight :: T.Text -> Segment
extractBotRight x = BotRight (T.index x 1)

-- give it encoded 7
extractTop :: [Segment] -> T.Text -> Segment
extractTop [(TopRight tr), (BotRight br)] seven =
  let diff = (T.unpack seven) L.\\ [tr, br]
      top = diff !! 0
   in Top top
extractTop _ _ = error "failed to parse top"

findThree :: [Segment] -> [T.Text] -> T.Text
findThree [(TopRight tr), (BotRight br)] xs =
  let sized = L.filter (\x -> (T.length x) == 5) xs
      both = L.filter (\x -> T.elem tr x && T.elem br x) sized
   in head both
findThree _ _ = error "failed to parse three"

-- give it encoded three and four
extractTopLeft :: T.Text -> T.Text -> Segment
extractTopLeft three four =
  let diff = (T.unpack four) L.\\ (T.unpack three)
      tl = diff !! 0
   in TopLeft tl

-- give it encoded four
extractMid :: [Segment] -> T.Text -> Segment
extractMid [(TopRight tr), (BotRight br), (TopLeft tl)] four =
  let diff = (T.unpack four) L.\\ [tr, br, tl]
      mid = diff !! 0
   in Mid mid
extractMid _ _ = error "failed to parse mid"

-- give it encoded three
extractBot :: [Segment] -> T.Text -> Segment
extractBot [(TopRight tr), (BotRight br), (Top t), (Mid m)] three =
  let diff = (T.unpack three) L.\\ [tr, br, t, m]
      bot = diff !! 0
   in Bot bot
extractBot _ _ = error "failed to parse bot"

-- give it encoded eight
extractBotLeft :: [Segment] -> T.Text -> Segment
extractBotLeft [(Top t), (Mid m), (Bot b), (TopLeft tl), (TopRight tr), (BotRight br)] eight =
  let diff = (T.unpack eight) L.\\ [t, m, b, tl, tr, br]
      bl = diff !! 0
   in BotLeft bl
extractBotLeft _ _ = error "failed to parse bl"

hasSegment :: Segment -> T.Text -> Bool
hasSegment (Top v) o = T.elem v o
hasSegment (Bot v) o = T.elem v o
hasSegment (Mid v) o = T.elem v o
hasSegment (TopLeft v) o = T.elem v o
hasSegment (TopRight v) o = T.elem v o
hasSegment (BotLeft v) o = T.elem v o
hasSegment (BotRight v) o = T.elem v o

containedSegment :: Segment -> T.Text -> Maybe Segment
containedSegment s o = if (hasSegment s o) then Just s else Nothing

decodeOutputs :: [Segment] -> [T.Text] -> Maybe [Int]
decodeOutputs segs outs =
  let results = fmap (decodeOutput segs) outs
      reduced = M.catMaybes $ results
   in if L.length results /= L.length reduced then Nothing else Just reduced

decodeOutput :: [Segment] -> T.Text -> Maybe Int
decodeOutput segs out =
  let segments = M.catMaybes $ fmap (\s -> containedSegment s out) segs
      perms = L.permutations segments
      opt = M.catMaybes $ fmap segmentsToInt perms
   in if (L.length opt) > 0 then Just (opt !! 0) else Nothing

decodeParts :: [T.Text] -> [[Segment]]
decodeParts ws =
  let sorted = L.sortBy (\f -> \s -> compare (T.length f) (T.length s)) ws
      one = head sorted
      seven = (head . tail) sorted
      four = (head . tail . tail) sorted
      eight = (last sorted)
      -- don't know which tr and br is which yet
      tr = extractTopRight one
      br = extractBotRight one
      top = extractTop [tr, br] seven
      three = findThree [tr, br] sorted
      tl = extractTopLeft three four
      mid = extractMid [tr, br, tl] four
      bot = extractBot [tr, br, top, mid] three
      bl = extractBotLeft [top, mid, bot, tl, tr, br] eight
      res = [tr, br, top, tl, mid, bot, bl]
      --
      one' = T.reverse one
      tr' = extractTopRight one'
      br' = extractBotRight one'
      top' = extractTop [tr', br'] seven
      three' = findThree [tr', br'] sorted
      tl' = extractTopLeft three four
      mid' = extractMid [tr', br', tl'] four
      bot' = extractBot [tr', br', top', mid'] three
      bl' = extractBotLeft [top', mid', bot', tl', tr', br'] eight
      res' = [tr', br', top', tl', mid', bot', bl']
   in [res, res']

decodeDisplay :: Display -> [Int]
decodeDisplay d =
  let segments = decodeParts (wires d)
      [f, s] = fmap (\s -> decodeOutputs s (out d)) segments
      res = if M.isJust f then M.fromJust f else if M.isJust s then M.fromJust s else error "wtf"
   in res

readInput :: IO [Display]
readInput = do
  txt <- I.readFile "in"

  let parts = fmap (\l -> let x = T.splitOn " | " l in (x !! 0, x !! 1)) (T.lines txt)

  pure $
    fmap
      ( \(w, o) ->
          let ws = T.splitOn " " w
              os = T.splitOn " " o
           in Display {wires = ws, out = os}
      )
      parts
