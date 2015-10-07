module FRP.Yampa.Extensions where

import FRP.Yampa as Yampa

-- * FRP Extensions
rMergeSF :: SF a (Yampa.Event b) -> SF a (Yampa.Event b) -> SF a (Yampa.Event b)
rMergeSF = lift2 rMerge

lift2 :: (b -> c -> d) -> SF a b -> SF a c -> SF a d
lift2 f sf1 sf2 = (sf1 &&& sf2) >>> arr (uncurry f)

-- Taken and modified from the original Yampa code
sampleWindow' :: a -> Int -> Time -> SF a (Yampa.Event [a])
sampleWindow' def wl q =
    identity &&& afterEachCat (repeat (q, ()))
    >>> arr (\(a, e) -> fmap (map (const a)) e)
    >>> accumBy updateWindow (take wl $ repeat def)
    where
        updateWindow w as = drop (max (length w' - wl) 0) w'
            where w' = w ++ as
