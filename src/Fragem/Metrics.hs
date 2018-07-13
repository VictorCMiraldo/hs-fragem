module Fragem.Metrics
  ( module Base
  , module BM
  , module IM
  ) where

import Fragem.Syntax
import Fragem.Syntax.Examples
import Fragem.Metrics.Base        as Base
import Fragem.Metrics.BeatMetric  as BM
import Fragem.Metrics.InnerMetric as IM

import qualified Data.Map as M

metricTest1 :: [Measure]
metricTest1 = [ Measure [ Note i          whole       50
                        , Note (i + wh)   eigth       47
                        , Note (i + ww)   half        47
                        , Note (i + wwh)  half        45
                        , Note (i + wwwh) (half + wh) 50
                        ]
              , Measure [ Note (i + 4*w + wh) eigth 47
                        , Note (i + 6*w)      half  47
                        ]
              ]
  where
    tpb = 60
    i = 0
    wh = whole + half
    whole = tpb
    w     = whole
    ww    = 2*w
    wwh   = w + wh
    wwwh  = ww + wh
    half  = whole `div` 2
    eigth = half `div` 2
