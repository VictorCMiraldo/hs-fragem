
module Fragem.Syntax.Examples where

import Fragem.Syntax


-----------------------
-- * Some Examples * --
-----------------------

-- Treble cleff from Hanon's first etude
hanon1_treble_full :: Section
hanon1_treble_full = Section (TimeSig 2 4) 240
  [ Measure [ Note 0 54 48
            , Note 60 54 52
            , Note 120 54 53
            , Note 180 54 55
            , Note 240 54 57
            , Note 300 54 55
            , Note 360 54 53
            , Note 420 54 52]
  , Measure [ Note 480 54 50
            , Note 540 54 53
            , Note 600 54 55
            , Note 660 54 57
            , Note 720 54 59
            , Note 780 54 57
            , Note 840 54 55
            , Note 900 54 53]
  , Measure [ Note 960 54 52
            , Note 1020 54 55
            , Note 1080 54 57
            , Note 1140 54 59
            , Note 1200 54 60
            , Note 1260 54 59
            , Note 1320 54 57
            , Note 1380 54 55]
  , Measure [ Note 1440 54 53
            , Note 1500 54 57
            , Note 1560 54 59
            , Note 1620 54 60
            , Note 1680 54 62
            , Note 1740 54 60
            , Note 1800 54 59
            , Note 1860 54 57]
  , Measure [ Note 1920 54 55
            , Note 1980 54 59
            , Note 2040 54 60
            , Note 2100 54 62
            , Note 2160 54 64
            , Note 2220 54 62
            , Note 2280 54 60
            , Note 2340 54 59]
  , Measure [ Note 2400 54 57
            , Note 2460 54 60
            , Note 2520 54 62
            , Note 2580 54 64
            , Note 2640 54 65
            , Note 2700 54 64
            , Note 2760 54 62
            , Note 2820 54 60]
  , Measure [ Note 2880 54 59
            , Note 2940 54 62
            , Note 3000 54 64
            , Note 3060 54 65
            , Note 3120 54 67
            , Note 3180 54 65
            , Note 3240 54 64
            , Note 3300 54 62]
  , Measure [ Note 3360 54 60
            , Note 3420 54 64
            , Note 3480 54 65
            , Note 3540 54 67
            , Note 3600 54 69
            , Note 3660 54 67
            , Note 3720 54 65
            , Note 3780 54 64]
  , Measure [ Note 3840 54 62
            , Note 3900 54 65
            , Note 3960 54 67
            , Note 4020 54 69
            , Note 4080 54 71
            , Note 4140 54 69
            , Note 4200 54 67
            , Note 4260 54 65]
  , Measure [ Note 4320 54 64
            , Note 4380 54 67
            , Note 4440 54 69
            , Note 4500 54 71
            , Note 4560 54 72
            , Note 4620 54 71
            , Note 4680 54 69
            , Note 4740 54 67]
  , Measure [ Note 4800 54 65
            , Note 4860 54 69
            , Note 4920 54 71
            , Note 4980 54 72
            , Note 5040 54 74
            , Note 5100 54 72
            , Note 5160 54 71
            , Note 5220 54 69]
  , Measure [ Note 5280 54 67
            , Note 5340 54 71
            , Note 5400 54 72
            , Note 5460 54 74
            , Note 5520 54 76
            , Note 5580 54 74
            , Note 5640 54 72
            , Note 5700 54 71]
  , Measure [ Note 5760 54 69
            , Note 5820 54 72
            , Note 5880 54 74
            , Note 5940 54 76
            , Note 6000 54 77
            , Note 6060 54 76
            , Note 6120 54 74
            , Note 6180 54 72]
  , Measure [ Note 6240 54 71
            , Note 6300 54 74
            , Note 6360 54 76
            , Note 6420 54 77
            , Note 6480 54 79
            , Note 6540 54 77
            , Note 6600 54 76
            , Note 6660 54 74]
  , Measure [ Note 6720 54 79
            , Note 6780 54 76
            , Note 6840 54 74
            , Note 6900 54 72
            , Note 6960 54 71
            , Note 7020 54 72
            , Note 7080 54 74
            , Note 7140 54 76]
  , Measure [ Note 7200 54 77
            , Note 7260 54 74
            , Note 7320 54 72
            , Note 7380 54 71
            , Note 7440 54 69
            , Note 7500 54 71
            , Note 7560 54 72
            , Note 7620 54 74]
  , Measure [ Note 7680 54 76
            , Note 7740 54 72
            , Note 7800 54 71
            , Note 7860 54 69
            , Note 7920 54 67
            , Note 7980 54 69
            , Note 8040 54 71
            , Note 8100 54 72]
  , Measure [ Note 8160 54 74
            , Note 8220 54 71
            , Note 8280 54 69
            , Note 8340 54 67
            , Note 8400 54 65
            , Note 8460 54 67
            , Note 8520 54 69
            , Note 8580 54 71]
  , Measure [ Note 8640 54 72
            , Note 8700 54 69
            , Note 8760 54 67
            , Note 8820 54 65
            , Note 8880 54 64
            , Note 8940 54 65
            , Note 9000 54 67
            , Note 9060 54 69]
  , Measure [ Note 9120 54 71
            , Note 9180 54 67
            , Note 9240 54 65
            , Note 9300 54 64
            , Note 9360 54 62
            , Note 9420 54 64
            , Note 9480 54 65
            , Note 9540 54 67]
  , Measure [ Note 9600 54 69
            , Note 9660 54 65
            , Note 9720 54 64
            , Note 9780 54 62
            , Note 9840 54 60
            , Note 9900 54 62
            , Note 9960 54 64
            , Note 10020 54 65]
  , Measure [ Note 10080 54 67
            , Note 10140 54 64
            , Note 10200 54 62
            , Note 10260 54 60
            , Note 10320 54 59
            , Note 10380 54 60
            , Note 10440 54 62
            , Note 10500 54 64]
  , Measure [ Note 10560 54 65
            , Note 10620 54 62
            , Note 10680 54 60
            , Note 10740 54 59
            , Note 10800 54 57
            , Note 10860 54 59
            , Note 10920 54 60
            , Note 10980 54 62]
  , Measure [ Note 11040 54 64
            , Note 11100 54 60
            , Note 11160 54 59
            , Note 11220 54 57
            , Note 11280 54 55
            , Note 11340 54 57
            , Note 11400 54 59
            , Note 11460 54 60]
  , Measure [ Note 11520 54 62
            , Note 11580 54 59
            , Note 11640 54 57
            , Note 11700 54 55
            , Note 11760 54 53
            , Note 11820 54 55
            , Note 11880 54 57
            , Note 11940 54 59]
  , Measure [ Note 12000 54 60
            , Note 12060 54 57
            , Note 12120 54 55
            , Note 12180 54 53
            , Note 12240 54 52
            , Note 12300 54 53
            , Note 12360 54 55
            , Note 12420 54 57]
  , Measure [ Note 12480 54 59
            , Note 12540 54 55
            , Note 12600 54 53
            , Note 12660 54 52
            , Note 12720 54 50
            , Note 12780 54 52
            , Note 12840 54 53
            , Note 12900 54 55]
  , Measure [ Note 12960 54 57
            , Note 13020 54 53
            , Note 13080 54 52
            , Note 13140 54 50
            , Note 13200 54 48
            , Note 13260 54 50
            , Note 13320 54 52
            , Note 13380 54 53]
  , Measure [ Note 13440 54 55
            , Note 13500 54 52
            , Note 13560 54 50
            , Note 13620 54 48
            , Note 13680 54 47
            , Note 13740 54 48
            , Note 13800 54 50
            , Note 13860 54 52]
  , Measure [ Note 13920 432 48]
  ]


-- First six measures of 'Evening in Transylvania'
-- by Bela Bartok (treble cleff)
bartok_section :: Section
bartok_section = Section (TimeSig 4 4) 240
  [ Measure [ Note 0    600 76
            , Note 600  120 74
            , Note 720  120 71
            , Note 840  120 74]
  , Measure [ Note 960  120 76
            , Note 1080 120 74
            , Note 1200 120 74
            , Note 1320 600 71]
  , Measure [ Note 1920 600 76
            , Note 2520 120 74
            , Note 2640 120 71
            , Note 2760 120 74]
  , Measure [ Note 2880 120 76
            , Note 3000 120 74
            , Note 3120 120 71
            , Note 3240 600 69]
  , Measure [ Note 3840 600 71
            , Note 4440 120 69
            , Note 4560 120 71
            , Note 4680 120 76]
  , Measure [ Note 4800 120 74
            , Note 4920 120 71
            , Note 5040 120 67
            , Note 5160 600 64
            ]
  ]
            
            
