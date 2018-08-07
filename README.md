# FRactal GEOmetry of Music

  This library provides functionality for computing Hausdorff dimension over
  excerpts of music in MIDI format. The `fragem` executable provides 
  a rudimentary command line interface.

  The notion of scalling is modelled by metrics, which tells us which notes to
  consider at each given scalling factor.

## Installing

  Run `stack build`

## Usage of `fragem`

  Run `stack exec fragem -- --options --to --fragem --come --here`.

### Examples:

  Get the fractal dimension of the prelude to Bach's Cello suite number 1.
  The measures are grouped 2 by 2:

```
$ stack exec fragem -- "dataset/jsbach/cellosui/01prelud.mid"
0.178850387616
0.158114378584
0.109915367425
0.143308151141
0.122156915346
0.129072754513
0.097805716386
0.123414766481
0.143280562736
0.117777582496
0.131543326263
0.086578741273
0.112061820095
0.104881646668
0.097654886808
0.138884555284
0.118860785174
0.089003949420
0.239329990569
0.221046594345
1.551035438443
```

  Now, lets say one wants to get the dimensions on by jumping from scaling factor
  3 to 5 and group the measures 4 by 4:

```
$ stack exec fragem -- --group=4 --zoom=3,5 "dataset/jsbach/cellosui/01prelud.mid"
0.320504698703
0.317546489356
0.316865532637
0.316786378126
0.319154811106
0.318082602743
0.317389643893
0.316978561089
0.316091297338
0.316034983039
```

  Now, lets say we are only interested in the first 8 measures:

```
$ stack exec fragem -- --interval=0,8 --group=4 --zoom=3,5 "dataset/jsbach/cellosui/01prelud.mid"
0.320504698703
0.317546489356
```

  We can also see information about how many voices and how many sections are
  in a given piece of music with the `--verbose` or `-v` flag:

```
$ stack exec fragem -- -v -i 0,8 -g 4 -z 3,5 "dataset/jsbach/cellosui/01prelud.mid"
Midi Information: 
Voice: 
---------------
  timesig:         4/4
  ticks-per-beat:  240
  measures length: 42

0.320504698703
0.317546489356
```
  
  This is usefull to learn how many voices and what the time-signature of each part
  of the midi file"

```
$ stack exec fragem -- -v --voice=1 "dataset/jsbach/sinfon/sinfon1.mid" 
Midi Information: 
Voice: 
---------------
timesig:         4/4
ticks-per-beat:  240
measures length: 21
Voice: 
---------------
timesig:         4/4
ticks-per-beat:  240
measures length: 20
Voice: 
---------------
timesig:         4/4
ticks-per-beat:  240
measures length: 21

1.243953157132
3.223508271268
0.023169562229
0.260019133590
0.297888639573
0.585230193007
1.671102048247
0.404998187800
0.026212211104
1.872974745471
```


  The `-d` or `--debug` flag displays some additional information about the internals:

```
$ stack exec fragem -- -v -d --interval=0,2 --group=2 --zoom=1,5 -v "dataset/jsbach/cellosui/01prelud.mid"
Midi Information: 
Voice: 
---------------
timesig:         4/4
ticks-per-beat:  240
measures length: 42

max metric zoom level: 5
Notes at: 5
43 |--      |        |        |        |--      |

Notes at: 1
60 |        |        |        |        |    --  |--  --  |    --  |--  --  |
   |    --  |--  --  |    --  |--  --  |      --|        |      --|        |
   |        |        |        |        |        |        |        |        |
   |      --|        |      --|        |        |        |        |        |
   |        |        |        |        |        |        |        |        |
   |        |        |        |        |        |        |        |        |
   |        |        |        |        |        |        |        |        |
   |        |        |        |        |        |        |        |        |
   |        |        |        |        |  --    |  --  --|  --    |  --  --|
   |        |        |        |        |        |        |        |        |
   |  --    |  --  --|  --    |  --  --|        |        |        |        |
   |        |        |        |        |        |        |        |        |
   |        |        |        |        |        |        |        |        |
   |        |        |        |        |        |        |        |        |
   |        |        |        |        |        |        |        |        |
   |        |        |        |        |        |        |        |        |
   |        |        |        |        |        |        |        |        |
43 |--      |        |--      |        |--      |        |--      |        |

1.057535219047
```
