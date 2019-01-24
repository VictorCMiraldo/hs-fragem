data MusicSkeleton 
  = Random
  | Fixed MusicPattern

data MusicPattern
  = IonianUp | IonianDown | BluesAlternating | ...

synthesize :: [MusicSkeleton] -> [Measure]
synthesize = ...