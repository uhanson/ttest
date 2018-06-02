module TimeUtils where
  import Data.Time
 
  applyZoned :: (UTCTime -> UTCTime) -> ZonedTime -> ZonedTime
  applyZoned f t = utcToZonedTime (zonedTimeZone t) (f (zonedTimeToUTC t))