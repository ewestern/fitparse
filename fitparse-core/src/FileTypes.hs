{-# LANGUAGE LambdaCase #-}

module FileTypes where

data FileType
  = Device
  | Settings
  | SportSettings
  | Activity
  | Workout
  | Course
  | Schedule
  | Weight
  | Totals
  | Goals
  | BloodPressure
  | MonitoringA
  | ActivitySummary
  | DailyMonitoring
  | MonitoringB
  | Segment
  | SegmentList
  deriving (Show, Eq)


getFileType :: Int -> Maybe FileType
getFileType = \case
  1 ->  Just Device
  2 ->  Just Settings
  3 ->  Just SportSettings
  4 ->  Just Activity
  5 ->  Just Workout
  6 ->  Just Course
  7 ->  Just Schedule
  9 ->  Just Weight
  10 -> Just Totals
  11 -> Just Goals
  14 -> Just BloodPressure
  15 -> Just MonitoringA
  20 -> Just ActivitySummary
  28 -> Just DailyMonitoring
  32 -> Just MonitoringB
  34 -> Just Segment
  35 -> Just SegmentList
  _ -> Nothing
