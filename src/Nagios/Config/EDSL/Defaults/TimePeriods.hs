module Nagios.Config.EDSL.Defaults.TimePeriods where

import Nagios.Config.EDSL.Types

always :: TimePeriod
always =
    let hours = "00:00-24:00"
        days = [Monday, Tuesday, Wednesday, Thursday, Friday, Saterday, Sunday]
    in
    TimePeriod { timePeriodName = "24x7"
               , timePeriodAlias = "24 Hours A Day, 7 Days A Week"
               , timePeriodWeekdays = map (\x -> x hours) days }

workhours :: TimePeriod
workhours =
    let hours = "09:00-17:00"
        days = [Monday, Tuesday, Wednesday, Thursday, Friday]
    in
    TimePeriod { timePeriodName = "workhours"
               , timePeriodAlias = "Normal Work Hours"
               , timePeriodWeekdays = map (\x -> x hours) days }

never :: TimePeriod
never = TimePeriod { timePeriodName = "none"
                   , timePeriodAlias = "No Time Is A Good Time"
                   , timePeriodWeekdays = [] }

