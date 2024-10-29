





















step_model (HourTemp = Winters_hours_gaps$Temp,df= )



step_model(HourTemps, df)

customChill (Winters_hours_gaps$Temp)

?chilling

chilling(stack_hourly_temps(fix_weather(KA_weather[which(KA_weather$Year > 2006), ]),
                            latitude = 50.4))

output <- tempResponse(make_JDay(Winters_hours_gaps), 
                       Start_JDay = 90,
                       End_JDay = 100, 
                       models = list(Chill_Portions = Dynamic_Model, 
                                      GDH = GDH))

output <- tempResponse(make_JDay(Winters_hours_gaps),
                       Start_JDay = 90,
                       End_JDay = 100,
                       models = list(Chill_Portions = Dynamic_Model, 
                                     GDH = GDH, Harry = customChill))





