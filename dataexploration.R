#Data exploration






table(syria_sequence$mignr)
table(syria_sequence$country)



iraq_sequence <- sequence[sequence$corigin == 60, ]

table(iraq_sequence$mignr)
table(iraq_sequence$country)

afg_pid <- sequence[sequence$mignr == 0 & sequence$country == "Afghanistan", 1]

afg_pid <- sequence[sequence$pid %in% afg_pid, ]

table(afg_pid$mignr)
table(afg_pid$country)

