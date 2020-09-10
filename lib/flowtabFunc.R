flowtabFunc <- function(desc) {
  outsdata <- pdata %>%
    count(koll) %>%
    complete(koll = c(NA, "Yes", "No"), fill = list(n = 0))

  flowtmp <- c(
    outsdata$n[outsdata$koll == "Yes" & !is.na(outsdata$koll)],
    paste0(
      outsdata$n[outsdata$koll == "No" & !is.na(outsdata$koll)] + outsdata$n[is.na(outsdata$koll)], " (",
      outsdata$n[is.na(outsdata$koll)], ")"
    )
  )

  flow <<- rbind(flow, c(desc, flowtmp))

  pdata <<- pdata %>% filter(koll == "Yes")
}
