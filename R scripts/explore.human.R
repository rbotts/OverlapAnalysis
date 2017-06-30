##Grouping data by human use

ind.data$Human <- ifelse(
  test = ind.data$Survey.Name == "Savegre Valley Fall 2012" & #Savegre FA12 - high traffic
    (
      ind.data$CamNumber1 == "1" |
        ind.data$CamNumber1 == "5" |
        ind.data$CamNumber1 == "6" |
        ind.data$CamNumber1 == "7" |
        ind.data$CamNumber1 == "8" |
        ind.data$CamNumber1 == "10" |
        ind.data$CamNumber1 == "19" |
        ind.data$CamNumber2 == "1" |
        ind.data$CamNumber2 == "5" |
        ind.data$CamNumber2 == "6" |
        ind.data$CamNumber2 == "7" |
        ind.data$CamNumber2 == "8" |
        ind.data$CamNumber2 == "10" |
        ind.data$CamNumber2 == "19"
    ),
  yes = TRUE,
  no = ifelse(
    test = ind.data$Survey.Name  == "Chirripo Fall 2012" & #Chirripo FA12 - high traffic
      (
        ind.data$CamNumber1 == "C1" |
          ind.data$CamNumber1 == "C2" |
          ind.data$CamNumber1 == "C3" |
          ind.data$CamNumber1 == "C4" |
          ind.data$CamNumber1 == "C5" |
          ind.data$CamNumber1 == "C6" |
          ind.data$CamNumber1 == "C7" |
          ind.data$CamNumber1 == "C8" |
          ind.data$CamNumber2 == "C1" |
          ind.data$CamNumber2 == "C2" |
          ind.data$CamNumber2 == "C3" |
          ind.data$CamNumber2 == "C4" |
          ind.data$CamNumber2 == "C5" |
          ind.data$CamNumber2 == "C6" |
          ind.data$CamNumber2 == "C7" |
          ind.data$CamNumber2 == "C8"
      ),
    yes = TRUE,
    no = ifelse(
      test = ind.data$Survey.Name == "Tapanti Fall 2012" & #Tapanti FA12 - high traffic
        (
          ind.data$CamNumber1 == "T1" |
            ind.data$CamNumber1 == "T2" |
            ind.data$CamNumber1 == "T3" |
            ind.data$CamNumber1 == "T4" |
            ind.data$CamNumber1 == "T5" |
            ind.data$CamNumber1 == "T6" |
            ind.data$CamNumber1 == "T7" |
            ind.data$CamNumber1 == "T8" |
            ind.data$CamNumber1 == "T9" |
            ind.data$CamNumber1 == "T10" |
            ind.data$CamNumber1 == "T11" |
            ind.data$CamNumber1 == "T12" |
            ind.data$CamNumber1 == "T13" |
            ind.data$CamNumber1 == "T14" |
            ind.data$CamNumber2 == "T1" |
            ind.data$CamNumber2 == "T2" |
            ind.data$CamNumber2 == "T3" |
            ind.data$CamNumber2 == "T4" |
            ind.data$CamNumber2 == "T5" |
            ind.data$CamNumber2 == "T6" |
            ind.data$CamNumber2 == "T7" |
            ind.data$CamNumber2 == "T8" |
            ind.data$CamNumber2 == "T9" |
            ind.data$CamNumber2 == "T10" |
            ind.data$CamNumber2 == "T11" |
            ind.data$CamNumber2 == "T12" |
            ind.data$CamNumber2 == "T13" |
            ind.data$CamNumber2 == "T14"
        ),
      yes = TRUE,
      no = ifelse(
        test = ind.data$Survey.Name == "Savegre Valley Fall 2012" & #Savegre FA12 - low traffic
          (
            ind.data$CamNumber1 == "26" |
              ind.data$CamNumber1 == "27" |
              ind.data$CamNumber2 == "26" |
              ind.data$CamNumber2 == "27"
          ),
        yes = FALSE,
        no = ifelse(
          test = ind.data$Survey.Name == "Chirripo Fall 2012" & #Chirripo FA12 - low traffic
            (
              ind.data$CamNumber1 == "C13" |
                ind.data$CamNumber1 == "C14" |
                ind.data$CamNumber1 == "C9" |
                ind.data$CamNumber1 == "C10" |
                ind.data$CamNumber2 == "C13" |
                ind.data$CamNumber2 == "C14" |
                ind.data$CamNumber2 == "C9" |
                ind.data$CamNumber2 == "C10"
            ),
          yes = FALSE,
          no = ifelse(
            test = ind.data$Survey.Name == "Tapanti Fall 2012" & #Tapanti FA12 - low traffic
              (
                ind.data$CamNumber1 == "T15" |
                  ind.data$CamNumber1 == "T16" |
                  ind.data$CamNumber1 == "T17" |
                  ind.data$CamNumber1 == "T18" |
                  ind.data$CamNumber1 == "T19" |
                  ind.data$CamNumber1 == "T20" |
                  ind.data$CamNumber1 == "T21" |
                  ind.data$CamNumber1 == "T22" |
                  ind.data$CamNumber1 == "T23" |
                  ind.data$CamNumber1 == "T24" |
                  ind.data$CamNumber1 == "T25" |
                  ind.data$CamNumber1 == "T26" |
                  ind.data$CamNumber2 == "T15" |
                  ind.data$CamNumber2 == "T16" |
                  ind.data$CamNumber2 == "T17" |
                  ind.data$CamNumber2 == "T18" |
                  ind.data$CamNumber2 == "T19" |
                  ind.data$CamNumber2 == "T20" |
                  ind.data$CamNumber2 == "T21" |
                  ind.data$CamNumber2 == "T22" |
                  ind.data$CamNumber2 == "T23" |
                  ind.data$CamNumber2 == "T24" |
                  ind.data$CamNumber2 == "T25" |
                  ind.data$CamNumber2 == "T26"
              ),
            yes = FALSE,
            no = ifelse(
              test = ind.data$Survey.Name == "Savegre Valley Spring 2013" & #Savegre SP13 - low traffic
                (
                  ind.data$CamNumber1 == "3" |
                    ind.data$CamNumber1 == "9" |
                    ind.data$CamNumber1 == "17" |
                    ind.data$CamNumber1 == "20" |
                    ind.data$CamNumber1 == "22" |
                    ind.data$CamNumber1 == "23" |
                    ind.data$CamNumber1 == "28" |
                    ind.data$CamNumber1 == "32" |
                    ind.data$CamNumber1 == "33" |
                    ind.data$CamNumber1 == "26" |
                    ind.data$CamNumber1 == "27" |
                    ind.data$CamNumber2 == "3" |
                    ind.data$CamNumber2 == "9" |
                    ind.data$CamNumber2 == "17" |
                    ind.data$CamNumber2 == "20" |
                    ind.data$CamNumber2 == "22" |
                    ind.data$CamNumber2 == "23" |
                    ind.data$CamNumber2 == "28" |
                    ind.data$CamNumber2 == "32" |
                    ind.data$CamNumber2 == "33" |
                    ind.data$CamNumber2 == "26" |
                    ind.data$CamNumber2 == "27"
                ),
              yes = FALSE,
              no = ifelse(
                test = ind.data$Survey.Name == "Savegre Valley Spring 2013" & #Savegre SP13 - high traffic
                  (
                    ind.data$CamNumber1 == "1" |
                      ind.data$CamNumber1 == "2" |
                      ind.data$CamNumber1 == "5" |
                      ind.data$CamNumber1 == "6" |
                      ind.data$CamNumber1 == "7" |
                      ind.data$CamNumber1 == "8" |
                      ind.data$CamNumber1 == "10" |
                      ind.data$CamNumber1 == "11" |
                      ind.data$CamNumber1 == "16" |
                      ind.data$CamNumber1 == "18" |
                      ind.data$CamNumber1 == "19" |
                      ind.data$CamNumber1 == "40" |
                      ind.data$CamNumber1 == "41" |
                      ind.data$CamNumber1 == "42" |
                      ind.data$CamNumber1 == "43" |
                      ind.data$CamNumber2 == "1" |
                      ind.data$CamNumber2 == "2" |
                      ind.data$CamNumber2 == "5" |
                      ind.data$CamNumber2 == "6" |
                      ind.data$CamNumber2 == "7" |
                      ind.data$CamNumber2 == "8" |
                      ind.data$CamNumber2 == "10" |
                      ind.data$CamNumber2 == "11" |
                      ind.data$CamNumber2 == "16" |
                      ind.data$CamNumber2 == "18" |
                      ind.data$CamNumber2 == "19" |
                      ind.data$CamNumber2 == "40" |
                      ind.data$CamNumber2 == "41" |
                      ind.data$CamNumber2 == "42" |
                      ind.data$CamNumber2 == "43"
                  ),
                yes = TRUE,
                no = ifelse(
                  test = ind.data$Survey.Name == "ASBC Spring 2013" & #ASBC SP13 - low traffic
                    (
                      ind.data$CamNumber1 == "13" |
                        ind.data$CamNumber1 == "14" |
                        ind.data$CamNumber1 == "17" |
                        ind.data$CamNumber1 == "18" |
                        ind.data$CamNumber1 == "A7" |
                        ind.data$CamNumber1 == "A8" |
                        ind.data$CamNumber1 == "A9" |
                        ind.data$CamNumber1 == "A10" |
                        ind.data$CamNumber2 == "13" |
                        ind.data$CamNumber2 == "14" |
                        ind.data$CamNumber2 == "17" |
                        ind.data$CamNumber2 == "18" |
                        ind.data$CamNumber2 == "A7" |
                        ind.data$CamNumber2 == "A8" |
                        ind.data$CamNumber2 == "A9" |
                        ind.data$CamNumber2 == "A10"
                    ),
                  yes = FALSE,
                  no = ifelse(
                    test = ind.data$Survey.Name == "ASBC Sprint 2013" & #ASBC SP13 - high traffic
                      (
                        ind.data$CamNumber1 == "A1" |
                          ind.data$CamNumber1 == "A2" |
                          ind.data$CamNumber2 == "A1" |
                          ind.data$CamNumber2 == "A2"
                      ),
                    yes = TRUE,
                    no = ifelse(
                      test = ind.data$Survey.Name == "Chirripo Spring 2013" & #Chirripo SP13 - high traffic
                        (
                          ind.data$CamNumber1 == "C1" |
                            ind.data$CamNumber1 == "C2" |
                            ind.data$CamNumber1 == "C3" |
                            ind.data$CamNumber1 == "C4" |
                            ind.data$CamNumber1 == "C5" |
                            ind.data$CamNumber1 == "C6" |
                            ind.data$CamNumber1 == "C7" |
                            ind.data$CamNumber1 == "C8" |
                            ind.data$CamNumber2 == "C1" |
                            ind.data$CamNumber2 == "C2" |
                            ind.data$CamNumber2 == "C3" |
                            ind.data$CamNumber2 == "C4" |
                            ind.data$CamNumber2 == "C5" |
                            ind.data$CamNumber2 == "C6" |
                            ind.data$CamNumber2 == "C7" |
                            ind.data$CamNumber2 == "C8"
                        ),
                      yes = TRUE,
                      no = ifelse(
                        test = ind.data$Survey.Name == "Chirripo Spring 2013" & #Chirripo SP13 - low traffic
                          (
                            ind.data$CamNumber1 == "C9" |
                              ind.data$CamNumber1 == "C10" |
                              ind.data$CamNumber1 == "C13" |
                              ind.data$CamNumber1 == "C14" |
                              ind.data$CamNumber2 == "C9" |
                              ind.data$CamNumber2 == "C10" |
                              ind.data$CamNumber2 == "C13" |
                              ind.data$CamNumber2 == "C14"
                          ),
                        yes = FALSE,
                        no = ifelse(
                          test = ind.data$Survey.Name == "Tapanti Spring 2013" & #Tapanti SP13 - high traffic
                            (
                              ind.data$CamNumber1 == "T1" |
                                ind.data$CamNumber1 == "T2" |
                                ind.data$CamNumber1 == "T3" |
                                ind.data$CamNumber1 == "T4" |
                                ind.data$CamNumber1 == "T5" |
                                ind.data$CamNumber1 == "T6" |
                                ind.data$CamNumber1 == "T7" |
                                ind.data$CamNumber1 == "T8" |
                                ind.data$CamNumber1 == "T9" |
                                ind.data$CamNumber1 == "T10" |
                                ind.data$CamNumber1 == "T11" |
                                ind.data$CamNumber1 == "T12" |
                                ind.data$CamNumber1 == "T13" |
                                ind.data$CamNumber1 == "T14" |
                                ind.data$CamNumber1 == "T25" |
                                ind.data$CamNumber2 == "T1" |
                                ind.data$CamNumber2 == "T2" |
                                ind.data$CamNumber2 == "T3" |
                                ind.data$CamNumber2 == "T4" |
                                ind.data$CamNumber2 == "T5" |
                                ind.data$CamNumber2 == "T6" |
                                ind.data$CamNumber2 == "T7" |
                                ind.data$CamNumber2 == "T8" |
                                ind.data$CamNumber2 == "T9" |
                                ind.data$CamNumber2 == "T10" |
                                ind.data$CamNumber2 == "T11" |
                                ind.data$CamNumber2 == "T12" |
                                ind.data$CamNumber2 == "T13" |
                                ind.data$CamNumber2 == "T14" |
                                ind.data$CamNumber2 == "T25"
                            ),
                          yes = TRUE,
                          no = ifelse(
                            test = ind.data$Survey.Name == "Savegre Valley Summer 2013" & #Savegre SU13 - low 
                              (
                                ind.data$CamNumber1 == "13" |
                                  ind.data$CamNumber1 == "20" |
                                  ind.data$CamNumber1 == "32" |
                                  ind.data$CamNumber1 == "28" |
                                  ind.data$CamNumber1 == "29" |
                                  ind.data$CamNumber1 == "31" |
                                  ind.data$CamNumber1 == "23" |
                                  ind.data$CamNumber1 == "25" |
                                  ind.data$CamNumber1 == "21" |
                                  ind.data$CamNumber1 == "22" |
                                  ind.data$CamNumber1 == "26" |
                                  ind.data$CamNumber1 == "27" |
                                  ind.data$CamNumber2 == "13" |
                                  ind.data$CamNumber2 == "20" |
                                  ind.data$CamNumber2 == "32" |
                                  ind.data$CamNumber2 == "28" |
                                  ind.data$CamNumber2 == "29" |
                                  ind.data$CamNumber2 == "31" |
                                  ind.data$CamNumber2 == "23" |
                                  ind.data$CamNumber2 == "25" |
                                  ind.data$CamNumber2 == "21" |
                                  ind.data$CamNumber2 == "22" |
                                  ind.data$CamNumber2 == "26" |
                                  ind.data$CamNumber2 == "27"
                              ),
                            yes = FALSE,
                            no = ifelse(
                              test = ind.data$Survey.Name == "Savegre Valley Summer 2013" & #Savegre SU13 - high
                                (
                                  ind.data$CamNumber1 == "1" |
                                    ind.data$CamNumber1 == "6" |
                                    ind.data$CamNumber1 == "7" |
                                    ind.data$CamNumber1 == "40" |
                                    ind.data$CamNumber1 == "41" |
                                    ind.data$CamNumber1 == "42" |
                                    ind.data$CamNumber1 == "43" |
                                    ind.data$CamNumber2 == "1" |
                                    ind.data$CamNumber2 == "6" |
                                    ind.data$CamNumber2 == "7" |
                                    ind.data$CamNumber2 == "40" |
                                    ind.data$CamNumber2 == "41" |
                                    ind.data$CamNumber2 == "42" |
                                    ind.data$CamNumber2 == "43"
                                ),
                              yes = TRUE,
                              no = ifelse(
                                test = ind.data$Survey.Name == "Pejibaye Summer 2013" & #Pejibaye SU13 - low
                                  (
                                    ind.data$CamNumber1 == "EC13" |
                                      ind.data$CamNumber1 == "EC14" |
                                      ind.data$CamNumber1 == "EC301" |
                                      ind.data$CamNumber1 == "EC302" |
                                      ind.data$CamNumber1 == "LM1" |
                                      ind.data$CamNumber1 == "LM2" |
                                      ind.data$CamNumber1 == "LM4" |
                                      ind.data$CamNumber2 == "EC13" |
                                      ind.data$CamNumber2 == "EC14" |
                                      ind.data$CamNumber2 == "EC301" |
                                      ind.data$CamNumber2 == "EC302" |
                                      ind.data$CamNumber2 == "LM1" |
                                      ind.data$CamNumber2 == "LM2" |
                                      ind.data$CamNumber2 == "LM4"
                                  ),
                                yes = FALSE,
                                no = ifelse(
                                  test = ind.data$Survey.Name == "Tapanti Summer 2013" & #Tapanti SU13 - low
                                    (
                                      ind.data$CamNumber1 == "T3" |
                                        ind.data$CamNumber1 == "T4" |
                                        ind.data$CamNumber1 == "T7" |
                                        ind.data$CamNumber1 == "T8" |
                                        ind.data$CamNumber1 == "T9" |
                                        ind.data$CamNumber1 == "T10" |
                                        ind.data$CamNumber1 == "T15" |
                                        ind.data$CamNumber1 == "T16" |
                                        ind.data$CamNumber1 == "T19" |
                                        ind.data$CamNumber1 == "T20" |
                                        ind.data$CamNumber2 == "T3" |
                                        ind.data$CamNumber2 == "T4" |
                                        ind.data$CamNumber2 == "T7" |
                                        ind.data$CamNumber2 == "T8" |
                                        ind.data$CamNumber2 == "T9" |
                                        ind.data$CamNumber2 == "T10" |
                                        ind.data$CamNumber2 == "T15" |
                                        ind.data$CamNumber2 == "T16" |
                                        ind.data$CamNumber2 == "T19" |
                                        ind.data$CamNumber2 == "T20"
                                    ),
                                  yes = FALSE,
                                  no = ifelse(
                                    test = ind.data$Survey.Name == "Savegre Valley Fall 2013" & #Savegre FA13 - high
                                      (
                                        ind.data$CamNumber1 == "1" |
                                          ind.data$CamNumber1 == "2" |
                                          ind.data$CamNumber1 == "3" |
                                          ind.data$CamNumber1 == "4" |
                                          ind.data$CamNumber1 == "6" |
                                          ind.data$CamNumber1 == "7" |
                                          ind.data$CamNumber1 == "40" |
                                          ind.data$CamNumber1 == "41" |
                                          ind.data$CamNumber1 == "42" |
                                          ind.data$CamNumber1 == "43" |
                                          ind.data$CamNumber2 == "1" |
                                          ind.data$CamNumber2 == "2" |
                                          ind.data$CamNumber2 == "3" |
                                          ind.data$CamNumber2 == "4" |
                                          ind.data$CamNumber2 == "6" |
                                          ind.data$CamNumber2 == "7" |
                                          ind.data$CamNumber2 == "40" |
                                          ind.data$CamNumber2 == "41" |
                                          ind.data$CamNumber2 == "42" |
                                          ind.data$CamNumber2 == "43"
                                      ),
                                    yes = TRUE,
                                    no = ifelse(
                                      test = ind.data$Survey.Name == "Savegre Valley Fall 2013" & #Savegre FA13 low
                                        (
                                          ind.data$CamNumber1 == "26" |
                                            ind.data$CamNumber1 == "27" |
                                            ind.data$CamNumber2 == "26" |
                                            ind.data$CamNumber2 == "27"
                                        ),
                                      yes = FALSE,
                                      no = NA #Still have part of FA13 to finish
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )
)