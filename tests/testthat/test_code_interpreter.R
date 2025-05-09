context("interpretation of scouted codes")

test_that("code interpretation works correctly", {
    trim <- function(z) sub("~+$", "", z)
    check_code <- function(tocheck, expected) {
        e20 <- !grepl("^(>|\\*T|aT|\\*p|ap|\\*c|ac|\\*C|aC|\\*P|aP)", tocheck) ## expect skill rows to be length 20
        if (any(e20)) expect_equal(nchar(tocheck[e20]), rep(20L, sum(e20)))
        expect_equal(trim(tocheck), expected)
    }
    expect_equal(ov_code_interpret(""), "")
    expect_equal(ov_code_interpret("10"), "")
    expect_equal(ov_code_interpret("+"), "")
    expect_equal(ov_code_interpret("5!"), "")
    expect_equal(ov_code_interpret("5!.3"), "")
    check_code(ov_code_interpret("a10BH-"), "a10BH-")
    ## check_code(ov_code_interpret("a10/"), "a10DH/") ## default skill, skill_type
    ## check_code(ov_code_interpret("a10"), "a10DH+") ## default skill, skill_type, eval
    ## changed behaviour May 2025
    expect_equal(ov_code_interpret("a10/"), "")
    expect_equal(ov_code_interpret("a10"), "")
    check_code(ov_code_interpret("01A.5"), c("*01AH-", "a05BH+"))
    check_code(ov_code_interpret("01F.5"), c("*01FH-", "a05FH+")) ## freeball-over to freeball-dig combo
    check_code(ov_code_interpret("*2AT#45H2"), "*02AT#~~~45~H2")
    check_code(ov_code_interpret("*2AT#452"), "*02AT#~~~45~H2")
    check_code(ov_code_interpret("*2AT#45P2"), "*02AT#~~~45~P2")
    check_code(ov_code_interpret("*5X5!"), "*05AT!X5~4~~H")
    check_code(ov_code_interpret("5X5!"), "*05AT!X5~4~~H")
    check_code(ov_code_interpret("5X5"), "*05AT+X5~4~~H") ## default evaluation
    check_code(ov_code_interpret("5X55"), "*05AT+X5~45~H")
    check_code(ov_code_interpret("*5X5+5H2"), "*05AT+X5~45~H2")
    check_code(ov_code_interpret("*5X55H2"), "*05AT+X5~45~H2")
    check_code(ov_code_interpret("a12SM.2#78"), c("a12SM-~~~78", "*02RM#~~~78"))
    check_code(ov_code_interpret("*2X5.15=5H4"), c("*02AT#X5~45~H4", "a15BT=~~~~5~~4"))
    check_code(ov_code_interpret("x8.11d/"), c("*~~AT+X8~9", "a11DT/")) ## rotation unknown so no attack player number
    check_code(ov_code_interpret("2AT#452"), "*02AT#~~~45~H2")
    check_code(ov_code_interpret("2AT#45t1"), "*02AT#~~~45~T1")
    check_code(ov_code_interpret("2X5#.15"), c("*02AT#X5~4", "a15BT=")) ## block error is the default on attack kill
    check_code(ov_code_interpret("2X5#.15B"), c("*02AT#X5~4", "a15BT="))
    check_code(ov_code_interpret("2X5#.15D"), c("*02AT#X5~4", "a15DT=")) ## but we can specify e.g. dig error.
    check_code(ov_code_interpret("2X5.15="), c("*02AT#X5~4", "a15BT=")) ## can specify the evaluation (error) on the block instead of the kill on the attack
    check_code(ov_code_interpret("2AH#.15"), c("*02AH#", "a15BH=")) ## same should work on attack skill (not using attack compound code)
    check_code(ov_code_interpret("2AH#.15B"), c("*02AH#", "a15BH="))
    check_code(ov_code_interpret("2AH#.15D"), c("*02AH#", "a15DH="))
    check_code(ov_code_interpret("2SQ1.5=4"), c("*02SQ#~~~14", "a05RQ=~~~14"))
    check_code(ov_code_interpret("*05SM15.2"), c("*05SM-~~~15", "a02RM+~~~15"))
    check_code(ov_code_interpret("*05SM15.2+"),c("*05SM-~~~15", "a02RM+~~~15"))
    check_code(ov_code_interpret("2X5#5S"), "*02AT#X5~45~H~S") ## Include special syntax
    check_code(ov_code_interpret("a6ETKP"), "a06ET+KP") ## Setting
    check_code(ov_code_interpret("2EHKPF4"), "*02EH+KPF~4") ## Setting
    check_code(ov_code_interpret("2HK1"), "*02EH+K1") ## Setting
    check_code(ov_code_interpret("a2HK1"), "a02EH+K1") ## Setting
    check_code(ov_code_interpret("aK1", visiting_setter_num = 2), "a02EH+K1") ## Setting
    check_code(ov_code_interpret("k1", home_setter_num = 7, visiting_setter_num = 2, serving_team = "a"), "*07EH+K1") ## team is inferred for the set action
    check_code(ov_code_interpret("*k1", home_setter_num = 7, visiting_setter_num = 2, serving_team = "a"), "*07EH+K1") ## explicit team assignment on setter call
    check_code(ov_code_interpret("k1", home_setter_num = 7, visiting_setter_num = 2, serving_team = "*"), "a02EH+K1") ## team is inferred for the set action
    check_code(ov_code_interpret("a37ET-KZB9C2"), "a37ET-KZB~9C2") ## with a custom Kx setting code
    check_code(ov_code_interpret("*02EH+KPF4"), "*02EH+KPF~4") ## Check that a proper code is left unchanged (v1)
    check_code(ov_code_interpret("*05AT+X5~45~H2"), "*05AT+X5~45~H2")  ## Check that a proper code is left unchanged (v2)
    ## space-separated
    check_code(ov_code_interpret("*05AT+X5~45~H2 2HK1"), c("*05AT+X5~45~H2", "*02EH+K1"))
    ## setter call without player number
    check_code(ov_code_interpret("K1"), "*00EH+K1")
    check_code(ov_code_interpret("aK1"), "a00EH+K1")
    check_code(ov_code_interpret("*K1", home_setter_num = 14), "*14EH+K1")
    check_code(ov_code_interpret("*K1", home_setter_num = NA), "*00EH+K1")
    check_code(ov_code_interpret("*K1", home_setter_num = 123), "*00EH+K1")
    check_code(ov_code_interpret("*K1", home_setter_num = -1), "*00EH+K1")
    ## non-skill codes
    check_code(ov_code_interpret("aT T ac3:2 c8.5 a2SM57.16="), c("aT", "*T", "ac3:2", "*c8.5", "a02SM#~~~57", "*16RM=~~~57"))
})

test_that("lineup code parsing works", {
    expect_equal(ovscout2:::split_lineup_codes("L1 2 3 4 5s 6 aL 3 4 5p 6 7 10"), c("L1 2 3 4 5s 6", "aL 3 4 5p 6 7 10"))
    expect_equal(ovscout2:::split_lineup_codes("L1 2 3 4 5s 6 aL3,4,5p,6,7,10"), c("L1 2 3 4 5s 6", "aL3,4,5p,6,7,10"))
    expect_equal(ovscout2:::split_lineup_codes("L 1 2 3 4 5s 6 aL3,4,5p,6,7,10"), c("L 1 2 3 4 5s 6", "aL3,4,5p,6,7,10"))
    expect_equal(ovscout2:::split_lineup_codes("  L1,2,3,4,5s,6 aL3,4,5p,6,7,10"), c("L1,2,3,4,5s,6", "aL3,4,5p,6,7,10"))
    expect_equal(ovscout2:::split_lineup_codes("  L1,2,3,4,5s,6aL3,4,5p,6,7,10"), c("L1,2,3,4,5s,6", "aL3,4,5p,6,7,10")) ## not recommended, but we can cope with it!
    expect_equal(ovscout2:::split_lineup_codes("  L1,2,3,4,5s,6 "), c("L1,2,3,4,5s,6"))


    expect_equal(ovscout2:::lineup_preprocess("L1 2 3 4 5s 6 aL 3 4 5p 6 7 10 11", beach = FALSE),
                 list(home = list(lineup = c(1L, 2L, 3L, 4L, 5L, 6L), setter = 5L, liberos = integer()),
                      visiting = list(lineup = c(3L, 4L, 5L, 6L, 7L, 10L), setter = 5L, liberos = 11L)))
    expect_equal(ovscout2:::lineup_preprocess(" L  1 2 3 4 5s 6 aL 3 4 5p 6 7 10 11", beach = FALSE),
                 list(home = list(lineup = c(1L, 2L, 3L, 4L, 5L, 6L), setter = 5L, liberos = integer()),
                      visiting = list(lineup = c(3L, 4L, 5L, 6L, 7L, 10L), setter = 5L, liberos = 11L)))
    expect_warning(ovscout2:::lineup_preprocess("L1 2 3 4 5s 6 aL 3 4 5p 6 7 10 11", beach = TRUE), "invalid")
    expect_warning(ovscout2:::lineup_preprocess("L 1 2 aL 3 4 5p 6 7 10 11", beach = TRUE), "invalid")
    expect_warning(ovscout2:::lineup_preprocess("L aL 3 4 5p 6 7 10 11", beach = TRUE), "invalid")
    expect_equal(ovscout2:::lineup_preprocess("L1 2 aL 3 4", beach = TRUE),
                 list(home = list(lineup = c(1L, 2L), setter = NA_integer_, liberos = integer()),
                      visiting = list(lineup = c(3L, 4L), setter = NA_integer_, liberos = integer())))
    expect_equal(ovscout2:::lineup_preprocess("L1p 2 aL 3 4", beach = TRUE),
                 list(home = list(lineup = c(1L, 2L), setter = NA_integer_, liberos = integer()),
                      visiting = list(lineup = c(3L, 4L), setter = NA_integer_, liberos = integer())))
})
