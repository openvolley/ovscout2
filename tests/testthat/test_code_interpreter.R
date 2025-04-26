context("interpretation of scouted codes")

test_that("code interpretation works correctly", {
    trim <- function(z) sub("~+$", "", z)
    check_code <- function(tocheck, expected) {
        e20 <- !grepl("^(>|\\*T|aT|\\*p|ap|\\*c|ac|\\*C|aC|\\*P|aP)", tocheck) ## expect skill rows to be length 20
        if (any(e20)) expect_equal(nchar(tocheck[e20]), rep(20L, sum(e20)))
        expect_equal(trim(tocheck), expected)
    }
    check_code(ov_code_interpret("a10BH-"), "a10BH-")
    check_code(ov_code_interpret("a10/"), "a10DH/") ## default skill, skill_type
    check_code(ov_code_interpret("a10"), "a10DH+") ## default skill, skill_type, eval
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
    check_code(ov_code_interpret("2X5#5S"), "*02AT#X5~45~H~S") ## Include special syntax
    check_code(ov_code_interpret("a6ETKP"), "a06ET+KP") ## Setting?
    check_code(ov_code_interpret("2EHKPF4"), "*02EH+KPF~4") ## Setting?
    check_code(ov_code_interpret("2HK1"), "*02EH+K1") ## Setting?
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
