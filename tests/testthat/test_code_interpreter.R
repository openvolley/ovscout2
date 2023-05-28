context("interpretation of scouted codes")

test_that("code interpretation works correctly", {
    trim <- function(z) sub("~+$", "", z)
    check_code <- function(tocheck, expected) {
        expect_equal(nchar(tocheck), rep(20L, length(tocheck)))
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
    check_code(ov_code_interpret("2X5#.15D"), c("*02AT#X5~4", "a15DT=")) ## but we can specify e.g. dig error.
    check_code(ov_code_interpret("2SQ1.5=4"), c("*02SQ#~~~14", "a05RQ=~~~14"))
    check_code(ov_code_interpret("2X5#5S"), "*02AT#X5~45~H~S") ## Include special syntax
    check_code(ov_code_interpret("a6ETKP"), "a06ET+KP") ## Setting?
    check_code(ov_code_interpret("2EHKPF4"), "*02EH+KPF~4") ## Setting?
    check_code(ov_code_interpret("2HK1"), "*02EH+K1") ## Setting?
    check_code(ov_code_interpret("a37ET-KZB9C2"), "a37ET-KZB~9C2") ## with a custom Kx setting code
    check_code(ov_code_interpret("*02EH+KPF4"), "*02EH+KPF~4") ## Check that a proper code is left unchanged (v1)
    check_code(ov_code_interpret("*05AT+X5~45~H2"), "*05AT+X5~45~H2")  ## Check that a proper code is left unchanged (v2)
})
