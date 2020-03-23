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
    expect_failure(expect_equal(trim(ov_code_interpret("*2AT#452")), "*02AT#~~~45~H2")) ## skip the skill_type H. Can't handle this yet
    check_code(ov_code_interpret("*2AT#45P2"), "*02AT#~~~45~P2")
    check_code(ov_code_interpret("*5X5!"), "*05AT!X5F4")
    check_code(ov_code_interpret("5X5!"), "*05AT!X5F4")
    check_code(ov_code_interpret("5X5"), "*05AT+X5F4") ## default evaluation
    expect_failure(expect_equal(trim(ov_code_interpret("5X55")), "*05AT+X5F45")) ## just the player number, attack combo and end cone/zone. Can't handle this yet
    check_code(ov_code_interpret("*5X5+5H2"), "*05AT+X5F45~H2")
    check_code(ov_code_interpret("*5X55H2"), "*05AT+X5F4")
    check_code(ov_code_interpret("a12SM.2#78"), c("a12SM-~~~78", "*02RM#~~~78"))
    check_code(ov_code_interpret("*2X5.15=5H4"), c("*02AT#X5F45~H4", "a15BT=~~~~5~~4"))
    expect_failure(expect_equal(trim(ov_code_interpret("2AT#452")), "*02AT#~~~45~H2")) ## currently this gives "*02AT#~~~45~2", missing the skill type H and the trailing 2 is in the wrong slot
    check_code(ov_code_interpret("2AT#45t1"), "*02AT#~~~45~T1")
    check_code(ov_code_interpret("2X5#.15"), c("*02AT#X5F4", "a15BT=")) ## block error is the default on attack kill
    expect_failure(expect_equal(trim(ov_code_interpret("2X5#.15D")), c("*02AT#X5F4", "a15DT="))) ## but we can specify e.g. dig error. Can't handle this yet
    check_code(ov_code_interpret("2SQ1.5=4"), c("*02SQ#~~~14", "a05RQ=~~~14"))
})

## test cases to think about:
## should "2X5#5S" be "*02AT#X5F45" - i.e. should the trailing S be ignored? it's not valid for attacks, at least in the default tables
