context("shortcut handling")

test_that("matching of key combos to shortcuts works", {
    ## plain ArrowRight key should not match a shortcut defined as Alt-ArrowRight
    expect_false(is_shortcut(decode_keypress("false|false|false|false|ArrowRight|@@@undefined@-1@1746320851350"), "Alt-ArrowRight"))
    ## but should match a shortcut defined as ArrowRight
    expect_true(is_shortcut(decode_keypress("false|false|false|false|ArrowRight|@@@undefined@-1@1746320851350"), "ArrowRight"))

    ## "B" should match "B" but "b" should not
    expect_true(is_shortcut(decode_keypress("false|false|true|false|B|@@@undefined@-1@1746320851350"), "B"))
    expect_false(is_shortcut(decode_keypress("false|false|false|false|b|@@@undefined@-1@1746320851350"), "B"))
})
