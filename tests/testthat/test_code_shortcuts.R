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

test_that("shortcut creation works", {
    def <- ov_default_type_shortcuts()
    sc <- ov_shortcuts("type")
    expect_equal(sc, def)

    def <- ov_default_playstable_shortcuts()
    sc <- ov_shortcuts("playstable")
    expect_equal(sc, def)

    def <- ov_default_click_shortcuts()
    sc <- ov_shortcuts("click")
    expect_equal(sc, def)

    ## re-assign j to pause
    sc <- ov_shortcuts("click", pause = "j")
    expect_equal(sc[!names(sc) %in% c("pause", "video_rewind_2")], def[!names(def) %in% c("pause", "video_rewind_2")])
    expect_equal(sc$pause, c(def$pause, "j"))
    expect_equal(sc$video_rewind_2, setdiff(def$video_rewind_2, "j"))
})
