context("player responsibility")

test_that("do_responsibility_posterior copes with previous skills that have no coordinates", {
    ## e.g. a reception row left without coordinates (seen after experimenting with undo while click-scouting)
    history <- data.frame(team = "*", player_number = c(4, 4, 2),
                          start_coordinate_x = c(1, NA, 2), start_coordinate_y = c(1.5, NA, 2),
                          end_coordinate_x = c(2, 2, NA), end_coordinate_y = c(5, 5, NA),
                          home_p1 = 4, home_p2 = 2, home_p3 = 3, home_p4 = 18, home_p5 = 13, home_p6 = 25)
    game_state <- list(start_x = 1.5, start_y = 1.8, end_x = 2, end_y = 5, home_team_end = "lower", current_team = "*")
    prior <- setNames(rep(1/7, 7), c(paste0("home_p", 1:6), "libero"))
    for (se in c("start", "end")) {
        post <- do_responsibility_posterior(history, game_state = game_state, prior = prior, home_visiting = "home", weighted = TRUE, current_team = "*", start_end = se, pseq = 1:6)
        expect_false(any(is.na(unlist(post))))
        expect_equal(sum(unlist(post)), 1)
    }
    ## all previous skills lacking coordinates should give back the prior
    history$start_coordinate_x <- NA_real_
    post <- do_responsibility_posterior(history, game_state = game_state, prior = prior, home_visiting = "home", weighted = TRUE, current_team = "*", start_end = "start", pseq = 1:6)
    expect_equal(unlist(post), prior)
})
