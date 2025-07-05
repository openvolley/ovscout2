context("match file creation")

test_that("dv_create works correctly", {

    expect_warning(x <- dv_create(teams = c("Home team", "Visiting team")), "less than 6 players")
    expect_error(x <- dv_create(teams = c("Home team", "Home team")), "team_ids must be unique")
    expect_error(x <- dv_create(teams = tibble(team_id = c("TMH", "TMH"), team = c("Home team", "Visiting team"))), "team_ids must be unique")

    teams <- tibble(team_id = c("TMA", "TMB"), team = c("Team A", "Team B"))
    expect_warning(x <- dv_create(teams = teams), "less than 6 players")

    players_h <- tribble(~player_id, ~lastname, ~firstname, ~number, ~role, ~special_role,
                         "PH-A", "HA", "a", 1, "setter", "",
                         "PH-B", "HB", "b", 2, "outside", "",
                         "PH-C", "HC", "c", 3, "middle", "",
                         "PH-D", "HD", "d", 7, "middle", "",
                         "PH-E", "HE", "e", 8, "opposite", "",
                         "PH-F", "HF", "f", 11, "outside", "C",
                         "PH-G", "HG", "g", 14, "libero", "L")

    players_v <- tribble(~player_id, ~lastname, ~firstname, ~number, ~role, ~special_role,
                         "PV-A", "VA", "a", 4, "outside", "C",
                         "PV-B", "VB", "b", 9, "opposite", "",
                         "PV-C", "VC", "c", 15, "outside", "",
                         "PV-D", "VD", "d", 20, "middle", "",
                         "PV-E", "VE", "f", 21, "middle", "",
                         "PV-F", "VF", "f", 22, "setter", "",
                         "PV-G", "VG", "g", 40, "outside", "")

    x <- dv_create(teams = teams, players_h = players_h, players_v = players_v)
    expect_error(x <- dv_create(teams = teams, players_h = rbind(players_h, players_h[1, ]), players_v = players_v), "duplicated player_id")
    expect_error(x <- dv_create(teams = teams, players_h = players_h, players_v = rbind(players_v, players_v[2, ])), "duplicated player_id")
    ## but duplicated player IDs on different teams is OK in typing mode, so allow it
    x <- dv_create(teams = teams, players_h = players_h, players_v = players_h)



    x <- dv_set_lineups(x, set_number = 1, lineups = list(c(11, 3, 8, 2, 7, 1), c(22, 15, 20, 9, 4, 21)), setter_positions = c(6, 1))

})
