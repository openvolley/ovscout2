code_bits_tbl <- dplyr::tribble(~bit, ~width,
                                "team", 1,
                                "number", 2,
                                "skill", 1,
                                "type", 1,
                                "eval", 1,
                                "combo", 2,
                                "target", 1,
                                "start_zone", 1,
                                "end_zone", 1,
                                "end_subzone", 1,
                                "skill_type", 1,
                                "num_players", 1,
                                "special", 1,
                                "custom", 5)
## some local markup to make the helper entries easier here
## | = <br />
## {thing} = <strong>thing</strong>
## [thing] = <span class=\"clet\">thing</span>
## (thing) = <em>thing</em>
## --- = <br /><hr />
gsubf <- function(...) gsub(..., fixed = TRUE)
mu2html <- function(z) gsubf("[", "<span class=\"clet\">", gsubf("]", "</span>", gsubf("{", "<strong>", gsubf("}", "</strong>", gsubf("|", "<br />", gsubf("(", "<em>", gsubf(")", "</em>", gsubf("---", "<br /><hr />", z))))))))

build_code_entry_guide <- function(mode, thisrow) {
    mode <- match.arg(mode, c("edit", "insert"))
    bitstbl <- code_bits_tbl
    if (mode %eq% "edit" && is_skill(thisrow$skill)) {
        ## only with skill, not timeout/sub/etc
        thiscode <- thisrow$code
        bitstbl$value <- vapply(seq_len(nrow(bitstbl)), function(z) substr(thiscode, bitstbl$start[z], bitstbl$end[z]), FUN.VALUE = "", USE.NAMES = FALSE)
    } else {
        bitstbl$value <- ""
    }
    bitstbl$value <- gsub("~", "", bitstbl$value)
    cbitInput <- function (bitname, value = "", width = 2, helper = "") {
        tags$div(style = paste0("display:inline-block; vertical-align:top;"), tags$input(id = paste0("code_entry_", bitname), type = "text", value = value, size = width, maxlength = width, class = "input-small"),
                 ##HTML(paste0("<input id=\"code_entry_", bitname, "\" type=\"text\" value=\"", value, "\" size=\"", width, "\" maxlength=\"", width, "\" class=\"input-small\"", if (bitname == "end_zone") " autofocus=\"autofocus\"", " />")),
                 tags$div(class = "code_entry_guide", helper))
    }
    tags$div(style = "padding: 8px;", do.call(shiny::fixedRow, lapply(seq_len(nrow(bitstbl)), function(z) {
        this_skill <- bitstbl$value[bitstbl$bit %eq% "skill"]
        this_ev <- bitstbl$value[bitstbl$bit %eq% "eval"]
        cbitInput(bitstbl$bit[z], value = bitstbl$value[z], width = bitstbl$width[z], helper = if (is.function(bitstbl$helper[[z]])) uiOutput(paste0("code_entry_helper_", bitstbl$bit[z], "_ui")) else HTML(bitstbl$helper[[z]]))
    })))
}

paste0_noNA <- function(...) do.call(paste0, Filter(Negate(is.na), list(...)))
special_helper <- function(skill, evaln) {
    htext <- NA_character_
    if (!is.null(skill) && !is.null(evaln)) {
        htext <- case_when(skill %eq% "A" & evaln %eq% "#" ~ "(Attk kill)|Blk out [S]ide|Blk out l[O]ng|Blk on [F]loor|[X] Direct|on floor",
                           skill %eq% "A" & evaln %eq% "=" ~ "(Attk err)|Out [S]ide|Out l[O]ng|In [N]et|[I] net contct|[A]ntenna|[Z] ref call",
                           skill %eq% "A" ~ "(Attk)|blk [C]ontrol|[N] let",
                           skill %eq% "B" & evaln %in% c("=", "/") ~ "(Blk err)|Out [S]ide|Out l[O]ng|Ball on [F]lr|[X] between|hands|[N] net touch|[A]ntenna|[P] no jump|[T] pos error|[Z] ref call",
                           skill %eq% "R" ~ "(Rcv)|[U]nplayable|[X] body err|[P]os err|No [E]ffort|[Z] ref call",
                           skill %eq% "S" & evaln %eq% "#" ~ "(Srv ace)|[N] let",
                           skill %eq% "S" & evaln %eq% "=" ~ "(Srv err)|Out l[O]ng|Out [L]eft|Out [R]ight|In [N]et|[Z] ref call",
                           skill %eq% "S" ~ "(Srv)|[N] let",
                           skill %eq% "E" & evaln %eq% "=" ~ "(Set err)|[U]nhittable|[I] net tch|[Z] ref call",
                           skill %eq% "Dig" & evaln %eq% "=" ~ "(Dig err)|[U]nplayable|[X] body err|[P]os err|[Z] Ref call|Ball on [F]lr|Ball [O]ut|No [E]ffort",
                           skill %eq% "Freeball" & evaln %eq% "=" ~ "(Fr err)|[U]nplayable|[X] body err|[P]os err|[Z] Ref call")
    }
    mu2html(paste0_noNA("{Special}---", htext))
}
skill_type_helper <- function(skill, evaln) {
    htext <- NA_character_
    if (!is.null(skill)) {
        htext <- case_when(skill %eq% "A" ~ "(Attk)|[H]ard|[P] soft|[T]ip",
                           skill %eq% "R" ~ "(Rec)|[L]eft|[R]ight|lo[W]|[O]vrhnd|[M]idline",
                           skill %eq% "E" ~ "(Set)|[1] hand|[2] hands|[3] bump|[4] othr|[5] uhand",
                           skill %eq% "D" ~ "(Dig)|[S] on spk|[C] spk|cover|[B] aftr|block|[E] emerg")
    }
    mu2html(paste0_noNA("{Skill|type}---", htext))
}
num_players_helper <- function(skill, evaln) {
    htext <- NA_character_
    if (!is.null(skill)) {
        htext <- case_when(skill %in% c("A", "B") ~ "(Attk|Blk)|[0]..[3]|[4] hole|block",
                           skill %eq% "R" ~ "(Rcv)|[1] 2p,L|[2] 2p,R|[3] 3p,L|[4] 3p,M|[5] 3p,R|[6] 4p,L|[7] 4p,LC|[8] 4p,RC|[9] 4p,R")
    }
    mu2html(paste0_noNA("{Num|plyrs}---", htext))
}
end_zone_helper <- function(skill, evaln, dvw) {
    if (!is.null(skill) && skill %eq% "A" && dvw$meta$match$zones_or_cones %eq% "C") {
        mu2html("{End cone}|(Attk)|[1..8]")
    } else {
        mu2html("{End zone}---[1..9]")
    }
}
code_bits_tbl$helper <- c(mu2html("{Team}---[*]&nbsp;H|[a]&nbsp;V"), ## team
                          mu2html("{Plyr|num}"), ## number
                          mu2html("{Skill}---[S]rv|[R]ec|[A]ttk|[B]lk|[D]ig|s[E]t|[F]reeb"), ## skill
                          mu2html("{Tempo}---[H]igh|[M]ed|[Q]uick|[T]ense|s[U]per|[N] fast|[O]ther"), ## type
                          mu2html("{Eval}---[#|+|!|-|/|=]"), ## eval
                          mu2html("{Combo}---(Atk code)|[X.]|[C.]|etc||(Set call)|[K.]"), ## combo
                          mu2html("{Target}---[F]ront|[C]ntr|[B]ack|[P]ipe|[S]etr"), ## target
                          mu2html("{Start|zone}---(Attk)|[1..9]||(Srv)|[57691]"), ##start_zone
                          end_zone_helper, ##end_zone
                          mu2html("{End|subzn}---[ABCD]"), ##end_subzone
                          skill_type_helper, ##skill_type
                          num_players_helper, ##players
                          special_helper, ##special
                          mu2html("{Custom}---")) ##custom
## note that if any other helpers are turned into functions, they need extra code added below to handle them (see ADD HANDLERS HERE)
code_bits_tbl$start <- cumsum(lag(code_bits_tbl$width, default = 0))+1L
code_bits_tbl$end <- code_bits_tbl$start+code_bits_tbl$width-1L
