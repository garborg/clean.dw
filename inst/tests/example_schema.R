loadEgSchema = function() {
    TABLE_SCHEMA <<- list(
        crufty_db1.crfttabl_2z = c(
            id          = "crfty_id_mstr",
            name        = "lower(trim(trailing from crft_shrt_nm))",
            cost        = "curr_avg_cst",
            category    = "cat"
        ),
        dw2.tb_react = c(
            date        = "cast(reg_dt as date format 'yyyy-mm-dd')",
            id          = "target_nbr",
            place       = "plc_cd",
            action      = "trs_cd mod 4",
            reaction    = "floor(trs_cd / 7)",
            kpi1        = "asdf",
            kpi2        = "fdas"
        ),
        dw2.tb_plc = c(
            place       = "cast(substring(full_place, 1, 3) as integer)",
            size        = "char_bh",
            cost        = "cost"
        ),
        dw2.tb_plc2 = c(
            place       = "test_key mod 1000",
            test        = "floor(test_key / 1000)",
            test_name   = "test"
        )
    )

    VIEW_SCHEMA <<- list(
        "@place" = list(
            dw2.tb_plc = list(),
            dw2.tb_plc2 = list(
                join = list(type="left", on="place", lazy=TRUE))
        ),
        "@reactions" = list(
            dw2.tb_react = list(
                where = AND(action = list("between", c(2, 5)))),
            crufty_db1.crfttabl_2z = list(
                where = AND(id = list(">", 99),
                            cost = list("!=", NULL)),
                join = list(type="inner", on="id")),
            "@place" = list(
                hide = "cost",
                join = list(type="inner", on="place"))
        )
    )

    validateViews()
}

loadEgSchema()
