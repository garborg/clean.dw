tableFields = function(table) {
    switch( table, 
        crufty_db1.crfttabl_2z = c(
            id = 'crfty_id_mstr',
            name = 'lower(trim(trailing from crft_shrt_nm))',
            cost = 'curr_avg_cst'
        ),
        dw2.tb_react = c(
            date = "cast(reg_dt as date format 'yyyy-mm-dd')",
            id = 'target_nbr',
            place = 'plc_cd',
            action = 'trs_cd mod 4',
            reaction = 'floor(trs_cd / 7)',
            kpi1 = 'asdf',
            kpi2 = 'fdas'
        ),
        dw2.tb_plc = c(
            place = 'cast(substring(full_place, 1, 3) as integer)',
            size = 'char_bh',
            cost = 'cost'
        ),
        dw2.tb_plc2 = c(
            place = 'test_key mod 1000',
            test = 'floor(test_key / 1000)',
            test_name = 'test'
        )
    )
}

viewSpec = function(name) {
    switch( name,
        '@place' = list(
            dw2.tb_plc = list(),
            dw2.tb_plc2 = list(join = list(type='left', on='place', lazy=TRUE))
        ),
        '@reactions' = list(
            dw2.tb_react = list(where = list(action=list('between', c(2, 5)))),
            crufty_db1.crfttabl_2z = list(
                where = list(id=list('>', 99), cost=list('!=', NULL)),
                join = list(type='inner', on='id')
            ),
            '@place' = list(hide = 'cost', join = list(type='inner', on='place'))
        )
    )
}

stripQuery = function(x) {
    gsub("^[ ]|[ ]$", "", gsub("[\r\n\t ]+", " ", x))
}

queriesEqual = function(x, y) {
    stripQuery(x) == stripQuery(y)
}


today = as.Date("2013-10-31")
last_months_end = today - as.POSIXlt(today)$mday
w = AND(date = list('>', last_months_end),
         cost = list('<', 10))
w_place = AND(size = 'big')
w_test = AND(test = c(1, 2, 7))

test_that("SQL handles raw table", {
    ref = "SELECT
       target_nbr AS \"id\"
       FROM
       dw2.tb_react"

    q = SQL(select="id", from="dw2.tb_react")

    expect_true(queriesEqual(q, ref))
})

test_that("SQL handles simple view", {
    ref = "SELECT
       a.\"place\"
       FROM
       (
          SELECT
             cast(substring(full_place, 1, 3) as integer) AS \"place\"
          FROM
             dw2.tb_plc
       ) AS a
       LEFT JOIN (
          SELECT
             test_key mod 1000 AS \"place\"
          FROM
             dw2.tb_plc2
          WHERE
             floor(test_key / 1000) in (1, 2, 7)
       ) AS b
       ON
          a.\"place\" = b.\"place\""

    q = SQL(select='place', from='@place', where=w_test)

    expect_true(queriesEqual(q, ref))
})

test_that("SQL handles lazy join", {
    ref = "SELECT
       cast(substring(full_place, 1, 3) as integer) AS \"place\"
       FROM
          dw2.tb_plc
       WHERE
          char_bh = 'big'"

    q = SQL(select="place", from="@place", where=w_place)

    expect_true(queriesEqual(q, ref))
})

test_that("SQL handles complex view", {
    ref = "SELECT
       \"date\",
       \"id\",
       \"action\",
       \"reaction\",
       \"kpi1\",
       \"kpi2\"
       FROM
       (
          SELECT
             \"date\",
             a.\"id\",
             \"action\",
             \"reaction\",
             \"kpi1\",
             \"kpi2\",
             \"place\"
          FROM
             (
                SELECT
                   cast(reg_dt as date format 'yyyy-mm-dd') AS \"date\",
                   target_nbr AS \"id\",
                   trs_cd mod 4 AS \"action\",
                   floor(trs_cd / 7) AS \"reaction\",
                   asdf AS \"kpi1\",
                   fdas AS \"kpi2\",
                   plc_cd AS \"place\"
                FROM
                   dw2.tb_react
                WHERE
                   cast(reg_dt as date format 'yyyy-mm-dd') > '2013-09-30'
             ) AS a
             INNER JOIN (
                SELECT
                   crfty_id_mstr AS \"id\"
                FROM
                   crufty_db1.crfttabl_2z
                WHERE
                   curr_avg_cst < 10
             ) AS b
             ON
                a.\"id\" = b.\"id\"
       ) AS e
       INNER JOIN (
          SELECT
             c.\"place\"
          FROM
             (
                SELECT
                   cast(substring(full_place, 1, 3) as integer) AS \"place\"
                FROM
                   dw2.tb_plc
                WHERE
                   char_bh = 'big'
             ) AS c
             LEFT JOIN (
                SELECT
                   test_key mod 1000 AS \"place\"
                FROM
                   dw2.tb_plc2
                WHERE
                   floor(test_key / 1000) in (1, 2, 7)
             ) AS d
             ON
                c.\"place\" = d.\"place\"
       ) AS f
       ON
          e.\"place\" = f.\"place\""

    q = SQL(select = c( "date", "id", "action", "reaction", "kpi1", "kpi2"),
            from = "@reactions",
            where = AND(w, w_place, w_test))

    expect_true(queriesEqual(q, ref))
})

test_that("SQL handles grouping", {
    ref = "SELECT
       \"id\",
       \"action\",
       \"reaction\",
       sum(\"kpi1\") AS \"kpi1\",
       count(\"kpi1\") AS \"count\",
       avg(\"kpi2\") AS \"kpi2\",
       min(\"kpi2\") AS \"min_kpi2\"
       FROM
       (
          SELECT
             a.\"id\",
             \"action\",
             \"reaction\",
             \"kpi1\",
             \"kpi2\",
             \"place\"
          FROM
             (
                SELECT
                   target_nbr AS \"id\",
                   trs_cd mod 4 AS \"action\",
                   floor(trs_cd / 7) AS \"reaction\",
                   asdf AS \"kpi1\",
                   fdas AS \"kpi2\",
                   plc_cd AS \"place\"
                FROM
                   dw2.tb_react
                WHERE
                   cast(reg_dt as date format 'yyyy-mm-dd') > '2013-09-30'
             ) AS a
             INNER JOIN (
                SELECT
                   crfty_id_mstr AS \"id\"
                FROM
                   crufty_db1.crfttabl_2z
                WHERE
                   curr_avg_cst < 10
             ) AS b
             ON
                a.\"id\" = b.\"id\"
       ) AS e
       INNER JOIN (
          SELECT
             c.\"place\"
          FROM
             (
                SELECT
                   cast(substring(full_place, 1, 3) as integer) AS \"place\"
                FROM
                   dw2.tb_plc
                WHERE
                   char_bh = 'big'
             ) AS c
             LEFT JOIN (
                SELECT
                   test_key mod 1000 AS \"place\"
                FROM
                   dw2.tb_plc2
                WHERE
                   floor(test_key / 1000) in (1, 2, 7)
             ) AS d
             ON
                c.\"place\" = d.\"place\"
       ) AS f
       ON
          e.\"place\" = f.\"place\"
          GROUP BY \"id\", \"action\", \"reaction\""

    q = SQL(select = c('kpi1', avg='kpi2', min_kpi2.min='kpi2', count.count='kpi1'),
            from = '@reactions',
            where = AND(w, w_place, w_test),
            groupby = c('id', 'action', 'reaction'))

    expect_true(queriesEqual(q, ref))
})

test_that("SQL handles OR", {
    ref = "SELECT
       cast(substring(full_place, 1, 3) as integer) AS \"place\"
       FROM
          dw2.tb_plc
       WHERE
          char_bh = 'big' OR
          cost > 100000"

    wo = OR(size="big", cost=list(">", 100000))
    q = SQL(select='place', from='@place', where=wo)

    expect_true(queriesEqual(q, ref))
})

test_that("SQL handles cross-table OR", {
    ref = "SELECT
       a.\"place\"
       FROM
       (
          SELECT
             cast(substring(full_place, 1, 3) as integer) AS \"place\",
             char_bh AS \"size\"
          FROM
             dw2.tb_plc
       ) AS a
       LEFT JOIN (
          SELECT
             test_key mod 1000 AS \"place\",
             floor(test_key / 1000) AS \"test\"
          FROM
             dw2.tb_plc2
       ) AS b
       ON
          a.\"place\" = b.\"place\"
       WHERE
          \"size\" = 'big' OR
          \"test\" in (1, 2, 7)"


    q = SQL(select='place', from='@place', where=OR(w_place, w_test))

    expect_true(queriesEqual(q, ref))
})
