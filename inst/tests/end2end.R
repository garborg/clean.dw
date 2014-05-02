
### Setup

source("example_schema.R")

today = as.Date("2013-10-31")
last_months_end = today - as.POSIXlt(today)$mday
w = AND(date = list('>', last_months_end),
        cost = list('<', 10))
w_place = AND(size = 'big')
w_test = AND(test = c(1, 2, 7))
w_nonexistent = AND(fakevar = c(1, 2, 7))

### Helpers

stripQuery = function(x) {
    gsub("^[ ]|[ ]$", "", gsub("[\r
        \t ]+", " ", x))
}

queriesEqual = function(x, y) {
    stripQuery(x) == stripQuery(y)
}

### Tests

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

test_that("SQL throws if a nonexistent field is passed via 'select'", {
    expect_error(SQL(select='plaaaaace', from='@place', where=w_test))
})

test_that("SQL throws if a nonexistent field is passed via 'where'", {
    expect_error(SQL(select='place', from='@place', where=w_nonexistent))
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
                   cast(reg_dt as date format 'yyyy-mm-dd') > '2013-09-30' AND
                   trs_cd mod 4 between 2 and 5
             ) AS a
             INNER JOIN (
                SELECT
                   crfty_id_mstr AS \"id\"
                FROM
                   crufty_db1.crfttabl_2z
                WHERE
                   curr_avg_cst < 10 AND
                   crfty_id_mstr > 99 AND
                   curr_avg_cst not is null
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
                   cast(reg_dt as date format 'yyyy-mm-dd') > '2013-09-30' AND
                   trs_cd mod 4 between 2 and 5
             ) AS a
             INNER JOIN (
                SELECT
                   crfty_id_mstr AS \"id\"
                FROM
                   crufty_db1.crfttabl_2z
                WHERE
                   curr_avg_cst < 10 AND
                   crfty_id_mstr > 99 AND
                   curr_avg_cst not is null
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

test_that("SQL handles overlap of select and groupby clauses", {
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
                   cast(reg_dt as date format 'yyyy-mm-dd') > '2013-09-30' AND
                   trs_cd mod 4 between 2 and 5
             ) AS a
             INNER JOIN (
                SELECT
                   crfty_id_mstr AS \"id\"
                FROM
                   crufty_db1.crfttabl_2z
                WHERE
                   curr_avg_cst < 10 AND
                   crfty_id_mstr > 99 AND
                   curr_avg_cst not is null
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

    q = SQL(select = c('id', 'action', 'kpi1', avg='kpi2', min_kpi2.min='kpi2', count.count='kpi1'),
            from = '@reactions',
            where = AND(w, w_place, w_test),
            groupby = c('id', 'action', 'reaction'))

    expect_true(queriesEqual(q, ref))
})

test_that("SQL throws if a nonexistent field is passed via 'groupby'", {
    expect_error(
        SQL(select = c('kpi1', avg='kpi2', min_kpi2.min='kpi2', count.count='kpi1'),
            from = '@reactions',
            where = AND(w, w_place, w_test),
            groupby = c('iiiiiiddddddd', 'action', 'reaction'))
    )
})

test_that("SQL handles OR", {
    ref = "SELECT
       cast(substring(full_place, 1, 3) as integer) AS \"place\"
       FROM
          dw2.tb_plc
       WHERE
          char_bh = 'big' OR
          cost > 100000"

    q = SQL(select = 'place',
            from = '@place',
            where = OR(size="big", cost=list(">", 100000)))

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

test_that("SQL & anyRow handle complex where clause", {
    ref = "SELECT
       e.\"place\",
       \"date\",
       sum(\"kpi1\") AS \"kpi1\",
       sum(\"kpi1\") AS \"kpi1\"
       FROM
       (
          SELECT
             \"place\",
             \"date\",
             \"kpi1\"
          FROM
             (
                SELECT
                   plc_cd AS \"place\",
                   cast(reg_dt as date format 'yyyy-mm-dd') AS \"date\",
                   asdf AS \"kpi1\",
                   target_nbr AS \"id\",
                   floor(trs_cd / 7) AS \"reaction\",
                   trs_cd mod 4 AS \"action\"
                FROM
                   dw2.tb_react
                WHERE
                   trs_cd mod 4 between 2 and 5
             ) AS a
             INNER JOIN (
                SELECT
                   crfty_id_mstr AS \"id\",
                   cat AS \"category\"
                FROM
                   crufty_db1.crfttabl_2z
                WHERE
                   crfty_id_mstr > 99 AND
                   curr_avg_cst not is null
             ) AS b
             ON
                a.\"id\" = b.\"id\"
          WHERE
             (  \"reaction\" = 0 AND
                (  (  \"category\" = 'A' AND
                      \"action\" = 1
                   ) OR
                   (  \"category\" = 'B' AND
                      \"action\" = 1
                   )
                )
             ) OR
             (  \"reaction\" = 1 AND
                (  (  \"category\" = 'A' AND
                      \"action\" in (2, 1)
                   ) OR
                   (  \"category\" = 'B' AND
                      \"action\" = 1
                   ) OR
                   (  \"category\" = 'E' AND
                      \"action\" in (0, 3)
                   ) OR
                   (  \"category\" = 'H' AND
                      \"action\" = 2
                   )
                )
             )
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
             ) AS c
             LEFT JOIN (
                SELECT
                   test_key mod 1000 AS \"place\"
                FROM
                   dw2.tb_plc2
                WHERE
                   floor(test_key / 1000) = 372
             ) AS d
             ON
                c.\"place\" = d.\"place\"
       ) AS f
       ON
          e.\"place\" = f.\"place\"
          GROUP BY e.\"place\", \"date\""

    targets = data.table(
        category = c('A', 'A', 'A', 'B', 'B', 'E', 'E', 'H'),
        action = c(1, 2, 1, 1, 1, 0, 3, 2),
        reaction = c(0, 1, 1, 0, 1, 1, 1, 1))

    q = SQL(select = c('kpi1', 'kpi1'),
            from = '@reactions',
            where = AND(anyRow(targets), test=372),
            groupby = c('place', 'date'))

    expect_true(queriesEqual(q, ref))
})
