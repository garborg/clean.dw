# Clean DW

Civilize rigid, crufty data warehouses.

## Installation
```r
library(devtools)
install_github('clean.dw', 'garborg')

# library(clean.dw)
```

## Curation
Clean up each table once, consistently for all queries.
```r
tableFields = function(table) {
	switch( table, 
		crufty_db1.crfttabl_2z = c(
			id = 'crfty_id_mstr',
			name = 'lower(trim(trailing from crft_shrt_nm))',
			cost = 'curr_avg_cst'
		),
		dw2.tb_react = c(
			date = "cast(reg_dt as date	format 'yyyy-mm-dd')",
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
```

Complex queries are built using simply defined, transparent views.
```r
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
```

## Usage
Queries benefit from the reliability and economies of scale of curation without losing flexibility.
Format: `SQL(select, from[, where, groupby])`

Setup:
```r
today = Sys.Date()
last_months_end = today - mday(today)  # require(data.table)

w = list(	date = list('>', last_months_end),
			cost = list('<', 10) )

w_place = list( size = 'big' )

w_test = list( test = c(1, 2, 7) )
```

Basic:
```r
SQL(	select = 'place',
		from = '@place',
		where = w_test )
```
yields
```sql
SELECT
   a."place"
FROM
   (
      SELECT
         cast(substring(full_place, 1, 3) as integer) AS "place"
      FROM
         dw2.tb_plc
   ) AS a
   LEFT JOIN (
      SELECT
         test_key mod 1000 AS "place"
      FROM
         dw2.tb_plc2
      WHERE
         floor(test_key / 1000) in (1, 2, 7)
   ) AS b
   ON
      a."place" = b."place"
```

Lazy Join (in)Action:
```r
SQL(	select = 'place',
		from = '@place',
		where = w_place )
```
yields
```sql
SELECT
   cast(substring(full_place, 1, 3) as integer) AS "place"
FROM
   dw2.tb_plc
WHERE
   char_bh = 'big'
```

Bigger:
```r
SQL(	select = c(	'date', 'id', 'action', 'reaction', 'kpi1', 'kpi2'),
		from = '@reactions',
		where = c(w, w_place, w_test) )
```
yields
```sql
SELECT
   "date",
   "id",
   "action",
   "reaction",
   "kpi1",
   "kpi2"
FROM
   (
      SELECT
         "date",
         a."id",
         "action",
         "reaction",
         "kpi1",
         "kpi2",
         "place"
      FROM
         (
            SELECT
               cast(reg_dt as dateformat 'yyyy-mm-dd') AS "date",
               target_nbr AS "id",
               trs_cd mod 4 AS "action",
               floor(trs_cd / 7) AS "reaction",
               asdf AS "kpi1",
               fdas AS "kpi2",
               plc_cd AS "place"
            FROM
               dw2.tb_react
            WHERE
               cast(reg_dt as dateformat 'yyyy-mm-dd') > '2013-10-31'
         ) AS a
         INNER JOIN (
            SELECT
               crfty_id_mstr AS "id"
            FROM
               crufty_db1.crfttabl_2z
            WHERE
               curr_avg_cst < 10
         ) AS b
         ON
            a."id" = b."id"
   ) AS e
   INNER JOIN (
      SELECT
         c."place"
      FROM
         (
            SELECT
               cast(substring(full_place, 1, 3) as integer) AS "place"
            FROM
               dw2.tb_plc
            WHERE
               char_bh = 'big'
         ) AS c
         LEFT JOIN (
            SELECT
               test_key mod 1000 AS "place"
            FROM
               dw2.tb_plc2
            WHERE
               floor(test_key / 1000) in (1, 2, 7)
         ) AS d
         ON
            c."place" = d."place"
   ) AS f
   ON
      e."place" = f."place"
```

With Grouping:
```r
SQL(	select = c('kpi1', avg='kpi2', min_kpi2.min='kpi2', count.count='kpi1'),
		from = '@reactions',
		where = c(w, w_place, w_test),
		groupby = c('id', 'action', 'reaction') )
```
yields
```sql
SELECT
   "id",
   "action",
   "reaction",
   sum("kpi1") AS "kpi1",
   count("kpi1") AS "count",
   avg("kpi2") AS "kpi2",
   min("kpi2") AS "min_kpi2"
FROM
   (
      SELECT
         a."id",
         "action",
         "reaction",
         "kpi1",
         "kpi2",
         "place"
      FROM
         (
            SELECT
               target_nbr AS "id",
               trs_cd mod 4 AS "action",
               floor(trs_cd / 7) AS "reaction",
               asdf AS "kpi1",
               fdas AS "kpi2",
               plc_cd AS "place"
            FROM
               dw2.tb_react
            WHERE
               cast(reg_dt as dateformat 'yyyy-mm-dd') > '2013-10-31'
         ) AS a
         INNER JOIN (
            SELECT
               crfty_id_mstr AS "id"
            FROM
               crufty_db1.crfttabl_2z
            WHERE
               curr_avg_cst < 10
         ) AS b
         ON
            a."id" = b."id"
   ) AS e
   INNER JOIN (
      SELECT
         c."place"
      FROM
         (
            SELECT
               cast(substring(full_place, 1, 3) as integer) AS "place"
            FROM
               dw2.tb_plc
            WHERE
               char_bh = 'big'
         ) AS c
         LEFT JOIN (
            SELECT
               test_key mod 1000 AS "place"
            FROM
               dw2.tb_plc2
            WHERE
               floor(test_key / 1000) in (1, 2, 7)
         ) AS d
         ON
            c."place" = d."place"
   ) AS f
   ON
      e."place" = f."place"
GROUP BY "id", "action", "reaction"
```


## Syntax

### `SQL`
* `select` (required) - vector of field names.
* `from` (required) -  'db.tablename' or '@viewname'.
* `where` - a list of:
    - two-element lists
        1. '=', '>', '<', 'like', or 'between'. optional '!' prepend.
        2. value(s).
    - and/or vectors, in which case, '=' is assumed. ('=' covers 'in' and 'is'.)
* `groupby` -
    - vector of field names.
    - elements of `groupby` don't have to be in `select`.
    - elements of `select` not in `groupby` will have an aggregate function applied.
    - those elements of `select` may be named as follows:
        + unspecified, '[new name].[function]', '[new name].', '.[function]', or '[function]'.
        + if new name not specified, old name prevails.
        + if function not specified, 'sum' is assumed.

### `viewSpec`
Loaded by user, or wrapper. See examples above for format.
* `hide` - currently name abiguities are resolved by by hiding all but one.
* `where` - same as `SQL` argument.
* `join`
    - `type` (required) - 'left', 'right', 'inner', 'full'.
    - `on` (required) - vector of field names.
    - `lazy` - if `TRUE`, included only if a field outside `on` is specified in selects or wheres.


### `tableFields`
Loaded by user, or wrapper. See examples above for format.

### `getFields`
Takes the name of a table or view, returns available fields and their definitions.
* `name` - (required) 'db.tablename' or '@viewname'.
* `combine` - (default `TRUE`) if `FALSE`, returns a list of lists split by source table.
* `wormhole` - (for internal use only).

## Shortcomings

`where` only does `AND`, not `OR`.

`GROUP BY` not available to views.

For date formats outside the ISO standard, field must be recast - casting `where` values would be better.
