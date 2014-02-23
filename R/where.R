#' Format conditions to be passed to a where clause.
#'
#' \code{OR} formats conditions to be passed to a where clause.
#'
#' @param ... A variable number of arguments, each an \code{AND} or \code{OR}
#'   object or a named argument, the name being a field name, the value:
#'   list(['=', '>', '<', 'like', or 'between'. optional '!' prepend], [values])
#'   or just a vector of values, in which case '=' is assumed.
#' @return S3 object of class \code{OR}.
#' @export
OR = function(...) {
    l = structure(list(...), class="OR")
    is_or = sapply(l, inherits, "OR")
    if (any(is_or)) {
        for (i in seq_along(l)) {
            li = l[[i]]
            if (inherits(li, "AND"))
                l[[i]] = list(li)
        }
        structure(do.call(c, l), class="OR")
    } else
        l
}

#' Format conditions to be passed to a where clause.
#'
#' \code{AND} formats conditions to be passed to a where clause.
#'
#' @param ... A variable number of arguments, each an \code{AND} or \code{OR}
#'   object or a named argument, the name being a field name, the value:
#'   list(['=', '>', '<', 'like', or 'between'. optional '!' prepend], [values])
#'   or just a vector of values, in which case '=' is assumed.
#' @return S3 object of class \code{AND}.
#' @export
AND = function(...) {
    l = structure(list(...), class="AND")
    is_and = sapply(l, inherits, "AND")
    if (any(is_and)) {
        for (i in seq_along(l)) {
            li = l[[i]]
            if (inherits(li, "OR"))
                l[[i]] = list(li)
        }
        structure(do.call(c, l), class="AND")
    } else
        l
}

#' Translate a data table into \code{AND}/\code{OR} objects.
#'
#' \code{anyRow} Takes a data.table and translates it into an \code{AND}/\code{OR}
#'    object requiring that all values of at least one row must be satisfied.
#'
#' @param dt A \code{data.table}, the names being field names, the values being
#'   possible values for those fields.
#' @return An efficiently nested S3 object of class \code{OR} or \code{AND}.
#' @export
anyRow = function(dt) {
    level_ct = sapply(names(dt), function(n) length(unique(dt[[n]])))
    sorted_names = names(sort(level_ct))
    levelDown(dt[, sorted_names, with=FALSE])
}

levelDown = function(dt) {
    n = names(dt)[[1]]
    vals = unique(dt[[1]])
    len = length(dt)
    if (len > 1) {
        structure(
            lapply(vals, function(val) {
                AND(structure(AND(val), names=n),
                    levelDown(dt[get(n) == val, 2:len, with=FALSE]))
            }),
            class = ifelse(length(vals) > 1, "OR", "AND")
        )
    } else
        structure(AND(vals), names=n)
}

cleanWheres = function(wheres, wormhole=FALSE) {

    and_or = class(wheres)
    if (!wormhole) {
        if (!and_or %chin% c("AND", "OR")) {
            stop("'where' clauses must be 'AND' objects or 'OR' objects.")
        } else if (and_or == "OR") {
            wheres = AND(wheres)
            and_or = class(wheres)
        }
    }

    and_ors = lapply(wheres, class)
    if (any(and_ors == and_or))
        stop(sprintf("No nesting %s directly inside of another %s in the 'where' clause.",
                     and_or, and_or))

    is_and_or = sapply(and_ors, `%chin%`, c("AND", "OR"))
    ns = names(wheres)
    if (is.null(ns)) {
        ns = rep_len("", length(wheres))
        names(wheres) = ns
    }
    if (any(nzchar(ns) == is_and_or))
        stop("Elements of 'where' must be unnamed 'iff' they're 'AND' or 'OR' objects.")

    structure(enquoteNames(mapply(
        function(w, i_a_o) if (i_a_o) cleanWheres(w, wormhole=TRUE) else w,
        wheres, is_and_or, SIMPLIFY=FALSE
    )), class=and_or)
}

whereNames = function(wheres) {
    mapply(function(wi, ni) {
        if (nzchar(ni)) ni else unique(do.call(c, whereNames(wi)))
    }, wheres, names(wheres), SIMPLIFY=FALSE, USE.NAMES=FALSE)
}

getWheres = function(where, fields, leaf = TRUE) {

    mapply(function(n, crit, i, len) {
        v = if (nzchar(n)) {
            if (is.list(crit)) {
                op = crit[[1]]
                crit = crit[[2]]
            } else
                op = '='

            un = substr(op, 1, 1) == '!'
            if (un)
                op = sub('^![ ]*', '', op)

            if (inherits(crit, 'Date'))
                crit = as.character(crit)
            if (class(crit) == 'character')
                crit = chrEscape(crit)

            multi_crit = length(crit) > 1
            if (is.null(crit)) {
                op = 'is'
                crit = 'null'
            } else if (multi_crit) {
                crit = if (op == 'between') {
                        paste(crit, collapse=' and ')
                    } else {
                        if (op == 'like')
                            op = 'like any'
                        paste0('(', paste(crit, collapse=', '), ')')
                    }
            }

            if (op == '=' && multi_crit)
                op = 'in'
            if (un)
                op = if (op == '=') '<>' else paste('not', op)

            val = if (leaf) fields[n] else n
            paste(val, op, crit)
        } else {
            getWheres(crit, fields, leaf=leaf)
        }
        if (len > 1 && length(v) > 1)
            v = c('(', indentWith(v, ''), ')')
        if (i < len) {
            vl = length(v)
            v[vl] = paste(v[vl], class(where))
        }
        v
    }, names(where), where, seq_along(where), length(where))
}
