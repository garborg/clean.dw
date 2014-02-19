#' @export
OR = function(...) {
    l = list(...)
    if (all(sapply(l, function(x) isTRUE(attr(x, "AND_OR") == "OR"))))
        l = do.call(c, l)
    setattr(l, "AND_OR", "OR")
    l
}

#' @export
AND = function(...) {
    l = list(...)
    if (all(sapply(l, function(x) isTRUE(attr(x, "AND_OR") == "AND"))))
        l = do.call(c, l)
    setattr(l, "AND_OR", "AND")
    l
}

cleanWheres = function(wheres, wormhole=FALSE) {

    and_or = attr(wheres, "AND_OR")
    if (!wormhole) {
        if (is.null(and_or)) {
            stop("'where' clauses must be 'AND' objects or 'OR' objects.")
        } else if (and_or == "OR") {
            wheres = AND(wheres)
            and_or = "AND"
        }
    }

    and_ors = lapply(wheres, attr, "AND_OR")
    if (any(and_ors == and_or))
        stop(sprintf("No nesting %s directly inside of another %s in the 'where' clause.",
                     and_or, and_or))

    is_and_or = !sapply(and_ors, is.null)
    ns = names(wheres)
    if (is.null(ns)) {
        ns = rep_len("", length(wheres))
        names(wheres) = ns
    }
    if (any(nzchar(ns) == is_and_or))
        stop("Elements of 'where' must be unnamed 'iff' they're 'AND' or 'OR' objects.")

    w = enquoteNames(mapply(
        function(w, i_a_o) if (i_a_o) cleanWheres(w, wormhole=TRUE) else w,
        wheres, is_and_or, SIMPLIFY=FALSE
    ))
    attr(w, "AND_OR") = and_or
    w
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
            v[vl] = paste(v[vl], attr(where, 'AND_OR'))
        }
        v
    }, names(where), where, seq_along(where), length(where))
}
