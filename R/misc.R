#' @export
tableFields = function(name) TABLE_SCHEMA[[name]]

#' @export
viewSpec = function(name) VIEW_SCHEMA[[name]]

#' List available fields for a table in \code{TABLE_SCHEMA} or a view in \code{VIEW_SCHEMA}
#'
#' \code{getFields} is used internally to write your queries and validate view,
#'   definitions, but it's exported for use in middleware that exposes available
#'   fields to users.
#'
#' @param name table/view name.
#' @param combine for internal use
#' @param wormhole also for internal use
#' @return vector of available field names
#' @export
getFields = function(name, combine = TRUE, wormhole = FALSE) {
    if (substring(name, 1, 1) == "@") {
        vs = viewSpec(name)
        fields = lapply(names(vs), function(name) {
            all = getFields(name, wormhole=TRUE)
            hide = enquote(vs[[name]]$hide)
            all[!names(all) %chin% hide]
        })
        if (combine || wormhole)
            fields = unlist(fields)
        fields
    } else {
        rf = enquoteNames(tableFields(name))
        if (wormhole) {
            rf = names(rf)
            setattr(rf, "names", rf)
        }
        rf
    }
}

#' Check correctness of a view defined in \code{VIEW_SCHEMA}
#'
#' \code{validateView} checks views against \code{TABLE_SCHEMA} and upstream
#'   views for formatting, typos, and logical errors.
#'
#' @param name view name.
#' @return NULL (Throws an error or prints reassurance depending on correctness).
#' @export
validateView = function(name) {
    e = viewErrors(name)

    if (!is.null(e)) {
        stop(e)
    } else {
        cat(sprintf("View '%s' O.K.\n", name))
    }
}

#' Check correctness of all views defined in \code{VIEW_SCHEMA}
#'
#' \code{validateViews} checks views against \code{TABLE_SCHEMA} and upstream
#'   views for formatting, typos, and logical errors.
#'
#' @return NULL (Throws an error or prints reassurance depending on correctness).
#' @export
validateViews = function() {
    es = unlist(sapply(names(VIEW_SCHEMA), viewErrors))

    if (!is.null(es)) {
        stop(paste(es, collapse = "\n"))
    } else {
        cat("All views O.K.\n")
    }
}

viewErrors = function(name) {
    vs = viewSpec(name)
    names = names(vs)
    fields = lapply(names, getFields, wormhole=TRUE)

    prev_fields = r = NULL

    for (i in seq_along(fields)) {
        i_fields = fields[[i]]

        i_where = vs[[i]]$where
        if (length(i_where)) {
            if (!isTRUE(class(i_where) %chin% c("AND", "OR"))) {
                r[length(r) + 1] = sprintf(
                    "Where clause for sub-view/table '%s' not an 'AND' or 'OR' object",
                    names[i])
            }

            i_where_names = names(i_where)
            where_valid = enquote(i_where_names) %chin% i_fields
            if (!all(where_valid)) {
                r[length(r) + 1] = sprintf(
                    "Where clause fields ('%s') not in sub-view/table '%s'",
                    paste(i_where_names[!where_valid], collapse = ", "),
                    names[i])
            }
        }

        i_hide = vs[[i]]$hide
        if (length(i_hide)) {
            hide_valid = enquote(i_hide) %chin% i_fields
            if (!all(hide_valid)) {
                r[length(r) + 1] = sprintf(
                    "Hide fields ('%s') not in sub-view/table '%s'",
                    paste(i_hide[!hide_valid], collapse = ", "),
                    names[i])
            }
            i_fields = setdiff(i_fields, enquote(i_hide))
        }

        i_join = vs[[i]]$join
        if (i == 1) {
            if (length(i_join)) {
                r[length(r) + 1] = "The first sub-view/table can't have a join"
            }
        } else if (!length(i_join)) {
            r[length(r) + 1] = sprintf(
                "Join spec missing for sub-view/table '%s'",
                names[i])
        } else {
            types = c("inner", "outer", "left", "right")
            if (!isTRUE(i_join$type %chin% types)) {
                r[length(r) + 1] = sprintf(
                    "Join type for '%s' should be in: %s'",
                    names[i],
                    paste(types, collapse = ", "))
            }

            i_join_on = i_join$on

            prev_valid = enquote(i_join_on) %chin% prev_fields
            if (!all(prev_valid)) {
                r[length(r) + 1] = sprintf(
                    "Join keys ('%s') not available upstream from sub-view/table '%s'",
                    paste(i_join_on[!prev_valid], collapse = ", "),
                    names[i])
            }

            i_valid = enquote(i_join_on) %chin% i_fields
            if (!all(i_valid)) {
                r[length(r) + 1] = sprintf(
                    "Join keys ('%s') not available in sub-view/table '%s'",
                    paste(i_join_on[!i_valid], collapse = ", "),
                    names[i])
            }
        }

        prev_fields = union(prev_fields, i_fields)
    }

    if (length(r)) {
        paste(c(sprintf("View '%s' errors:", name), r), collapse = "\n")
    }
}
