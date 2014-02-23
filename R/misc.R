#' @export
getFields = function(name, combine=TRUE, wormhole=FALSE) {
    if (substring(name, 1, 1) == '@') {
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
            setattr(rf, 'names', rf)
        }
        rf
    }
}

#' @export
validateView = function(vs) {
    names = names(vs)
    fields = lapply(names, getFields, wormhole=TRUE)

    prev_fields = fields[[1]]
    r = NULL
    for (i in seq_along(fields)[-1]) {
        i_fields = fields[[i]]
        overlap = intersect(prev_fields, i_fields)
        on = enquote(vs[[i]]$join$on)
        if (!setequal(overlap, on)) {
            ri = sprintf("Overlap with '%s' (%s) doesn't match key (%s)",
                         names(i),
                         paste(overlap, collapse=', '),
                         paste(on, collapse=', '))
            r = c(r, ri)
        }
        prev_fields = union(prev_fields, i_fields)
    }
    r

    if (length(r))
        stop(paste(c('viewSpec errors:', r), collapse='\n'))
}
