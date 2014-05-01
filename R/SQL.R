#' Let me write your queries for you.
#'
#' \code{SQL} uses functions \code{tableFields} and \code{viewSpec} from the 
#'   calling scope to construct queries from simple arguments.
#'
#' @param select Field names.
#' @param from  'db.tablename' or '@@viewname', corresponding to an entry in
#'   \code{tableFields} or \code{viewSpec}.
#' @param where An \code{AND} or \code{OR} object.
#' @param groupby Optional, vector of field names. If provided, elements of
#'   select not in \code{groupby} will have an aggregate function applied.
#'   Those elements may be named in the select argument as follows:
#'   '', '[new name].[function], '[new name].', '.[function]', or '[function]'.
#'   Defaults: '[old name].[sum]'
#' @return Query string.
#' @export
SQL = function(select, from, where=NULL, groupby=NULL) {

    if (length(where)) {
        w = cleanWheres(where)
        where = list(vals = w, names = whereNames(w))
    }

    validnames = names(getFields(from))

    selectnames = enquote(union(groupby, select))
    allnames = unique(c(selectnames, unlist(where$names)))
    namesfound = allnames %chin% validnames
    if (!all(namesfound))
        stop(paste('Invalid fields specified:', paste(allnames[!namesfound], collapse=', '),
                   'Valid fields:.', paste(validnames, collapse=', '),
                   sep='\n'))

    q = qBuild(select = selectnames,
               from = from,
               where = where)$q

    if (!is.null(groupby))
        q = groupify(q, select, groupby)

    paste(q, collapse='\n')
}

groupify = function(q, select, groupby) {

    end_line = match('FROM', q) - 1
    if (is.na(end_line))
        stop("Didn't find 'FROM'.")
    selects = q[1:end_line]

    r = paste0('^', indent(), '((.+) AS |([a-z][.]))?"([a-z][a-z0-9_]*)",?$')

    fields = sub(r, '\\4', selects)
    preps = sub(r, '\\3', selects)
    exprs = sub(r, '\\2', selects)
    exprs = ifelse(nzchar(exprs), 
                   exprs,
                   paste0(preps, enquote(fields)))

    #~ Sub selects
    to_aggr = select[!select %chin% groupby]
    how_aggr = names(to_aggr)
    if (is.null(how_aggr))
        how_aggr = character(length(to_aggr))

    for (i in seq_along(fields)) {
        matches = ( to_aggr == fields[i] )
        if (any(matches)) {

            new = NULL
            for (j in which(matches)) {
                field = to_aggr[j]
                directions = how_aggr[j]

                name = op = ''
                if (nzchar(directions)) {
                    r = '^(([^.]*)[.])?([^.]*)$'
                    if (!grepl(r, directions))
                        stop(sprintf("Invalid name (%s) for field '%s'",
                                     directions, field))

                    name = sub(r, '\\2', directions)
                    op = sub(r, '\\3', directions)
                }
                if (!nzchar(name))
                    name = field
                if (!nzchar(op))
                    op = 'sum'

                line = paste0(indent(), op, '(', exprs[i], ') AS "', name, '"')
                new = if (length(new)) paste0(new, ',\n', line) else line
            }
            if (i != end_line)
                new = paste0(new, ',')
            q[i] = new
        }
    }

    c(q, paste('GROUP BY', 
               paste0(preps[match(groupby, fields)],
                      enquote(groupby),
                      collapse=', ')))
}

qBuild = function(select, from, where=NULL, aliased=0) {

    if (substr(from, 1, 1) == '@') {
        nest(select, from, where, aliased)
    } else
        leaf(select, from, where$vals, aliased)
}

leaf = function(select, from, where, aliased) {

    fields = getFields(from)
    selects = fields[select]
    select_strs = paste(selects, 'AS', names(selects))

    wheres = if (length(where))
        c('WHERE', indentWith(getWheres(where, fields), ''))

    list(q = c('SELECT',
               indentWith(select_strs, ','),
               'FROM',
               indentWith(from, ''),
               wheres),
         a = aliased)
}

nest = function(select, from, where, aliased) {

    view_spec = viewSpec(from)
    names = names(view_spec)
    fields = getFields(from, combine=FALSE)

    set = lookAhead(select, where, fields, view_spec)

    q = NULL
    explicit_select = NULL
    for (i in seq_along(set)) {

        iset = set[[i]]

        if (!is.null(iset)) {

            #~ Build query 'i'
            qb = qBuild(iset$all_select, names[i], iset$where_leaf, aliased)
            q_i = qb$q
            aliased = qb$a

            #~ Update main query
            explicit_select = union(explicit_select, iset$explicit_select)
            if (length(q)) {
                type = toupper(iset$type)
                
                #~ Update alias
                aliased = aliased + 2
                if (aliased > 26)
                    stop('No letters left in alphabet. Perhaps a new table or two...')

                q = join(union(explicit_select, iset$select_for_later), type, 
                         iset$on, q, q_i, letters[aliased-1], letters[aliased],
                         where=iset$where_join$vals)
            } else
                q = q_i
        }
    }
    list(q=q, a=aliased)
}

subsetWhere = function(where, bool) {
    if (any(bool))
        sapply(where,
               function(x) structure(x[bool], class=class(x)),
               simplify=FALSE)
}

lookAhead = function(select, where, fields, view_spec) {

    r = vector('list', length(fields))

    select_for_later = joins = NULL
    for (i in rev(seq_along(fields))) {
        fields_i = fields[[i]]
        hide_i = enquote(view_spec[[i]]$hide)
        fields_i = fields_i[!names(fields_i) %chin% hide_i]

        join_i = view_spec[[i]]$join
        type_i = join_i$type
        on_i = enquote(join_i$on)

        #~ Divy wheres
        where_leaf = where_join = i_where_join_names = other_where_join_names = where_leftover = NULL
        if (length(where)) {
            wnames = where$names
            is_any = sapply(wnames, function(ns) {
                any(ns %chin% fields_i) &&
                (type_i != "left" || !all(ns %chin% on_i))
            })
            is_leaf = sapply(wnames, function(ns) { 
                all(ns %chin% fields_i) &&
                ((!any(ns %chin% on_i)) || type_i %chin% c("right", "inner"))
            })
            is_join = is_any & !is_leaf
            is_neither = !is_any

            where_leaf = subsetWhere(where, is_leaf)
            where_join = subsetWhere(where, is_join)
            where_leftover = subsetWhere(where, is_neither)

            all_where_join_names = if (length(where_join))
                unique(unlist(where_join$names))

            i_where_join_names = intersect(all_where_join_names, fields_i)
            other_where_join_names = setdiff(all_where_join_names, fields_i)
        }
        where_nonkey = setdiff(union(where_leaf$names, i_where_join_names), on_i)

        #~ Divy selects
        explicit_select = intersect(select, fields_i)
        for_later_select = intersect(select_for_later, fields_i)

        all_select = unique(c(on_i, explicit_select, for_later_select, i_where_join_names))
        select_nonkey = setdiff(all_select, on_i)

        #~ Join if you have to
        lazy = isTRUE(join_i$lazy)

        if (!lazy || length(select_nonkey) || length(where_nonkey)) {
            r[[i]] = list(type=type_i, on=on_i, explicit_select=explicit_select,
                          select_for_later=select_for_later, all_select=all_select,
                          where_leaf=where_leaf, where_join=where_join)
            select_for_later = unique(c(select_for_later, on_i, other_where_join_names))
            where = where_leftover
        }
    }

    if (!is.null(where))
        stop("Some wheres not found.")
    r
}

join = function(select, type, on, q1, q2, a1, a2, where=NULL) {

    for (s in intersect(on, select)) {
        newsel = switch(type,
            full=paste('case', tf(a1, s), 'when null then', tf(a2, s),
                       'else', tf(a1, s),  'end AS', s),
            right=tf(a2, s),
            tf(a1, s))
        
        select[match(s, select)] = newsel
    }
    
    on = paste(tf(a1, on), '=', tf(a2, on))

    q = c(
        'SELECT',
        indentWith(select, ','),
        'FROM',
        indentWith(c(
            '(',
            indentWith(q1, ''),
            paste(') AS', a1),
            paste(type, 'JOIN ('),
            indentWith(q2, ''),
            paste(') AS', a2),
            'ON',
            indentWith(on, ' AND')
        ), '')
    )

    if (length(where)) {
        q = c(q,
              'WHERE',
              indentWith(getWheres(where, NULL, leaf=FALSE), '')
        )
    }
    q
}
