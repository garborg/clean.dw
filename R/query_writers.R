chrEscape = function(chr) {
	if (length(chr)) {
		paste0("'", gsub("'", "''", chr), "'")
	} else
		chr
}

enquote = function(name) {
	if (length(name)) {
		paste0('"', name, '"')
	} else
		name
}

enquoteNames = function(x) {
	names(x) = enquote(names(x))
	x
}

tf = function(table, field) {
	paste0(table, '.', field)
}

indent = function() {
	'   '
}

indentWith = function(v, sep) {
	if (lv <- length(v))
		paste0(indent(), v, c(rep_len(sep, lv-1), ''))
}

getFields = function(name, combine=T, wormhole=F) {
	if (substring(name, 1, 1) == '@') {
		vs = viewSpec(name)
		fields = lapply(names(vs), function(name) {
			all = getFields(name, wormhole=T)
			hide = enquote(vs[[name]][['hide']])
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

validateView = function(vs) {
	names = names(vs)
	fields = lapply(names, getFields, wormhole=T)

	prev_fields = fields[[1]]
	r = NULL
	for (i in seq_along(fields)[-1]) {
		i_fields = fields[[i]]
		overlap = intersect(prev_fields, i_fields)
		on = enquote(vs[[i]][['join']][['on']])
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

SQL = function(select, from, where=NULL, groupby=NULL) {
	cat('<SQL>')
	q = qBuild(select=union(groupby, select), from=from, where=where)

	if (!is.null(groupby))
		q = groupify(q, select, groupby)

	cat('\n')

	paste(q, collapse='\n')
}

groupify = function(q, select, groupby) {
	cat('\n<GROUP>')

	end_line = match('FROM', q) - 1
	if (is.na(end_line))
		stop("Didn't find 'FROM'.")
	selects = q[1:end_line]

	r = paste0('^', indent(), '((.+) AS |([a-z][.]))?"([a-z][a-z0-9_]*)",?$')

	fields = sub(r, '\\4', selects)
	preps = sub(r, '\\3', selects)
	exprs = sub(r, '\\2', selects)
	exprs = ifelse( nzchar(exprs), 
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
		paste0(preps[match(groupby, fields)], enquote(groupby), collapse=', ')))
}

qBuild = function(select, from, where=NULL, aliased=0, wormhole=F) {
	if (!wormhole) {
		select = enquote(select)
		if (length(where))
			where = enquoteNames(where)
	}

	r = if (substr(from, 1, 1) == '@') {
			nest(select, from, where, aliased)
		} else
			leaf(select, from, where, aliased)

	return( if (wormhole) r else r[['q']] )
}

leaf = function(select, from, where, aliased) {
	cat('\n<LEAF>')
	fields = getFields(from)
	fnames = names(fields)
	if (!all(select %chin% fnames))
		stop(paste('Selected fields:', paste(select, collapse=', '),
					'don\'t match options:.', paste(fnames, collapse=', '),
					sep='\n'))

	selects = fields[select]
	select_strs = paste(selects, 'AS', names(selects))

	wheres = if (length(where))
		c('WHERE', indentWith(getWheres(where, fields), ' AND'))
	
	list(	q=c(	'SELECT',
					indentWith(select_strs, ','),
					'FROM',
					indentWith(from, ''),
					wheres ),
			a=aliased )
}

getWheres = function(where, fields) {
	cat('<WHERES>')
	mapply(
		function(expr, crit) {
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
			if (un) {
				op = if (op == '=') '<>' else paste('not', op)
			}

			paste(expr, op, crit)
		},
		fields[names(where)],
		where
  )
}

nest = function(select, from, where, aliased) {
	cat('\n<NEST>')
	view_spec = viewSpec(from)
	names = names(view_spec)
	fields = getFields(from, combine=F)

	set = lookAhead(select, where, fields, view_spec)

	q = NULL
	join_select = NULL
	for (i in seq_along(set)) {

		iset = set[[i]]

		if (!is.null(iset)) {

			#~ Build query 'i'
			qb = qBuild(iset[['select']], names[i], iset[['where']],
							aliased, wormhole=T)
			q_i = qb[['q']]
			aliased = qb[['a']]

			#~ Update main query
			join_select = union(join_select, iset[['join_select']])
			if (length(q)) {
				type = toupper(iset[['type']])
				
				#~ Update alias
				aliased = aliased + 2
				if (aliased > 26)
					stop('No letters left in alphabet. Perhaps a new table or two...')

				q = join(union(join_select, iset[['join_ahead']]), type, 
					iset[['on']], q, q_i, letters[aliased-1], letters[aliased])
			} else
				q = q_i
		}
	}
	list(q=q, a=aliased)
}

lookAhead = function(select, where, fields, view_spec) {
	cat('<LOOKAHEAD>')

	if (length(where))
		wnames = names(where)

	join_ahead = joins = NULL
	r = vector('list', length(fields))


	for (i in rev(seq_along(fields))) {
		fields_i = fields[[i]]
		hide_i = enquote(view_spec[[i]][['hide']])
		fields_i = fields_i[!names(fields_i) %chin% hide_i]


		join_i = view_spec[[i]][['join']]
		on_i = enquote(join_i[['on']])

		#~ Divy selects and wheres
		join_select = intersect(select, fields_i)
		later_select = intersect(join_ahead, fields_i)

		select_i = union(on_i, union(join_select, later_select))
		select_other = setdiff(select_i, on_i)

		where_i = if (length(where))
		where[intersect(wnames, fields_i)]
		where_other = setdiff(where_i, on_i)

		#~ Join if you have to
		lazy = isTRUE(join_i[['lazy']])

		if (!lazy || length(select_other) || length(where_other)) {
			r[[i]] = list(type=join_i[['type']], on=on_i, join_select=join_select,
				join_ahead=join_ahead, select=select_i, where=where_i)
			join_ahead = c(join_ahead, on_i)
		}
	}

	return(r)
}

join = function(select, type, on, q1, q2, a1, a2) {
	cat('\n<JOIN>')
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
}
