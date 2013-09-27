#library(data.table)

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

namesAsVals = function(x) {
	x = names(x)
	setattr(x, 'names', x)  
}

tf = function(table, field) {
	paste0(table, '.', field)
}

indentWith = function(v, sep) {
	if (lv <- length(v))
		paste0('  ', v, c(rep_len(sep, lv-1), ''))
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
	
	'OK'	
}

SQL = function(select, from, where=NULL) {
	print('<SQL>')
	paste(qBuild(select=select, from=from, where=where), collapse='\n')
}

qBuild = function(select, from, where=NULL, aliased=NULL, wormhole=F) {
	print('<QBUILD>')
	if (!wormhole) {
		select = enquote(select)
		if (length(where))
			where = enquoteNames(where)
	}
	
	if (substr(from, 1, 1) == '@') {
		if (is.null(aliased))
			aliased = rep(F, 26)
		
		r = nest(select, from, where, aliased)

	} else {
		fields = getFields(from)
		fnames= names(fields)
		if (!all(select %chin% fnames))
			stop(paste('Selected fields:', paste(select, collapse=', '),
						  'don\'t match options:.', paste(fnames, collapse=', '),
						  sep='\n'))

		selects = fields[select]
		select_strs = paste(selects, 'AS', names(selects))

		wheres = if (length(where))
			c('WHERE', indentWith(getWheres(where, fields), ' AND'))
		
		r = list(
			q=c('SELECT',
					indentWith(select_strs, ','),
					'FROM',
					indentWith(from, ''),
					wheres
				),
			a=aliased
		)
	}
	
	if (wormhole) {
		return(r)    
	} else {
		return(r[['q']])
	}
}

getWheres = function(where, fields) {

  wo = mapply(
    function(expr, crit) {
      if (is.list(crit)) {
        op = crit[[1]]
        crit = crit[[2]]
      } else
        op = '='
      
      un = substr(op, 1, 1) == '!'
      if (un)
      	op = sub('^![ ]*', '', op)

      if (class(crit) == 'Date')
        crit = as.character(crit)
      if (class(crit) == 'character')
        crit = chrEscape(crit)

     	multi_crit = length(crit) > 1
      if (is.null(crit)) {
      	op = 'is'
      	crit = 'NULL'
      } else if (multi_crit) {
	      crit = if (op == 'between') {
	      		paste(crit, collapse=' and ')
	    		} else
	    			paste0('(', paste(crit, collapse=', '), ')')
      }

      if (op == '=' && multi_crit)
      	op = 'in'
      if (un) {
      	op = if (op == '=') {
      			'<>'
      		} else
      			paste('not', op)
      }

      paste(expr, op, crit)
    },
    fields[names(where)],
    where
  )
  wo
}

nest = function(select, from, where, aliased) {
	print('<NEST>')
	view_spec = viewSpec(from)
	names = names(view_spec)
	fields = getFields(from, combine=F)

	seq = seq_along(fields)

	if (length(where))
		wnames = names(where)

	q = NULL
	for (i in seq) {
		fields_i = fields[[i]]
		hide_i = enquote(view_spec[[i]][['hide']])
		fields_i = fields_i[!names(fields_i) %chin% hide_i]


		join_i = view_spec[[i]][['join']]
		on_i = enquote(join_i[['on']])

		#~ Divy selects and wheres
		select_i = union(on_i, intersect(select, fields_i))
		other_i = setdiff(select_i, on_i)

		where_i = if (length(where))
			where[intersect(wnames, fields_i)]

		#~ Join if you have to
		lazy = isTRUE(join_i[['lazy']])

		if (!lazy || length(other_i) || length(where_i)) {

			#~ Build query 'i'
			qb = qBuild(select_i, names[i], where_i, aliased, wormhole=T)
			q_i = qb[['q']]

			#~ Update alias
			aliased = qb[['a']]
			n = sum(aliased) + 1
			if (n > 26)
				stop('No letters left in alphabet. Perhaps a new table or two...')
			a_i = letters[n]
			aliased[n] = T

			#~ Update main query
			if (length(q)) {
				type = toupper(join_i[['type']])
				q = join(select, type, on_i, q, q_i, a_prev, a_i)
			} else
				q = q_i
				
			a_prev = a_i
		}
	}
	list(q=q, a=aliased)
}

join = function(select, type, on, q1, q2, a1, a2) {
	print('<JOIN>')
	for (s in intersect(on, select)) {
		newsel = switch(type,
			full=paste('case', tf(a1, s), 'when null then', tf(a2, s),
						  'else', tf(a1, s),  'end AS', s),
			right=tf(a2, s),
			tf(a1, s)
		)
		
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
			paste(type, "JOIN ("),
			indentWith(q2, ''),
			paste(') AS', a2),
			'ON',
			indentWith(on, ' AND')
		), '')
	)
}

