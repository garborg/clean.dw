#library(data.table)

enquote = function(name) {
	if (length(name)) 
		paste0('"', name, '"')
	else
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

getFields = function(name, wormhole=F) {
	if (substring(name, 1, 1) == '@') {
		vs = viewSpec(name)
		unlist(lapply(names(vs), function(n) {
			all = getFields(n, wormhole=T)
			hide = enquote(vs[[n]][['hide']])
			all[!names(all) %chin% hide]
		}))
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
	
	view = (substr(from, 1, 1) == '@')
	if (view) {
		if (is.null(aliased))
			aliased = rep(F, 26)
		
		r = nest(select, viewSpec(from), where, aliased)
		print('nested')
	} else {
		fields = getFields(from)
		fnames= names(fields)
		if (!all(select %chin% fnames))
			stop(paste('Selected fields:', paste(select, collapse=', '),
						  'don\'t match options:.', paste(fnames, collapse=', '),
						  sep='\n'))

		selects = fields[select]
		select_strs = paste(selects, 'AS', names(selects))

		if (!is.null(where)) {
			wheres = mapply(
				function(expr, crit) {
					crit_str = switch(mode(crit),
						character=paste0("'", paste(crit, collapse="', '"), "'"),
						paste(crit, collapse=', '))

					if (length(crit) == 1) {
						paste(expr, '=', crit_str)
					} else
						paste0(expr, ' in (', crit_str, ')')
				},
				fields[names(where)],
				where
			)
			wheres = c('WHERE', indentWith(wheres, ' AND'))
		} else
			wheres = NULL
		
		r = list(
			q=c('SELECT',
					indentWith(select_strs, ','),
					'FROM',
					indentWith(from, ''),
					wheres
				),
			a=aliased
		)
		print('unnested')
	}
	
	if (wormhole) {
		return(r)    
	} else {
		return(r[['q']])
	}
}

nest = function(select, view_spec, where, aliased) {
	print('<NEST>')
	names = names(view_spec)
	fields = lapply(names, getFields, wormhole=T)
	seq = seq_along(fields)

	if (length(where))
		wnames = names(where)

	q = NULL
	for (i in seq) {
		fields_i = fields[[i]]
		join_i = view_spec[[i]][['join']]
		on_i = enquote(join_i[['on']])

		#~ Divy selects and wheres
		inter_i = intersect(select, fields_i)
		select_i = union(on_i, inter_i)

		where_i = if (length(where))
			where[intersect(wnames, fields_i)]

		#~ Join if you have to
		lazy = isTRUE(join_i[['lazy']])
	
		if (!lazy || length(inter_i) || length(where_i)) {

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















# join = function(select, view_spec, where, aliased) {
# 	vst = view_spec[['tables']]
# 	vsj = view_spec[['join']]

# 	type = toupper(vsj[['type']])
# 	on = enquote(vsj[['on']])
	
# 	t1 = vst[1]
# 	t2 = vst[2]
# 	names1 = names(getFields(t1))
# 	names2 = names(getFields(t2))
	
# 	joint = intersect(names1, names2)
# 	if (!setequal(on, joint))
# 		stop(sprintf("Overlapping names (%s) in '%s' and '%s' don't match key (%s)",
# 								 paste(joint, collapse=', '),
# 								 t1, t2,
# 								 paste(on, collapse=', ')))
	
# 	# add handling
	
# 	select1 = union(on, intersect(select, names1))
# 	select2 = union(on, intersect(select, names2))
# 	print('joinwhere:')
# 	print(where)
# 	print('names1:')
# 	print(names1)
# 	print('names2:')
# 	print(names2)
# 	if (length(where)) {
# 		wnames = names(where)
# 		where1 = where[intersect(wnames, names1)]
# 		where2 = where[intersect(wnames, names2)]
# 	}
# 	print('where1:')
# 	print(where1)
# 	print('where2:')
# 	print(where2)
# 	qa1 = qBuild(select1, t1, where1, aliased, wormhole=T)
# 	q1 = qa1[['q']]
# 	aliased = qa1[['a']]
	
# 	qa2 = qBuild(select2, t2, where2, aliased, wormhole=T)
# 	q2 = qa2[['q']]
# 	aliased = qa2[['a']]

# 	n = sum(aliased)
# 	if (n == 26)
# 		stop('No letters left in alphabet. Perhaps a new table or two...')
# 	a1 = letters[n + 1]
# 	aliased[n + 1] = T
	
# 	n = sum(aliased)
# 	if (n == 26)
# 		stop('No letters left in alphabet. Perhaps a new table or two...')
# 	a2 = letters[n + 1]
# 	aliased[n + 1] = T
	
# 	for (s in intersect(on, select)) {
# 		newsel = switch(type,
# 										full=paste('case', tf(a1, s), 'when null then',
# 															 tf(a2, s), 'else', tf(a1, s),  'end AS', s),
# 										right=tf(a2, s),
# 										tf(a1, s))
		
# 		select[match(s, select)] = newsel
# 	}
	
# 	on = paste(tf(a1, on), '=', tf(a2, on))

# 	q = c(
# 		'SELECT',
# 		indentWith(select, ','),
# 		'FROM (',
# 		indentWith(c(
# 			indentWith(q1, ''),
# 			paste(') AS', a1),
# 			paste(type, "JOIN ("),
# 			indentWith(q2, ''),
# 			paste(') AS', a2, 'ON'),
# 			indentWith(on, ' AND')
# 		), '')
# 	)
	
# 	return(list(q=q, a=aliased))
# }

# getFields = function(names, wormhole=F) {
# 	fields = unlist(lapply(
# 		names,
# 		function(name) {
# 			if (substring(name, 1, 1) == '@') {
# 				getFields(viewSpec(name)[['tables']], wormhole=T)
# 			} else {
# 				rf = enquoteNames(tableFields(name))
# 				if (wormhole) {
# 					rf = names(rf)
# 					setattr(rf, 'names', rf)          
# 				}
# 				rf
# 			}
# 		}))
# }