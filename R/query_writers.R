#library(data.table)

enquote = function(name) {
  paste0('"', name, '"')
}

enquoteNames = function(x) {
  setattr(x, 'names', enquote(names(x)))
}

tf = function(table, field) {
  paste0(table, '.', field)
}

indentWith = function(v, sep) {
  if (lv <- length(v))
    paste0('  ', v, c(rep_len(sep, lv-1), ''))
}

getFields = function(names, wormhole=F) {
  fields = unlist(lapply(
    names,
    function(name) {
      if (substring(name, 1, 1) == '@') {
        getFields(viewSpec(name)[['tables']], wormhole=T)
      } else {
        rf = tableFields(name)
        enquoteNames(rf)
        if (wormhole) {
          rf = names(rf)
          setattr(rf, 'names', rf)          
        }
        rf
      }
    }))
}

qWrite = function(select, from, where=NULL) {
  paste(qBuild(select=select, from=from, where=where), collapse='\n')
}

qBuild = function(select, from, where=NULL, aliased=NULL, wormhole=F) {
  if (!wormhole) {
    select = enquote(select)
    if (length(where))
      enquoteNames(where)
  }
  
  view = (substr(from, 1, 1) == '@')
  if (view) {
    if (is.null(aliased))
      aliased = rep(F, 26)
    
    r = join(select, viewSpec(from), where, aliased)
    
  } else {
    fields = getFields(from)
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
      wheres =NULL
    
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

join = function(select, view_spec, where, aliased) {
  vst = view_spec[['tables']]
  vsj = view_spec[['join']]

  type = toupper(vsj[['type']])
  on = enquote(vsj[['on']])
  
  t1 = vst[1]
  t2 = vst[2]
  names1 = names(getFields(t1))
  names2 = names(getFields(t2))
  
  joint = intersect(names1, names2)
  if (!setequal(on, joint))
    stop(sprintf("Overlapping names (%s) in '%s' and '%s' don't match key (%s)",
                 paste(joint, sep=', '), t1, t2, paste(on, sep=', ')))
  
  # add handling
  
  select1 = union(on, intersect(select, names1))
  select2 = union(on, intersect(select, names2))
  if (length(where)) {
    wnames = names(where)
    where1 = where[intersect(wnames, names1)]
    where1 = where[intersect(wnames, names1)]
  }

  qa1 = qBuild(select1, t1, where1, aliased, wormhole=T)
  q1 = qa1[['q']]
  aliased = qa1[['a']]
  
  qa2 = qBuild(select2, t2, where2, aliased, wormhole=T)
  q2 = qa2[['q']]
  aliased = qa2[['a']]

  n = sum(aliased)
  if (n == 26)
    stop('No letters left in alphabet. Perhaps a new table or two...')
  a1 = letters[n + 1]
  aliased[n + 1] = T
  
  n = sum(aliased)
  if (n == 26)
    stop('No letters left in alphabet. Perhaps a new table or two...')
  a2 = letters[n + 1]
  aliased[n + 1] = T
  
  
  
  for (s in intersect(on, select)) {
    newsel = switch(type,
                    full=paste('case', tf(a1, s), 'when null then',
                               tf(a2, s), 'else', tf(a1, s),  'end AS', s),
                    right=tf(a2, s),
                    tf(a1, s))
    
    select[match(s, select)] = newsel
  }
  
  on = paste(tf(a1, on), '=', tf(a2, on))

  q = c(
    'SELECT',
    indentWith(select, ','),
    'FROM (',
    indentWith(c(
      indentWith(q1, ''),
      paste(') AS', a1),
      paste(type, "JOIN ("),
      indentWith(q2, ''),
      paste(') AS', a2, 'ON'),
      indentWith(on, ' AND')
    ), '')
  )
  
  return(list(q=q, a=aliased))
}
