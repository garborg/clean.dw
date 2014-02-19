options(scipen=20)

enquote = function(names) {
    if (length(names)) {
        ifelse(nzchar(names), paste0('"', names, '"'), "")
    } else
        names
}

enquoteNames = function(x) {
    names(x) = enquote(names(x))
    x
}

chrEscape = function(chr) {
    if (length(chr)) {
        paste0("'", gsub("'", "''", chr), "'")
    } else
        chr
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
