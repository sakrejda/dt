#' Multiple a flat matrix by a flat column vector (col-wise)
#'
#' @param m flat matrix
#' @param v flat column vector
#' @return column-wise product
#'
#' @export
flat_matrix_times_column_vector = function(m, v) {
  m_nze_lgl = m$row_nze_idx %in% v$row_nze_idx
  m_nze_idx_mult = which(m_nze_lgl)
  m_nze_idx_zero = which(!m_nze_lgl)
  v_nze_idx = match(m$row_nze_idx[m_nze_lgl], v$row_nze_idx)
  m$nze_value[m_nze_idx_mult] = m$nze_value[m_nze_idx_mult] * v$nze_value[v_nze_idx]
  m$n_nze = length(m_nze_idx_mult)
  if (length(m_nze_idx_zero) > 0) {
    m$nze_idx = m$nze_idx[-m_nze_idx_zero]
    m$nze_value = m$nze_value[-m_nze_idx_zero]
    m$row_nze_idx = m$row_nze_idx[-m_nze_idx_zero]
    m$col_nze_idx = m$col_nze_idx[-m_nze_idx_zero]
  }
  m$row_start_idx = which(!duplicated(m$row_nze_idx))
  m$row_n_nze = tabulate(m$row_nze_idx)
  m$row_n_nze = m$row_n_nze[m$row_n_nze != 0]
  if (!is.null(v$col_names)) {
    m$col_names = paste(m$col_names, v$col_names, sep = ':::')
    m$col_names[m$col_names == ':::'] = ''
  }
  return(m)
}
