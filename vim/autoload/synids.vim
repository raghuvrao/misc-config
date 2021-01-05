" synids.vim
" Author: Raghu V. Rao <raghu.v.rao@gmail.com>

function! synids#SynIDs(line_num, col_num) abort
  let id = synID(a:line_num, a:col_num, 1)
  let id_name = synIDattr(id, 'name')
  if id_name == ''
    return id_name
  endif
  let final_id = synIDtrans(id)
  if final_id == id
    return id_name
  endif
  return id_name .. ': ' .. synIDattr(final_id, 'name')
endfunction
