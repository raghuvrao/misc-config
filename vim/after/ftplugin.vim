augroup no_concealing
  autocmd!
  autocmd FileType * setlocal conceallevel=0
  autocmd FileType * setlocal concealcursor=
augroup END

augroup no_automatic_hard_wrapping
  autocmd!
  autocmd FileType * setlocal formatoptions-=c
  autocmd FileType * setlocal formatoptions-=t
augroup END

augroup no_automatic_comment_leader_insertion_in_normal_mode
  autocmd!
  autocmd FileType * setlocal formatoptions-=o
augroup END
