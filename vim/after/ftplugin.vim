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
