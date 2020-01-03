" Note: no augroups below because the autocommands will end up in the augroup
" Vim defines for us in $VIMRUNTIME/filetype.vim.  See :help ftdetect.

" For me, go.mod is related to Go, and not to LambdaProlog, Modsim III, or
" Modula 2.  So, when I open a file named go.mod, do not load any
" lprolog/modsim3/modula2 autocommands, functions etc.

" Modsim III (or LambdaProlog).
" First, remove the autocommand defined in $VIMRUNTIME/filetype.vim.
autocmd! BufNewFile,BufRead *.mod
" Then, add back the same autocommand, with go.mod excluded.
autocmd BufNewFile,BufRead *.mod
      \ if expand('%:t') !=? 'go.mod' |
      \   if getline(1) =~ '\<module\>' |
      \     setf lprolog |
      \   else |
      \     setf modsim3 |
      \   endif |
      \ endif

" Modula 2.
" First, remove the autocommand defined in $VIMRUNTIME/filetype.vim.
autocmd! BufNewFile,BufRead *.MOD
" Then, add back the same autocommand, with go.mod excluded.
autocmd BufNewFile,BufRead *.MOD
      \ if expand('%:t') !=? 'go.mod' |
      \   setf modula2 |
      \ endif
