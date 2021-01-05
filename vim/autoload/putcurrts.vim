" putcurrts.vim
" Author: Raghu V. Rao <raghu.v.rao@gmail.com>

" TODO: after upgrading to Vim>=8.1.1310, make the argument for
" parameter 'unix' below optional: assign it the default value 0.
function! putcurrts#PutCurrentTimestamp(unix) abort
  put =strftime(a:unix ? '%s' : '%F %a %T %Z(UTC%z)')
endfunction
