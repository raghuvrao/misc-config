" If the matchparen plugin has been loaded, the commands :NoMatchParen
" and :DoMatchParen will be defined.
if !exists(':NoMatchParen') || !exists(':DoMatchParen')
  finish
endif

" Start off with matching brackets highlighting disabled.  When I want
" matching brackets highlighting, I will call ToggleMatchParen() defined
" below.
NoMatchParen

function! ToggleMatchParen() abort
  if exists('g:loaded_matchparen')
    NoMatchParen
  else
    DoMatchParen
  endif
endfunction

command! ToggleMatchParen call ToggleMatchParen()
