" If the matchparen plugin has been loaded, the commands :NoMatchParen
" and :DoMatchParen will be defined.
if !exists(':NoMatchParen') || !exists(':DoMatchParen')
    finish
endif

function! ToggleMatchParen() abort
    if exists('g:loaded_matchparen')
        NoMatchParen
    else
        DoMatchParen
    endif
endfunction

command! ToggleMatchParen call ToggleMatchParen()
