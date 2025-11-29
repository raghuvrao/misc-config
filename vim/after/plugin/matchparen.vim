vim9script

# If the matchparen plugin has been loaded, the commands :NoMatchParen
# and :DoMatchParen will be defined.
if !exists(':NoMatchParen') || !exists(':DoMatchParen')
    finish
endif

def g:ToggleMatchParen()
    if exists('g:loaded_matchparen')
        NoMatchParen
    else
        DoMatchParen
    endif
enddef

command! ToggleMatchParen call ToggleMatchParen()
