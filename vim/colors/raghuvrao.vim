set background=light
hi clear
if exists("g:syntax_on")
  syntax reset
endif

let g:colors_name = 'raghuvrao'

hi clear Conceal
hi clear Constant
hi clear Identifier
hi clear Ignore
hi clear Keyword
hi clear Normal
hi clear PreProc
hi clear Special
hi clear Statement
hi clear Type

hi Comment term=bold cterm=NONE ctermfg=Brown ctermbg=NONE guifg=Brown guibg=NONE
hi SpecialKey term=bold cterm=NONE ctermfg=DarkCyan ctermbg=NONE guifg=DarkCyan guibg=NONE

hi diffAdded term=underline cterm=NONE ctermfg=DarkGreen ctermbg=NONE guifg=DarkGreen guibg=NONE
hi diffFile term=bold cterm=NONE ctermfg=DarkBlue ctermbg=NONE guifg=DarkBlue guibg=NONE
hi diffLine term=bold cterm=NONE ctermfg=Brown ctermbg=NONE guifg=Brown guibg=NONE
hi diffRemoved term=bold cterm=NONE ctermfg=DarkMagenta ctermbg=NONE guifg=DarkMagenta guibg=NONE

hi gitcommitBranch term=bold,underline cterm=NONE ctermfg=DarkBlue ctermbg=NONE guifg=DarkBlue guibg=NONE

hi link vimCommentTitle vimComment
hi link vimCommentString vimComment
