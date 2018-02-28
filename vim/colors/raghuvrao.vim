" Plain colorscheme for vim.
" Maintainer:	Raghu V. Rao
"
" Goal: disable colors for most things.


hi clear
if exists('g:syntax_on')
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
hi clear String
hi clear Type


hi Comment
      \ term=bold
      \ cterm=NONE ctermfg=Brown ctermbg=NONE
      \ gui=NONE guifg=Brown guibg=NONE
if &background == 'dark'
  hi Comment ctermfg=DarkYellow guifg=DarkYellow
endif


hi Pmenu
      \ cterm=NONE ctermfg=Black ctermbg=Cyan
      \ gui=NONE guifg=Black guibg=LightBlue
if &background == 'dark'
  hi Pmenu ctermfg=LightGray ctermbg=DarkBlue guifg=LightGray guibg=DarkBlue
endif

hi PmenuSbar
      \ cterm=NONE ctermfg=NONE ctermbg=Black
      \ gui=NONE guifg=NONE guibg=Gray
if &background == 'dark'
  hi PmenuSbar ctermbg=White guibg=LightGray
endif

hi PmenuSel
      \ cterm=NONE ctermfg=White ctermbg=DarkBlue
      \ gui=NONE guifg=White guibg=DarkBlue
if &background == 'dark'
  hi PmenuSel ctermfg=Black ctermbg=Cyan guifg=Black guibg=DarkCyan
endif

hi PmenuThumb
      \ cterm=NONE ctermfg=NONE ctermbg=DarkYellow
      \ gui=NONE guifg=NONE guibg=Brown
if &background == 'dark'
  hi PmenuThumb ctermbg=DarkGreen guibg=DarkGreen
endif


hi NonText
      \ term=bold
      \ cterm=bold ctermfg=Blue ctermbg=NONE
      \ gui=bold guifg=Blue guibg=NONE


hi SpecialKey
      \ term=bold
      \ cterm=NONE ctermfg=DarkMagenta ctermbg=NONE
      \ gui=NONE guifg=DarkMagenta guibg=NONE
if &background == 'dark'
  hi SpecialKey ctermfg=DarkCyan guifg=DarkCyan
endif


hi StatusLine
      \ term=reverse,bold
      \ cterm=bold ctermfg=White ctermbg=DarkBlue
      \ gui=bold guifg=White guibg=DarkBlue


hi StatusLineNC
      \ term=reverse
      \ cterm=NONE ctermfg=Black ctermbg=Gray
      \ gui=NONE guifg=Black guibg=Gray


hi VertSplit
      \ term=reverse
      \ cterm=NONE ctermfg=DarkRed ctermbg=NONE
      \ gui=NONE guifg=DarkRed guibg=NONE
if &background == 'dark'
  hi VertSplit ctermfg=Red guifg=Red
endif


hi Visual term=reverse cterm=NONE gui=NONE
if has('gui_running') || &t_Co >= 16
  hi Visual ctermfg=Black ctermbg=Cyan guifg=Black guibg=LightBlue
  if &background == 'dark'
    hi Visual
	  \ ctermfg=White ctermbg=DarkMagenta
	  \ guifg=LightGray guibg=DarkMagenta
  endif
else
  hi Visual cterm=reverse ctermfg=NONE ctermbg=NONE
endif


hi diffAdded term=bold cterm=NONE gui=NONE
if &background == 'light'
  if has('gui_running') || &t_Co >= 256
    hi diffAdded ctermfg=Black ctermbg=LightGreen guifg=Black guibg=LightGreen
  else
    hi diffAdded ctermfg=DarkBlue ctermbg=NONE
  endif
else
  hi diffAdded ctermfg=Green ctermbg=NONE guifg=Green guibg=NONE
endif

hi diffFile
      \ term=bold
      \ cterm=NONE ctermfg=DarkMagenta ctermbg=NONE
      \ gui=bold guifg=DarkMagenta guibg=NONE
if &background == 'dark'
  hi diffFile ctermfg=Magenta guifg=LightMagenta
endif

hi diffLine
      \ term=bold
      \ cterm=NONE ctermfg=Brown ctermbg=NONE
      \ gui=NONE guifg=Brown guibg=NONE
if &background == 'dark'
  hi diffLine ctermfg=Yellow guifg=Yellow
endif

hi diffRemoved term=bold cterm=NONE gui=NONE
if &background == 'light'
  if has('gui_running') || &t_Co >= 256
    hi diffRemoved ctermfg=Black ctermbg=LightRed guifg=Black guibg=LightRed
  else
    hi diffRemoved ctermfg=DarkRed ctermbg=NONE
  endif
else
  hi diffRemoved ctermfg=Red ctermbg=NONE guifg=Red guibg=NONE
endif


hi gitcommitBranch
      \ term=bold,underline
      \ cterm=NONE ctermfg=DarkMagenta ctermbg=NONE
      \ gui=NONE guifg=DarkMagenta guibg=NONE
if &background == 'dark'
  hi gitcommitBranch ctermfg=Magenta guifg=LightMagenta
endif


hi link cCommentString cComment


hi link gitcommitHeader gitcommitComment
hi link gitcommitType gitcommitComment
hi link gitcommitFile gitcommitComment


hi link javaCommentTitle javaComment

hi link javaDocParam javaDocComment
hi link javaDocSeeTagParam javaDocComment
hi link javaDocTags javaDocComment


hi link vimCommentString vimComment
hi link vimCommentTitle vimComment
hi link vimCommentTitleLeader vimComment
