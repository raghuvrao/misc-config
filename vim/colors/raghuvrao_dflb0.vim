" Plain colorscheme for vim.
" Maintainer:	Raghu V. Rao
"
" Goal: disable colors for most things.


hi clear
if exists('g:syntax_on')
  syntax reset
endif


let g:colors_name = 'raghuvrao_dflb0'


hi Comment
      \ term=bold
      \ cterm=NONE ctermfg=Brown ctermbg=NONE
      \ gui=NONE guifg=Brown guibg=NONE
if &background == 'dark'
  hi Comment ctermfg=DarkYellow guifg=DarkYellow
endif


if &background == 'dark'
  hi Pmenu
	\ cterm=NONE ctermfg=Black ctermbg=LightGray
	\ gui=NONE guifg=Black guibg=LightGray

  hi PmenuSel
	\ cterm=NONE ctermfg=White ctermbg=DarkBlue
	\ gui=NONE guifg=White guibg=DarkBlue
endif

if !has('gui_running') && &t_Co < 256
  hi PmenuThumb cterm=NONE ctermfg=NONE ctermbg=DarkBlue
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


hi Visual
      \ term=reverse
      \ cterm=NONE ctermfg=Black ctermbg=Cyan
      \ gui=NONE guifg=Black guibg=LightBlue
if &background == 'dark'
  hi Visual ctermfg=White ctermbg=DarkBlue guifg=LightGray guibg=DarkBlue
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


hi link perlSharpBang perlComment


hi link vimCommentString vimComment
hi link vimCommentTitle vimComment
hi link vimCommentTitleLeader vimComment


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
