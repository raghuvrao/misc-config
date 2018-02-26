" Plain colorscheme for vim.
" Maintainer:	Raghu V. Rao
"
" Goal: disable syntax highlighting for most things.


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
hi clear String
hi clear Type


hi Comment
      \ term=bold
      \ cterm=NONE ctermfg=Brown ctermbg=NONE
      \ gui=NONE guifg=Brown guibg=NONE
if &background == "dark"
  hi Comment ctermfg=DarkYellow guifg=DarkYellow
endif


hi Pmenu
      \ cterm=NONE ctermfg=Black ctermbg=Cyan
      \ gui=NONE guifg=Black guibg=Cyan

hi PmenuSel
      \ cterm=NONE ctermfg=White ctermbg=DarkBlue
      \ gui=NONE guifg=White guibg=DarkBlue

hi PmenuSbar
      \ cterm=NONE ctermfg=NONE ctermbg=Gray
      \ gui=NONE guifg=NONE guibg=LightGray

hi PmenuThumb
      \ cterm=NONE ctermfg=NONE ctermbg=Brown
      \ gui=NONE guifg=NONE guibg=Brown

hi NonText
      \ term=bold
      \ cterm=bold ctermfg=Blue ctermbg=NONE
      \ gui=bold guifg=Blue guibg=NONE


hi SpecialKey
      \ term=bold
      \ cterm=NONE ctermfg=DarkMagenta ctermbg=NONE
      \ gui=NONE guifg=DarkMagenta guibg=NONE
if &background == "dark"
  hi SpecialKey ctermfg=DarkCyan guifg=DarkCyan
endif


hi Visual
      \ term=reverse
      \ cterm=NONE ctermfg=Black ctermbg=Cyan
      \ gui=NONE guifg=Black guibg=LightBlue
if &background == "dark"
  hi Visual ctermfg=White ctermbg=DarkBlue guifg=LightGray guibg=Blue
endif


hi diffAdded
      \ term=underline
      \ cterm=NONE ctermfg=DarkBlue ctermbg=NONE
      \ gui=NONE guifg=DarkBlue guibg=NONE
if &background == "dark"
  hi diffAdded ctermfg=Green guifg=LightGreen
endif

hi diffFile
      \ term=bold
      \ cterm=NONE ctermfg=DarkMagenta ctermbg=NONE
      \ gui=bold guifg=DarkMagenta guibg=NONE
if &background == "dark"
  hi diffFile ctermfg=Magenta guifg=LightMagenta
endif

hi diffLine
      \ term=bold
      \ cterm=NONE ctermfg=Brown ctermbg=NONE
      \ gui=NONE guifg=Brown guibg=NONE
if &background == "dark"
  hi diffLine ctermfg=Yellow guifg=Yellow
endif

hi diffRemoved
      \ term=bold
      \ cterm=NONE ctermfg=DarkRed ctermbg=NONE
      \ gui=NONE guifg=DarkRed guibg=NONE
if &background == "dark"
  hi diffRemoved ctermfg=Red guifg=LightRed
endif


hi gitcommitBranch
      \ term=bold,underline
      \ cterm=NONE ctermfg=DarkMagenta ctermbg=NONE
      \ gui=NONE guifg=DarkMagenta guibg=NONE
if &background == "dark"
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
