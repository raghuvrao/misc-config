" Plain colorscheme for vim.
" Maintainer:	Raghu V. Rao

" This colorscheme disables colors for most things.  The few things for which
" it defines colors are elements of Vim itself, and not any syntactic
" elements, with the exception of comments.  I want to highlight only comments
" because sometimes it is hard to spot lines of a program that have been
" disabled using multiline-comment syntax.  This colorscheme works best when
" the background is light and the foreground is dark.


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


hi Pmenu
      \ cterm=NONE ctermfg=Black ctermbg=Cyan
      \ gui=NONE guifg=Black guibg=LightBlue

hi PmenuSbar
      \ cterm=NONE ctermfg=NONE ctermbg=Black
      \ gui=NONE guifg=NONE guibg=Gray

hi PmenuSel
      \ cterm=NONE ctermfg=White ctermbg=DarkBlue
      \ gui=NONE guifg=White guibg=DarkBlue

hi PmenuThumb
      \ cterm=NONE ctermfg=NONE ctermbg=DarkYellow
      \ gui=NONE guifg=NONE guibg=Brown


hi NonText
      \ term=bold
      \ cterm=bold ctermfg=Blue ctermbg=NONE
      \ gui=bold guifg=Blue guibg=NONE


hi SpecialKey
      \ term=bold
      \ cterm=NONE ctermfg=DarkMagenta ctermbg=NONE
      \ gui=NONE guifg=DarkMagenta guibg=NONE


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


hi Visual term=reverse cterm=NONE gui=NONE
if has('gui_running') || &t_Co >= 16
  hi Visual ctermfg=Black ctermbg=Cyan guifg=Black guibg=LightBlue
else
  hi Visual cterm=reverse ctermfg=NONE ctermbg=NONE
endif


hi diffAdded term=bold cterm=NONE gui=NONE
if has('gui_running') || &t_Co >= 256
  hi diffAdded ctermfg=Black ctermbg=LightGreen guifg=Black guibg=LightGreen
else
  hi diffAdded ctermfg=DarkBlue ctermbg=NONE
endif

hi diffFile
      \ term=bold
      \ cterm=NONE ctermfg=DarkMagenta ctermbg=NONE
      \ gui=bold guifg=DarkMagenta guibg=NONE

hi diffLine
      \ term=bold
      \ cterm=NONE ctermfg=Brown ctermbg=NONE
      \ gui=NONE guifg=Brown guibg=NONE

hi diffRemoved term=bold cterm=NONE gui=NONE
if has('gui_running') || &t_Co >= 256
  hi diffRemoved ctermfg=Black ctermbg=LightRed guifg=Black guibg=LightRed
else
  hi diffRemoved ctermfg=DarkRed ctermbg=NONE
endif


hi gitcommitBranch
      \ term=bold,underline
      \ cterm=NONE ctermfg=DarkMagenta ctermbg=NONE
      \ gui=NONE guifg=DarkMagenta guibg=NONE


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
