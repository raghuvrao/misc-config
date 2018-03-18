" Mostly plain colorscheme for Vim.
" Maintainer:	Raghu V. Rao

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

hi Comment ctermfg=Brown ctermbg=NONE cterm=NONE guifg=Brown guibg=NONE gui=NONE term=bold

hi Error ctermfg=White ctermbg=DarkRed cterm=NONE guifg=White guibg=DarkRed gui=NONE term=reverse

hi ErrorMsg ctermfg=White ctermbg=DarkRed cterm=NONE guifg=White guibg=DarkRed gui=NONE term=standout

hi Folded ctermfg=DarkCyan ctermbg=NONE cterm=NONE guifg=DarkCyan guibg=NONE gui=NONE term=standout

hi NonText ctermfg=Blue ctermbg=NONE cterm=bold guifg=Blue guibg=NONE gui=bold term=bold

hi Pmenu ctermfg=Black ctermbg=Cyan cterm=NONE guifg=Black guibg=LightBlue gui=NONE
hi PmenuSbar ctermfg=NONE ctermbg=Black cterm=NONE guifg=NONE guibg=Gray gui=NONE
hi PmenuSel ctermfg=White ctermbg=DarkBlue cterm=NONE guifg=White guibg=DarkBlue gui=NONE
hi PmenuThumb ctermfg=NONE ctermbg=DarkYellow cterm=NONE guifg=NONE guibg=Brown gui=NONE

hi SpecialKey ctermfg=DarkMagenta ctermbg=NONE cterm=NONE guifg=DarkMagenta guibg=NONE gui=NONE term=bold

hi StatusLine ctermfg=White ctermbg=DarkMagenta cterm=bold guifg=White guibg=DarkMagenta gui=bold term=reverse,bold
hi StatusLineNC ctermfg=White ctermbg=DarkBlue cterm=bold guifg=White guibg=DarkBlue gui=bold term=reverse
hi clear StatusLineTerm
hi link StatusLineTerm StatusLine
hi clear StatusLineTermNC
hi link StatusLineTermNC StatusLineNC

hi TabLine ctermfg=DarkBlue ctermbg=LightGray cterm=NONE term=reverse
hi TabLineFill ctermfg=DarkBlue ctermbg=LightGray cterm=NONE term=reverse
hi TabLineSel ctermfg=White ctermbg=DarkBlue cterm=bold term=bold

hi Todo ctermfg=Blue ctermbg=NONE cterm=bold guifg=Blue guibg=NONE gui=bold term=standout

hi VertSplit ctermfg=DarkRed ctermbg=LightGray cterm=NONE guifg=DarkRed guibg=LightGray gui=NONE term=reverse

hi Visual term=reverse cterm=NONE gui=NONE
if has('gui_running') || &t_Co >= 16
  hi Visual ctermfg=Black ctermbg=Cyan guifg=Black guibg=LightBlue
else
  hi Visual cterm=reverse ctermfg=NONE ctermbg=NONE
endif

hi WildMenu ctermfg=White ctermbg=DarkMagenta cterm=bold guifg=White guibg=DarkMagenta gui=bold term=standout

hi diffAdded ctermfg=DarkGreen ctermbg=NONE cterm=NONE guifg=DarkGreen guibg=NONE gui=NONE term=bold
hi diffFile ctermfg=NONE ctermbg=NONE cterm=bold guifg=NONE guibg=NONE gui=bold term=bold
hi diffLine ctermfg=DarkCyan ctermbg=NONE cterm=NONE guifg=DarkCyan guibg=NONE gui=NONE term=bold
hi diffRemoved ctermfg=DarkRed ctermbg=NONE cterm=NONE guifg=DarkRed guibg=NONE gui=NONE term=underline

hi gitcommitBranch ctermfg=DarkMagenta ctermbg=NONE cterm=NONE guifg=DarkMagenta guibg=NONE gui=NONE term=bold,underline
hi link gitcommitHeader gitcommitComment
hi link gitcommitType gitcommitComment
hi link gitcommitFile gitcommitComment

hi link cCommentString cComment

hi link javaCommentTitle javaComment

hi link javaDocParam javaDocComment
hi link javaDocSeeTagParam javaDocComment
hi link javaDocTags javaDocComment

hi link vimCommentString vimComment
hi link vimCommentTitle vimComment
hi link vimCommentTitleLeader vimComment
