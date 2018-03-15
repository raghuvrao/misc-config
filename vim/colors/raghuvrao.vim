" Mostly plain colorscheme for vim.
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

hi Comment term=bold cterm=NONE ctermfg=Brown ctermbg=NONE gui=NONE guifg=Brown guibg=NONE

hi Error term=reverse cterm=NONE ctermfg=White ctermbg=DarkRed gui=NONE guifg=White guibg=DarkRed

hi ErrorMsg term=standout cterm=NONE ctermfg=White ctermbg=DarkRed gui=NONE guifg=White guibg=DarkRed

hi Folded term=standout cterm=NONE ctermfg=DarkCyan ctermbg=NONE gui=NONE guifg=DarkCyan guibg=NONE

hi NonText term=bold cterm=bold ctermfg=Blue ctermbg=NONE gui=bold guifg=Blue guibg=NONE

hi Pmenu cterm=NONE ctermfg=Black ctermbg=Cyan gui=NONE guifg=Black guibg=LightBlue
hi PmenuSbar cterm=NONE ctermfg=NONE ctermbg=Black gui=NONE guifg=NONE guibg=Gray
hi PmenuSel cterm=NONE ctermfg=White ctermbg=DarkBlue gui=NONE guifg=White guibg=DarkBlue
hi PmenuThumb cterm=NONE ctermfg=NONE ctermbg=DarkYellow gui=NONE guifg=NONE guibg=Brown

hi SpecialKey term=bold cterm=NONE ctermfg=DarkMagenta ctermbg=NONE gui=NONE guifg=DarkMagenta guibg=NONE

hi StatusLine term=reverse,bold cterm=bold ctermfg=White ctermbg=DarkMagenta gui=bold guifg=White guibg=DarkMagenta
hi StatusLineNC term=reverse cterm=bold ctermfg=White ctermbg=DarkBlue gui=bold guifg=White guibg=DarkBlue
hi link StatusLineTerm StatusLine
hi link StatusLineTermNC StatusLineNC

hi TabLine term=reverse cterm=NONE ctermfg=DarkBlue ctermbg=LightGray
hi TabLineFill term=reverse cterm=NONE ctermfg=DarkBlue ctermbg=LightGray
hi TabLineSel term=bold cterm=bold ctermfg=White ctermbg=DarkBlue

hi VertSplit term=reverse cterm=NONE ctermfg=DarkRed ctermbg=LightGray gui=NONE guifg=DarkRed guibg=LightGray

hi Visual term=reverse cterm=NONE gui=NONE
if has('gui_running') || &t_Co >= 16
  hi Visual ctermfg=Black ctermbg=Cyan guifg=Black guibg=LightBlue
else
  hi Visual cterm=reverse ctermfg=NONE ctermbg=NONE
endif

hi WildMenu term=standout cterm=bold ctermfg=White ctermbg=DarkMagenta gui=bold guifg=White guibg=DarkMagenta

hi diffAdded term=bold cterm=NONE ctermfg=DarkGreen ctermbg=NONE gui=NONE guifg=DarkGreen guibg=NONE
hi diffFile term=bold cterm=bold ctermfg=NONE ctermbg=NONE gui=bold guifg=NONE guibg=NONE
hi diffLine term=bold cterm=NONE ctermfg=DarkCyan ctermbg=NONE gui=NONE guifg=DarkCyan guibg=NONE
hi diffRemoved term=underline cterm=NONE ctermfg=DarkRed ctermbg=NONE gui=NONE guifg=DarkRed guibg=NONE

hi gitcommitBranch term=bold,underline cterm=NONE ctermfg=DarkMagenta ctermbg=NONE gui=NONE guifg=DarkMagenta guibg=NONE
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
