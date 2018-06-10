" Mostly-plain colorscheme for dark-foreground-light-background screens.
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

if &t_Co <= 8
  hi ColorColumn ctermfg=NONE ctermbg=DarkGreen cterm=NONE
else
  hi ColorColumn ctermfg=NONE ctermbg=Yellow cterm=NONE
endif

hi Comment ctermfg=DarkYellow ctermbg=NONE cterm=NONE guifg=Brown guibg=NONE gui=NONE term=bold

hi Error ctermfg=DarkRed ctermbg=NONE cterm=NONE guifg=Red guibg=NONE gui=NONE term=reverse

hi ErrorMsg ctermfg=DarkRed ctermbg=NONE cterm=NONE guifg=Red guibg=NONE gui=NONE term=standout

hi Folded ctermfg=DarkCyan ctermbg=NONE cterm=NONE guifg=DarkCyan guibg=NONE gui=NONE term=standout

hi MoreMsg ctermfg=DarkGreen ctermbg=NONE cterm=NONE guifg=DarkGreen guibg=NONE gui=bold term=bold

hi NonText ctermfg=Blue ctermbg=NONE cterm=bold guifg=Blue guibg=NONE gui=bold term=bold

hi Pmenu cterm=NONE guifg=Black guibg=LightBlue gui=NONE
hi PmenuSbar ctermfg=NONE cterm=NONE guifg=NONE guibg=LightBlue gui=NONE
hi PmenuSel ctermfg=White cterm=bold guifg=White guibg=DarkBlue gui=bold
hi PmenuThumb ctermfg=NONE cterm=NONE guifg=NONE guibg=DarkCyan gui=NONE
if &t_Co >= 16
  hi Pmenu ctermfg=Black ctermbg=Cyan
  hi PmenuSbar ctermbg=Cyan
  hi PmenuSel ctermbg=DarkBlue
  hi PmenuThumb ctermbg=DarkCyan
else
  hi Pmenu ctermfg=White ctermbg=DarkBlue cterm=bold
  hi PmenuSbar ctermbg=DarkBlue
  hi PmenuSel ctermbg=DarkMagenta
  hi PmenuThumb ctermbg=DarkCyan
endif

hi Question ctermfg=DarkGreen ctermbg=NONE cterm=NONE guifg=DarkGreen guibg=NONE gui=bold term=standout

hi Search ctermfg=Black ctermbg=Green cterm=NONE guifg=Black guibg=LightGreen gui=NONE term=reverse

hi SpecialKey ctermfg=DarkCyan ctermbg=NONE cterm=NONE guifg=DarkCyan guibg=NONE gui=NONE term=bold

hi StatusLine ctermfg=White ctermbg=DarkBlue cterm=bold guifg=White guibg=DarkBlue gui=bold term=reverse,bold
hi StatusLineNC ctermfg=LightGray ctermbg=DarkBlue cterm=NONE guifg=LightGray guibg=DarkBlue gui=NONE term=reverse
hi clear StatusLineTerm
hi link StatusLineTerm StatusLine
hi clear StatusLineTermNC
hi link StatusLineTermNC StatusLineNC

hi TabLine ctermfg=DarkBlue ctermbg=LightGray cterm=NONE term=reverse
hi TabLineFill ctermfg=DarkBlue ctermbg=LightGray cterm=NONE term=reverse
hi TabLineSel ctermfg=White ctermbg=DarkBlue cterm=bold term=bold

hi Todo ctermfg=Blue ctermbg=NONE cterm=bold guifg=Blue guibg=NONE gui=bold term=standout

hi VertSplit ctermfg=DarkBlue ctermbg=NONE cterm=NONE guifg=DarkBlue guibg=NONE gui=NONE term=reverse

hi Visual ctermfg=Black cterm=NONE guifg=Black guibg=LightBlue gui=NONE term=reverse
if &t_Co >= 16
  hi Visual ctermbg=Cyan
else
  hi Visual ctermbg=DarkCyan
endif

hi WildMenu ctermfg=White ctermbg=DarkMagenta cterm=bold guifg=White guibg=DarkMagenta gui=bold term=standout

hi diffAdded ctermfg=DarkGreen ctermbg=NONE cterm=NONE guifg=DarkGreen guibg=NONE gui=NONE term=bold
hi diffFile ctermfg=NONE ctermbg=NONE cterm=bold guifg=NONE guibg=NONE gui=bold term=bold
hi diffLine ctermfg=DarkCyan ctermbg=NONE cterm=NONE guifg=DarkCyan guibg=NONE gui=NONE term=bold
hi diffRemoved ctermfg=DarkRed ctermbg=NONE cterm=NONE guifg=DarkRed guibg=NONE gui=NONE term=underline

hi gitHead ctermfg=Blue ctermbg=NONE cterm=bold guifg=Blue guibg=NONE gui=bold term=bold
hi link gitIdentityKeyword gitKeyword
hi gitKeyword ctermfg=Brown ctermbg=NONE cterm=NONE guifg=Brown guibg=NONE gui=bold term=bold

hi gitcommitBranch ctermfg=DarkMagenta ctermbg=NONE cterm=NONE guifg=DarkMagenta guibg=NONE gui=NONE term=bold,underline
hi link gitcommitHeader gitcommitComment
hi link gitcommitOverflow gitcommitComment
hi link gitcommitType gitcommitComment
hi link gitcommitFile gitcommitComment

hi link cCommentString cComment

hi link javaCommentTitle javaComment

hi link javaDocParam javaDocComment
hi link javaDocSeeTagParam javaDocComment
hi link javaDocTags javaDocComment

hi netrwClassify ctermfg=DarkCyan ctermbg=NONE cterm=NONE guifg=DarkCyan guibg=NONE gui=NONE term=underline
hi link netrwDir netrwPlain
hi link netrwSymLink netrwPlain

if &t_Co <= 8
  hi qfLineNr ctermfg=DarkYellow ctermbg=NONE cterm=NONE
  hi qfFileName ctermfg=DarkMagenta ctermbg=NONE cterm=NONE
endif

hi link vimCommentString vimComment
hi link vimCommentTitle vimComment
hi link vimCommentTitleLeader vimComment
