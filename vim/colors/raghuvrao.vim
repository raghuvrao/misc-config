" Mostly-plain colorscheme for dark-foreground-light-background screens.
" Maintainer:	Raghu Rao <raghu.v.rao@gmail.com>

set background=light

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

hi ColorColumn ctermfg=NONE ctermbg=Yellow cterm=NONE guifg=NONE guibg=Yellow gui=NONE term=reverse
if &t_Co <= 8
  hi ColorColumn ctermbg=DarkGreen
endif

hi Comment ctermfg=Brown ctermbg=NONE cterm=NONE guifg=Brown guibg=NONE gui=NONE term=bold

hi Error ctermfg=Red ctermbg=NONE cterm=bold guifg=Red guibg=NONE gui=bold term=reverse

hi ErrorMsg ctermfg=Red ctermbg=NONE cterm=bold guifg=Red guibg=NONE gui=bold term=standout

hi Folded ctermfg=DarkCyan ctermbg=NONE cterm=NONE guifg=DarkCyan guibg=NONE gui=NONE term=standout

hi MatchParen ctermfg=Black ctermbg=Yellow cterm=NONE guifg=Black guibg=Yellow gui=underline,bold term=bold

hi MoreMsg ctermfg=DarkGreen ctermbg=NONE cterm=NONE guifg=DarkGreen guibg=NONE gui=bold term=bold

hi NonText ctermfg=Blue ctermbg=NONE cterm=bold guifg=Blue guibg=NONE gui=bold term=bold

hi Pmenu ctermfg=Black ctermbg=Cyan cterm=NONE guifg=Black guibg=LightBlue gui=NONE
hi PmenuSbar ctermfg=NONE ctermbg=Cyan cterm=NONE guifg=NONE guibg=LightBlue gui=NONE
hi PmenuSel ctermfg=White ctermbg=DarkBlue cterm=bold guifg=White guibg=DarkBlue gui=bold
hi PmenuThumb ctermfg=NONE ctermbg=DarkCyan cterm=NONE guifg=NONE guibg=DarkCyan gui=NONE
if &t_Co <= 8
  hi Pmenu ctermfg=White ctermbg=DarkBlue cterm=bold
  hi PmenuSbar ctermbg=DarkBlue
  hi PmenuSel ctermbg=DarkMagenta
  hi PmenuThumb ctermbg=DarkCyan
endif

hi Question ctermfg=DarkGreen ctermbg=NONE cterm=NONE guifg=DarkGreen guibg=NONE gui=bold term=standout

hi QuickFixLine ctermfg=Black ctermbg=Yellow cterm=NONE guifg=Black guibg=LightGreen gui=NONE term=reverse,bold
if &t_Co <= 8
  hi QuickFixLine ctermbg=DarkGreen
endif

hi Search ctermfg=White ctermbg=Brown cterm=bold guifg=Black guibg=Orange gui=underline term=reverse

hi SpecialKey ctermfg=DarkCyan ctermbg=NONE cterm=NONE guifg=DarkCyan guibg=NONE gui=NONE term=bold

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

hi VertSplit ctermfg=DarkMagenta ctermbg=NONE cterm=NONE guifg=DarkMagenta guibg=NONE gui=NONE term=reverse

hi Visual ctermfg=Black ctermbg=Cyan cterm=NONE guifg=Black guibg=LightBlue gui=NONE term=reverse

hi WildMenu ctermfg=White ctermbg=DarkBlue cterm=bold guifg=White guibg=DarkBlue gui=bold term=standout

hi link cCommentString cComment

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

hi helpHyperTextEntry ctermfg=DarkGreen ctermbg=NONE cterm=NONE guifg=DarkGreen guibg=NONE gui=NONE term=bold
hi helpHyperTextJump ctermfg=DarkMagenta ctermbg=NONE cterm=NONE guifg=DarkMagenta guibg=NONE gui=NONE term=underline

hi link javaCommentTitle javaComment

hi link javaDocParam javaDocComment
hi link javaDocSeeTagParam javaDocComment
hi link javaDocTags javaDocComment

hi manOptionDesc ctermfg=NONE ctermbg=NONE cterm=bold guifg=NONE guibg=NONE gui=bold term=bold
hi link manCFuncDefinition manOptionDesc
hi link manLongOptionDesc manOptionDesc
hi manReference ctermfg=NONE ctermbg=NONE cterm=underline guifg=NONE guibg=NONE gui=underline term=underline
hi manTitle ctermfg=NONE ctermbg=NONE cterm=bold guifg=NONE guibg=NONE gui=bold term=bold
hi link manSectionHeading manTitle
hi link manSubHeading manTitle

hi netrwClassify ctermfg=DarkCyan ctermbg=NONE cterm=NONE guifg=DarkCyan guibg=NONE gui=NONE term=underline
hi link netrwDir netrwPlain
hi link netrwSymLink netrwPlain

hi pythonBuiltin ctermfg=NONE ctermbg=NONE cterm=underline guifg=NONE guibg=NONE gui=underline term=underline

hi qfFileName ctermfg=DarkMagenta ctermbg=NONE cterm=NONE guifg=DarkMagenta guibg=NONE gui=NONE term=bold
hi qfLineNr ctermfg=Brown ctermbg=NONE cterm=NONE guifg=Brown guibg=NONE gui=NONE term=bold

hi link vimCommentString vimComment
hi link vimCommentTitle vimComment
hi link vimCommentTitleLeader vimComment

hi link xmlProcessingDelim NONE
