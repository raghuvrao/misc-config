" Mostly-plain colorscheme for dark-foreground-light-background screens.
"
" Maintainer: Raghu V. Rao <raghu.v.rao@gmail.com>
"
" For text-terminals that support color, this colorscheme makes the
" reasonable assumption that the 'upper' 8 colors (colors 8-15, or
" colors 0-7 + bold) of the terminal are bright variants of the
" corresponding 'lower' 8 colors (colors 0-7).

set background=light

hi clear

if exists('g:syntax_on')
    syntax reset
endif

let g:colors_name = 'raghuvrao'

hi clear Conceal
hi clear Constant
hi clear Identifier
hi clear Keyword
hi clear PreProc
hi clear Special
hi clear Statement
hi clear String
hi clear Type

hi ColorColumn ctermfg=NONE ctermbg=Yellow cterm=NONE guifg=NONE guibg=Yellow gui=NONE term=reverse
if &t_Co < 16
    hi ColorColumn ctermbg=DarkGreen
endif

hi Comment ctermfg=Blue ctermbg=NONE cterm=NONE guifg=Blue guibg=NONE gui=NONE term=bold
if &t_Co < 16
    hi Comment ctermfg=DarkBlue
endif
hi link SpecialComment Comment

hi Error ctermfg=DarkRed ctermbg=NONE cterm=NONE guifg=Red guibg=NONE gui=bold term=reverse

hi ErrorMsg ctermfg=DarkRed ctermbg=NONE cterm=NONE guifg=Red guibg=NONE gui=bold term=standout

hi FoldColumn ctermfg=DarkBlue ctermbg=Yellow cterm=NONE guifg=DarkBlue guibg=LightGray gui=NONE term=standout
if &t_Co < 16
    hi FoldColumn ctermbg=White
endif

hi Folded ctermfg=DarkCyan ctermbg=NONE cterm=NONE guifg=DarkCyan guibg=NONE gui=NONE term=standout

hi Ignore ctermfg=Yellow ctermbg=NONE cterm=NONE guifg=LightGray guibg=NONE gui=NONE term=NONE

hi MatchParen ctermfg=Black ctermbg=Yellow cterm=NONE guifg=Black guibg=Yellow gui=underline,bold term=bold
if &t_Co < 16
    hi MatchParen ctermfg=Magenta ctermbg=NONE cterm=bold
endif

hi MoreMsg ctermfg=Blue ctermbg=NONE cterm=bold guifg=Blue guibg=NONE gui=bold term=bold

hi Normal ctermfg=NONE ctermbg=NONE cterm=NONE guifg=Black guibg=White gui=NONE term=NONE

hi NonText ctermfg=DarkMagenta ctermbg=NONE cterm=NONE guifg=DarkMagenta guibg=NONE gui=bold term=bold

hi Pmenu ctermfg=Black ctermbg=Cyan cterm=NONE guifg=Black guibg=LightBlue gui=NONE
hi PmenuSbar ctermfg=NONE ctermbg=Cyan cterm=NONE guifg=NONE guibg=LightBlue gui=NONE
hi PmenuSel ctermfg=White ctermbg=DarkBlue cterm=bold guifg=White guibg=DarkBlue gui=bold
hi PmenuThumb ctermfg=NONE ctermbg=DarkCyan cterm=NONE guifg=NONE guibg=DarkCyan gui=NONE
if &t_Co < 16
    hi Pmenu ctermfg=White ctermbg=DarkBlue cterm=bold
    hi PmenuSbar ctermbg=DarkBlue
    hi PmenuSel ctermbg=DarkMagenta
    hi PmenuThumb ctermbg=DarkCyan
endif

hi Question ctermfg=Blue ctermbg=NONE cterm=bold guifg=Blue guibg=NONE gui=bold term=standout

hi QuickFixLine ctermfg=Black ctermbg=Yellow cterm=NONE guifg=Black guibg=LightGreen gui=NONE term=reverse,bold
if &t_Co < 16
    hi QuickFixLine ctermbg=DarkGreen
endif

hi Search ctermfg=Black ctermbg=Yellow cterm=NONE guifg=Black guibg=Orange gui=underline term=reverse

hi SpecialKey ctermfg=DarkCyan ctermbg=NONE cterm=NONE guifg=DarkCyan guibg=NONE gui=NONE term=bold

hi StatusLine ctermfg=White ctermbg=DarkMagenta cterm=bold guifg=White guibg=DarkMagenta gui=bold term=reverse,bold
hi StatusLineNC ctermfg=White ctermbg=DarkBlue cterm=bold guifg=White guibg=DarkBlue gui=bold term=reverse
hi clear StatusLineTerm
hi link StatusLineTerm StatusLine
hi clear StatusLineTermNC
hi link StatusLineTermNC StatusLineNC

hi TabLine ctermfg=DarkBlue ctermbg=LightGray cterm=NONE guifg=DarkBlue guibg=LightGray gui=NONE term=reverse
hi TabLineFill ctermfg=DarkBlue ctermbg=LightGray cterm=NONE guifg=DarkBlue guibg=DarkGray gui=NONE term=reverse
hi TabLineSel ctermfg=White ctermbg=DarkBlue cterm=bold guifg=White guibg=SeaGreen gui=bold term=bold

hi Title ctermfg=DarkMagenta ctermbg=NONE cterm=NONE guifg=DarkMagenta guibg=NONE gui=bold term=bold

hi Todo ctermfg=DarkMagenta ctermbg=NONE cterm=NONE guifg=DarkMagenta guibg=NONE gui=bold term=standout

hi Underlined ctermfg=NONE ctermbg=NONE cterm=underline guifg=NONE guibg=NONE gui=underline term=underline

hi VertSplit ctermfg=DarkMagenta ctermbg=NONE cterm=NONE guifg=DarkMagenta guibg=NONE gui=NONE term=reverse

hi Visual ctermfg=Black ctermbg=Cyan cterm=NONE guifg=Black guibg=LightBlue gui=NONE term=reverse

hi WarningMsg ctermfg=DarkRed ctermbg=NONE cterm=NONE guifg=DarkRed guibg=NONE gui=bold term=standout

hi WildMenu ctermfg=White ctermbg=DarkBlue cterm=bold guifg=White guibg=DarkBlue gui=bold term=standout

hi diffAdded ctermfg=DarkGreen ctermbg=NONE cterm=NONE guifg=DarkGreen guibg=NONE gui=NONE term=bold
hi diffFile ctermfg=NONE ctermbg=NONE cterm=bold guifg=NONE guibg=NONE gui=bold term=bold
hi diffLine ctermfg=DarkCyan ctermbg=NONE cterm=NONE guifg=DarkCyan guibg=NONE gui=NONE term=bold
hi diffRemoved ctermfg=DarkRed ctermbg=NONE cterm=NONE guifg=DarkRed guibg=NONE gui=NONE term=underline

hi gitHash ctermfg=DarkCyan ctermbg=NONE cterm=NONE guifg=DarkCyan guibg=NONE gui=NONE term=bold
hi gitHead ctermfg=Blue ctermbg=NONE cterm=bold guifg=Blue guibg=NONE gui=bold term=bold
hi link gitIdentityKeyword gitKeyword
hi gitKeyword ctermfg=DarkMagenta ctermbg=NONE cterm=NONE guifg=Brown guibg=NONE gui=bold term=bold

hi gitcommitBranch ctermfg=DarkMagenta ctermbg=NONE cterm=NONE guifg=DarkMagenta guibg=NONE gui=NONE term=bold,underline
hi gitcommitHeader ctermfg=DarkMagenta ctermbg=NONE cterm=NONE guifg=DarkMagenta guibg=NONE gui=NONE term=bold,underline
hi gitcommitSummary ctermfg=DarkMagenta ctermbg=NONE cterm=NONE guifg=DarkMagenta guibg=NONE gui=bold term=bold
hi link gitcommitType gitcommitComment
hi link gitcommitFile gitcommitComment
hi gitcommitUntracked ctermfg=DarkRed ctermbg=NONE cterm=NONE guifg=DarkRed guibg=NONE gui=NONE term=bold

hi gitrebaseCommand ctermfg=DarkGreen ctermbg=NONE cterm=NONE guifg=DarkGreen guibg=NONE gui=NONE term=bold
hi gitrebaseDrop ctermfg=DarkRed ctermbg=NONE cterm=NONE guifg=DarkRed guibg=NONE gui=NONE term=standout
hi gitrebaseEdit ctermfg=DarkMagenta ctermbg=NONE cterm=NONE guifg=DarkMagenta guibg=NONE gui=NONE term=standout
hi gitrebaseExec ctermfg=DarkRed ctermbg=NONE cterm=NONE guifg=DarkRed guibg=NONE gui=NONE term=standout
hi gitrebaseFixup ctermfg=DarkMagenta ctermbg=NONE cterm=NONE guifg=DarkMagenta guibg=NONE gui=NONE term=bold
hi gitrebaseHash ctermfg=DarkCyan ctermbg=NONE cterm=NONE guifg=DarkCyan guibg=NONE gui=NONE term=bold
hi gitrebasePick ctermfg=DarkBlue ctermbg=NONE cterm=NONE guifg=DarkBlue guibg=NONE gui=NONE term=bold
hi gitrebaseReword ctermfg=DarkCyan ctermbg=NONE cterm=NONE guifg=DarkCyan guibg=NONE gui=NONE term=bold
hi gitrebaseSquash ctermfg=DarkMagenta ctermbg=NONE cterm=NONE guifg=DarkMagenta guibg=NONE gui=NONE term=bold

hi helpSpecial ctermfg=DarkMagenta ctermbg=NONE cterm=NONE guifg=DarkMagenta guibg=NONE gui=NONE term=bold
hi helpHyperTextEntry ctermfg=DarkRed ctermbg=NONE cterm=NONE guifg=DarkRed guibg=NONE gui=NONE term=bold
hi helpHyperTextJump ctermfg=DarkBlue ctermbg=NONE cterm=NONE guifg=DarkBlue guibg=NONE gui=NONE term=underline

" Also affects Markdown.
hi link htmlTitle NONE
hi link htmlLink NONE
hi link htmlH1 NONE
hi link htmlBold NONE
hi link htmlUnderline NONE
hi link htmlItalic NONE
hi link htmlBoldItalic NONE
hi link htmlUnderlineItalic NONE
hi link htmlBoldUnderline NONE
hi link htmlBoldUnderlineItalic NONE

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
hi qfLineNr ctermfg=DarkRed ctermbg=NONE cterm=NONE guifg=Brown guibg=NONE gui=NONE term=bold

hi link vimCommentString vimComment
hi link vimCommentTitle vimComment
hi link vimCommentTitleLeader vimComment

hi link xmlProcessingDelim NONE
