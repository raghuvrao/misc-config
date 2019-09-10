" Mostly-plain colorscheme for light-foreground-dark-background screens.
"
" Maintainer:	Raghu V. Rao <raghu.v.rao@gmail.com>
"
" For text-terminals that support color, this colorscheme makes the reasonable
" assumption that the 'upper' 8 colors (colors 8-15, or colors 0-7 + bold) of
" the terminal are bright variants of the corresponding 'lower' 8 colors
" (colors 0-7).

if !has('gui_running') && &t_Co < 8
  echoerr 'Available colors: ' . &t_Co . '; selected colorscheme needs at least 8, looks best with 16 or more'
  finish
endif

set background=dark

hi clear

if exists('g:syntax_on')
  syntax reset
endif

let g:colors_name = 'raghuvrao_lfdb0'

hi ColorColumn ctermfg=NONE ctermbg=DarkGreen cterm=NONE guifg=NONE guibg=DarkGreen gui=NONE term=reverse
if &t_Co == 8
  hi ColorColumn ctermbg=DarkGreen
endif

hi Comment ctermfg=Cyan ctermbg=NONE cterm=NONE guifg=Cyan guibg=NONE gui=NONE term=bold
if &t_Co == 8
  hi Comment cterm=bold
endif
hi link SpecialComment Comment

hi CursorLine ctermfg=NONE ctermbg=NONE cterm=underline guifg=NONE guibg=NONE gui=underline term=underline

hi Error ctermfg=Red ctermbg=NONE cterm=bold guifg=Red guibg=NONE gui=bold term=reverse

hi ErrorMsg ctermfg=Red ctermbg=NONE cterm=bold guifg=Red guibg=NONE gui=bold term=standout

hi FoldColumn ctermfg=Black ctermbg=Gray cterm=NONE guifg=Black guibg=Gray gui=NONE term=standout

hi Folded ctermfg=DarkYellow ctermbg=NONE cterm=NONE guifg=DarkYellow guibg=NONE gui=NONE term=standout

hi Ignore ctermfg=DarkGray ctermbg=NONE cterm=NONE guifg=Gray guibg=NONE gui=NONE term=NONE

hi MatchParen ctermfg=White ctermbg=Blue cterm=bold guifg=White guibg=Blue gui=underline,bold term=bold

hi MoreMsg ctermfg=Green ctermbg=NONE cterm=NONE guifg=Blue guibg=NONE gui=NONE term=bold
if &t_Co == 8
  hi MoreMsg cterm=bold
endif

hi Normal ctermfg=NONE ctermbg=NONE cterm=NONE guifg=LightGray guibg=Black gui=NONE term=NONE

hi NonText ctermfg=Blue ctermbg=NONE cterm=bold guifg=Blue guibg=NONE gui=bold term=bold
if &t_Co == 8
  hi NonText cterm=bold
endif

hi Pmenu ctermfg=White ctermbg=DarkBlue cterm=NONE guifg=White guibg=DarkBlue gui=NONE
hi PmenuSbar ctermfg=NONE ctermbg=DarkBlue cterm=NONE guifg=NONE guibg=DarkBlue gui=NONE
hi PmenuSel ctermfg=White ctermbg=DarkMagenta cterm=bold guifg=White guibg=DarkMagenta gui=bold
hi PmenuThumb ctermfg=NONE ctermbg=DarkCyan cterm=NONE guifg=NONE guibg=DarkCyan gui=NONE
if &t_Co == 8
  hi Pmenu cterm=bold
  hi PmenuSel cterm=bold
endif

hi Question ctermfg=Green ctermbg=NONE cterm=NONE guifg=Green guibg=NONE gui=NONE term=standout
if &t_Co == 8
  hi Question cterm=bold
endif

hi QuickFixLine ctermfg=Black ctermbg=DarkGreen cterm=NONE guifg=Black guibg=DarkGreen gui=NONE term=reverse,bold

hi Search ctermfg=White ctermbg=DarkMagenta cterm=NONE guifg=White guibg=DarkMagenta gui=NONE term=reverse
if &t_Co == 8
  hi Search cterm=bold
endif

hi SpecialKey ctermfg=Magenta ctermbg=NONE cterm=NONE guifg=Magenta guibg=NONE gui=NONE term=bold
if &t_Co == 8
  hi SpecialKey cterm=bold
endif

hi StatusLine ctermfg=NONE ctermbg=NONE cterm=reverse,bold guifg=NONE guibg=NONE gui=reverse,bold term=reverse,bold
hi StatusLineNC ctermfg=NONE ctermbg=NONE cterm=reverse guifg=NONE guibg=NONE gui=reverse term=reverse
hi clear StatusLineTerm
hi link StatusLineTerm StatusLine
hi clear StatusLineTermNC
hi link StatusLineTermNC StatusLineNC

hi TabLine ctermfg=Black ctermbg=LightGray cterm=NONE guifg=DarkBlue guibg=LightGray gui=NONE term=reverse
hi TabLineFill ctermfg=Black ctermbg=LightGray cterm=NONE guifg=DarkBlue guibg=DarkGray gui=NONE term=reverse
hi TabLineSel ctermfg=White ctermbg=DarkBlue cterm=bold guifg=White guibg=DarkBlue gui=bold term=bold

hi Title ctermfg=DarkMagenta ctermbg=NONE cterm=NONE guifg=DarkMagenta guibg=NONE gui=bold term=bold

hi Todo ctermfg=DarkMagenta ctermbg=NONE cterm=NONE guifg=DarkMagenta guibg=NONE gui=bold term=standout

hi Underlined ctermfg=NONE ctermbg=NONE cterm=underline guifg=NONE guibg=NONE gui=underline term=underline

hi VertSplit ctermfg=DarkYellow ctermbg=NONE cterm=NONE guifg=DarkYellow guibg=NONE gui=NONE term=reverse

hi Visual ctermfg=Black ctermbg=DarkCyan cterm=NONE guifg=Black guibg=DarkCyan gui=NONE term=reverse

hi WarningMsg ctermfg=Red ctermbg=NONE cterm=NONE guifg=Red guibg=NONE gui=bold term=standout
if &t_Co == 8
  hi WarningMsg cterm=bold
endif

hi WildMenu ctermfg=White ctermbg=DarkBlue cterm=bold guifg=White guibg=DarkBlue gui=bold term=standout

hi diffAdded ctermfg=Green ctermbg=NONE cterm=NONE guifg=Green guibg=NONE gui=NONE term=bold
hi diffFile ctermfg=NONE ctermbg=NONE cterm=bold guifg=NONE guibg=NONE gui=bold term=bold
hi diffLine ctermfg=Cyan ctermbg=NONE cterm=NONE guifg=Cyan guibg=NONE gui=NONE term=bold
hi diffRemoved ctermfg=Red ctermbg=NONE cterm=NONE guifg=Red guibg=NONE gui=NONE term=underline
if &t_Co == 8
  hi diffAdded cterm=bold
  hi diffLine cterm=bold
  hi diffRemoved cterm=bold
endif

hi gitHash ctermfg=Cyan ctermbg=NONE cterm=NONE guifg=Cyan guibg=NONE gui=NONE term=bold
hi gitHead ctermfg=Blue ctermbg=NONE cterm=bold guifg=Blue guibg=NONE gui=bold term=bold
hi gitKeyword ctermfg=Magenta ctermbg=NONE cterm=NONE guifg=Brown guibg=NONE gui=bold term=bold
if &t_Co == 8
  hi gitHash cterm=bold
  hi gitKeyword cterm=bold
endif
hi link gitIdentityKeyword gitKeyword

hi gitcommitBranch ctermfg=Magenta ctermbg=NONE cterm=NONE guifg=Magenta guibg=NONE gui=NONE term=bold,underline
hi gitcommitHeader ctermfg=Magenta ctermbg=NONE cterm=NONE guifg=Magenta guibg=NONE gui=NONE term=bold,underline
hi gitcommitSummary ctermfg=Magenta ctermbg=NONE cterm=NONE guifg=Magenta guibg=NONE gui=bold term=bold
hi gitcommitUntracked ctermfg=Red ctermbg=NONE cterm=NONE guifg=Red guibg=NONE gui=NONE term=bold
if &t_Co == 8
  hi gitcommitBranch cterm=bold
  hi gitcommitHeader cterm=bold
  hi gitcommitSummary cterm=bold
  hi gitcommitUntracked cterm=bold
endif
hi link gitcommitType gitcommitComment
hi link gitcommitFile gitcommitComment

hi gitrebaseCommand ctermfg=Green ctermbg=NONE cterm=NONE guifg=Green guibg=NONE gui=NONE term=bold
hi gitrebaseDrop ctermfg=Red ctermbg=NONE cterm=NONE guifg=Red guibg=NONE gui=NONE term=standout
hi gitrebaseEdit ctermfg=Magenta ctermbg=NONE cterm=NONE guifg=Magenta guibg=NONE gui=NONE term=standout
hi gitrebaseExec ctermfg=Red ctermbg=NONE cterm=NONE guifg=Red guibg=NONE gui=NONE term=standout
hi gitrebaseFixup ctermfg=Magenta ctermbg=NONE cterm=NONE guifg=Magenta guibg=NONE gui=NONE term=bold
hi gitrebaseHash ctermfg=Yellow ctermbg=NONE cterm=NONE guifg=Yellow guibg=NONE gui=NONE term=bold
hi gitrebasePick ctermfg=Cyan ctermbg=NONE cterm=NONE guifg=Cyan guibg=NONE gui=NONE term=bold
hi gitrebaseReword ctermfg=Cyan ctermbg=NONE cterm=NONE guifg=Cyan guibg=NONE gui=NONE term=bold
hi gitrebaseSquash ctermfg=Magenta ctermbg=NONE cterm=NONE guifg=Magenta guibg=NONE gui=NONE term=bold
if &t_Co == 8
  hi gitrebaseCommand cterm=bold
  hi gitrebaseDrop cterm=bold
  hi gitrebaseEdit cterm=bold
  hi gitrebaseExec cterm=bold
  hi gitrebaseFixup cterm=bold
  hi gitrebaseHash cterm=bold
  hi gitrebasePick cterm=bold
  hi gitrebaseReword cterm=bold
  hi gitrebaseSquash cterm=bold
endif

hi helpSpecial ctermfg=Magenta ctermbg=NONE cterm=NONE guifg=Magenta guibg=NONE gui=NONE term=bold
hi helpHyperTextEntry ctermfg=Green ctermbg=NONE cterm=NONE guifg=Green guibg=NONE gui=bold term=bold
hi helpHyperTextJump ctermfg=Yellow ctermbg=NONE cterm=NONE guifg=Yellow guibg=NONE gui=underline term=underline
if &t_Co == 8
  hi helpSpecial cterm=bold
  hi helpHyperTextEntry cterm=bold
  hi helpHyperTextJump cterm=bold
endif

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

hi link perlPOD NONE
hi link perlSharpBang perlComment

hi pythonBuiltin ctermfg=NONE ctermbg=NONE cterm=underline guifg=NONE guibg=NONE gui=underline term=underline

hi qfFileName ctermfg=Magenta ctermbg=NONE cterm=NONE guifg=Magenta guibg=NONE gui=NONE term=bold
hi qfLineNr ctermfg=DarkYellow ctermbg=NONE cterm=NONE guifg=DarkYellow guibg=NONE gui=NONE term=bold
if &t_Co == 8
  hi qfFileName cterm=bold
  hi qfLineNr cterm=bold
endif

hi link vimCommentString vimComment
hi link vimCommentTitle vimComment
hi link vimCommentTitleLeader vimComment

hi link xmlProcessingDelim NONE

hi clear Conceal
hi clear Constant
hi clear Identifier
hi clear Keyword
hi clear PreProc
hi clear Special
hi clear Statement
hi clear String
hi clear Type
