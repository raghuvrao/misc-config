" Mostly-plain colorscheme for light-foreground-dark-background screens.
"
" Author: Raghu V. Rao <raghu.v.rao@gmail.com>
"
" For text-terminals that support color, this colorscheme makes the
" reasonable assumption that the 'upper' 8 colors (colors 8-15, or
" colors 0-7 + bold) of the terminal are bright variants of the
" corresponding 'lower' 8 colors (colors 0-7).

set background=dark

hi clear

if exists('g:syntax_on')
  syntax reset
endif

let g:colors_name = 'raghuvrao_lfdb0'

hi ColorColumn ctermfg=NONE ctermbg=DarkBlue cterm=NONE guifg=NONE guibg='#3030BF' gui=NONE term=reverse

hi Comment ctermfg=DarkCyan ctermbg=NONE cterm=NONE guifg='#30BFBF' guibg=NONE gui=NONE term=bold
hi link SpecialComment Comment

hi CursorColumn ctermfg=NONE ctermbg=DarkGray cterm=NONE guifg=NONE guibg='#333333' gui=NONE term=reverse
if &t_Co < 16
  hi CursorColumn ctermbg=DarkBlue
endif

hi CursorLine ctermfg=NONE ctermbg=NONE cterm=underline guifg=NONE guibg=NONE gui=underline term=underline

hi CursorLineNr ctermfg=Yellow ctermbg=NONE cterm=bold guifg='#FFFF40' guibg=NONE gui=bold term=bold

hi Directory ctermfg=Cyan ctermbg=NONE cterm=NONE guifg='#30BFBF' guibg=NONE gui=NONE term=bold
if &t_Co < 16
  hi Directory ctermfg=DarkCyan
endif

hi DiffAdd ctermfg=Green ctermbg=NONE cterm=NONE guifg='#40FF40' guibg=NONE gui=NONE
hi DiffChange ctermfg=DarkCyan ctermbg=NONE cterm=NONE guifg='#30BFBF' guibg=NONE gui=NONE
hi DiffDelete ctermfg=Red ctermbg=NONE cterm=NONE guifg='#FF4040' guibg=NONE gui=NONE
hi DiffText ctermfg=Cyan ctermbg=NONE cterm=NONE guifg='#40FFFF' guibg=NONE gui=NONE
if &t_Co < 16
  hi DiffAdd cterm=bold
  hi DiffChange cterm=bold
  hi DiffDelete cterm=bold
endif

hi EndOfBuffer ctermfg=DarkGray ctermbg=NONE cterm=NONE guifg='#737373' guibg=NONE gui=NONE term=bold
if &t_Co < 16
  hi EndOfBuffer ctermfg=DarkYellow
endif

hi Error ctermfg=White ctermbg=DarkRed cterm=NONE guifg='#FFFFFF' guibg='#BF3030' gui=NONE term=reverse
if &t_Co < 16
  hi Error cterm=bold
endif

hi ErrorMsg ctermfg=White ctermbg=DarkRed cterm=NONE guifg='#FFFFFF' guibg='#BF3030' gui=NONE term=standout
if &t_Co < 16
  hi ErrorMsg cterm=bold
endif

hi FoldColumn ctermfg=Cyan ctermbg=DarkGray cterm=NONE guifg='#40FFFF' guibg='#404040' gui=NONE term=standout
if &t_Co < 16
  hi FoldColumn ctermfg=DarkCyan ctermbg=Black cterm=bold
endif

hi Folded ctermfg=DarkYellow ctermbg=NONE cterm=NONE guifg='#BF7830' guibg=NONE gui=NONE term=standout

hi Ignore ctermfg=DarkGray ctermbg=NONE cterm=NONE guifg='#737373' guibg=NONE gui=NONE term=NONE

hi LineNr ctermfg=DarkYellow ctermbg=NONE cterm=NONE guifg='#BF7830' guibg=NONE gui=NONE term=bold

hi MatchParen ctermfg=White ctermbg=DarkBlue cterm=bold guifg='#FFFFFF' guibg='#3030BF' gui=bold term=bold

hi ModeMsg ctermfg=NONE ctermbg=NONE cterm=bold guifg=NONE guibg=NONE gui=bold term=bold

hi MoreMsg ctermfg=Green ctermbg=NONE cterm=NONE guifg='#40FF40' guibg=NONE gui=NONE term=bold
if &t_Co < 16
  hi MoreMsg cterm=bold
endif

hi NonText ctermfg=Blue ctermbg=NONE cterm=bold guifg='#407FFF' guibg=NONE gui=bold term=bold
if &t_Co < 16
  hi NonText cterm=bold
endif

hi Normal ctermfg=NONE ctermbg=NONE cterm=NONE guifg='#BFA760' guibg='#262626' gui=NONE term=NONE

hi Pmenu ctermfg=White ctermbg=DarkBlue cterm=NONE guifg='#FFFFFF' guibg='#3030BF' gui=NONE
hi PmenuSbar ctermfg=NONE ctermbg=DarkBlue cterm=NONE guifg=NONE guibg='#3078BF' gui=NONE
hi PmenuSel ctermfg=White ctermbg=DarkMagenta cterm=bold guifg='#FFFFFF' guibg='#BF30BF' gui=bold
hi PmenuThumb ctermfg=NONE ctermbg=DarkCyan cterm=NONE guifg=NONE guibg='#30BFBF' gui=NONE
if &t_Co < 16
  hi Pmenu cterm=bold
  hi PmenuSel cterm=bold
endif

hi Question ctermfg=Green ctermbg=NONE cterm=NONE guifg='#40FF40' guibg=NONE gui=NONE term=standout
if &t_Co < 16
  hi Question cterm=bold
endif

hi QuickFixLine ctermfg=Black ctermbg=DarkGreen cterm=NONE guifg='#000000' guibg='#30BF30' gui=NONE term=reverse,bold

hi Search ctermfg=Black ctermbg=Magenta cterm=NONE guifg='#000000' guibg='#FF40FF' gui=NONE term=reverse
if &t_Co < 16
  hi Search ctermfg=White ctermbg=DarkMagenta cterm=bold
endif

hi SignColumn ctermfg=Cyan ctermbg=DarkGray cterm=NONE guifg='#40FFFF' guibg='#404040' gui=NONE term=standout
if &t_Co < 16
  hi SignColumn ctermfg=DarkCyan ctermbg=Black cterm=bold
endif

hi SpecialKey ctermfg=DarkMagenta ctermbg=NONE cterm=NONE guifg='#BF30BF' guibg=NONE gui=NONE term=bold

hi StatusLine ctermfg=Black ctermbg=DarkCyan cterm=NONE guifg='#000000' guibg='#30BFBF' gui=bold term=reverse,bold
hi StatusLineNC ctermfg=Black ctermbg=DarkGray cterm=NONE guifg='#000000' guibg='#737373' gui=NONE term=reverse
if &t_Co < 16
  hi StatusLineNC ctermbg=LightGray
endif
hi clear StatusLineTerm
hi link StatusLineTerm StatusLine
hi clear StatusLineTermNC
hi link StatusLineTermNC StatusLineNC

hi clear TabLine
hi link TabLine StatusLineNC
hi clear TabLineFill
hi link TabLineFill TabLine
hi clear TabLineSel
hi link TabLineSel StatusLine

hi Title ctermfg=NONE ctermbg=NONE cterm=bold guifg=NONE guibg=NONE gui=bold term=bold

hi Todo ctermfg=Magenta ctermbg=NONE cterm=NONE guifg='#FF40FF' guibg=NONE gui=bold term=standout

hi Underlined ctermfg=NONE ctermbg=NONE cterm=underline guifg=NONE guibg=NONE gui=underline term=underline

hi VertSplit ctermfg=DarkYellow ctermbg=NONE cterm=NONE guifg='#BF7830' guibg=NONE gui=NONE term=reverse

hi Visual ctermfg=White ctermbg=DarkBlue cterm=NONE guifg='#FFFFFF' guibg='#3060BF' gui=NONE term=reverse
if &t_Co < 16
  hi Visual cterm=bold
endif
hi VisualNOS ctermfg=Black ctermbg=LightGray cterm=NONE guifg='#000000' guibg='#BFBFBF' gui=NONE term=reverse

hi WarningMsg ctermfg=Red ctermbg=NONE cterm=bold guifg='#FF4040' guibg=NONE gui=bold term=standout

hi WildMenu ctermfg=Black ctermbg=DarkYellow cterm=NONE guifg='#000000' guibg='#BF7830' gui=bold term=standout

hi diffAdded ctermfg=Green ctermbg=NONE cterm=NONE guifg='#40FF40' guibg=NONE gui=NONE term=bold
hi diffFile ctermfg=NONE ctermbg=NONE cterm=bold guifg=NONE guibg=NONE gui=bold term=bold
hi diffLine ctermfg=Cyan ctermbg=NONE cterm=NONE guifg='#40FFFF' guibg=NONE gui=NONE term=bold
hi diffRemoved ctermfg=Red ctermbg=NONE cterm=NONE guifg='#FF4040' guibg=NONE gui=NONE term=underline
if &t_Co < 16
  hi diffAdded cterm=bold
  hi diffLine cterm=bold
  hi diffRemoved cterm=bold
endif

hi gitHash ctermfg=Cyan ctermbg=NONE cterm=NONE guifg='#40FFFF' guibg=NONE gui=NONE term=bold
hi gitHead ctermfg=Blue ctermbg=NONE cterm=bold guifg='#407FFF' guibg=NONE gui=bold term=bold
hi gitKeyword ctermfg=DarkYellow ctermbg=NONE cterm=NONE guifg='#BF7830' guibg=NONE gui=bold term=bold
if &t_Co < 16
  hi gitHash ctermfg=DarkCyan cterm=bold
  hi gitHead ctermfg=DarkRed
endif
hi link gitIdentityKeyword gitKeyword

hi gitcommitBranch ctermfg=Magenta ctermbg=NONE cterm=NONE guifg='#FF40FF' guibg=NONE gui=NONE term=bold,underline
hi gitcommitHeader ctermfg=Magenta ctermbg=NONE cterm=NONE guifg='#FF40FF' guibg=NONE gui=NONE term=bold,underline
hi gitcommitOverflow ctermfg=DarkYellow ctermbg=NONE cterm=NONE guifg='#BFBF30' guibg=NONE gui=NONE term=bold
hi gitcommitUntracked ctermfg=Red ctermbg=NONE cterm=NONE guifg='#FF4040' guibg=NONE gui=NONE term=bold
if &t_Co < 16
  hi gitcommitBranch cterm=bold
  hi gitcommitHeader cterm=bold
  hi gitcommitSummary cterm=bold
  hi gitcommitUntracked cterm=bold
endif
hi link gitcommitType gitcommitComment
hi link gitcommitFile gitcommitComment

hi gitrebaseCommand ctermfg=Green ctermbg=NONE cterm=NONE guifg='#40FF40' guibg=NONE gui=NONE term=bold
hi gitrebaseDrop ctermfg=Red ctermbg=NONE cterm=NONE guifg='#FF4040' guibg=NONE gui=NONE term=standout
hi gitrebaseEdit ctermfg=Magenta ctermbg=NONE cterm=NONE guifg='#FF40FF' guibg=NONE gui=NONE term=standout
hi gitrebaseExec ctermfg=Red ctermbg=NONE cterm=NONE guifg='#FF4040' guibg=NONE gui=NONE term=standout
hi gitrebaseFixup ctermfg=Magenta ctermbg=NONE cterm=NONE guifg='#FF40FF' guibg=NONE gui=NONE term=bold
hi gitrebaseHash ctermfg=Yellow ctermbg=NONE cterm=NONE guifg='#FFFF40' guibg=NONE gui=NONE term=bold
hi gitrebasePick ctermfg=Cyan ctermbg=NONE cterm=NONE guifg='#40FFFF' guibg=NONE gui=NONE term=bold
hi gitrebaseReword ctermfg=Cyan ctermbg=NONE cterm=NONE guifg='#40FFFF' guibg=NONE gui=NONE term=bold
hi gitrebaseSquash ctermfg=Magenta ctermbg=NONE cterm=NONE guifg='#FF40FF' guibg=NONE gui=NONE term=bold
if &t_Co < 16
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

hi helpSpecial ctermfg=Magenta ctermbg=NONE cterm=NONE guifg='#FF40FF' guibg=NONE gui=NONE term=bold
hi helpHyperTextEntry ctermfg=Green ctermbg=NONE cterm=NONE guifg='#40FF40' guibg=NONE gui=NONE term=bold
hi helpHyperTextJump ctermfg=Yellow ctermbg=NONE cterm=NONE guifg='#FFFF40' guibg=NONE gui=underline term=underline
if &t_Co < 16
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

hi netrwClassify ctermfg=Cyan ctermbg=NONE cterm=NONE guifg='#40FFFF' guibg=NONE gui=NONE term=underline
hi link netrwDir netrwPlain
hi link netrwSymLink netrwPlain

hi link perlPOD NONE
hi link perlSharpBang perlComment

hi pythonBuiltin ctermfg=NONE ctermbg=NONE cterm=underline guifg=NONE guibg=NONE gui=underline term=underline

hi link specGlobalMacro NONE

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
