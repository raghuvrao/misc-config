" Mostly-plain light-background color scheme.
"
" Author: Raghu V. Rao <raghu.v.rao@gmail.com>

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
hi clear Terminal
hi clear Type

hi Cursor guifg=White guibg=#0077FF gui=NONE

if &t_Co >= 256
    hi CursorLine ctermfg=NONE ctermbg=254 cterm=NONE
endif

hi EndOfBuffer ctermfg=Blue ctermbg=NONE cterm=bold guifg=Blue guibg=NONE gui=bold

hi Error ctermfg=Red ctermbg=NONE cterm=bold guifg=Red guibg=NONE gui=bold

hi Folded ctermfg=DarkCyan ctermbg=NONE cterm=NONE guifg=DarkCyan guibg=NONE gui=NONE

hi Ignore ctermfg=LightGrey ctermbg=NONE cterm=NONE guifg=LightGrey guibg=NONE gui=NONE

hi NonText ctermfg=DarkCyan ctermbg=NONE cterm=NONE guifg=DarkCyan guibg=NONE gui=bold

hi Normal ctermfg=NONE ctermbg=NONE cterm=NONE guifg=Black guibg=White gui=NONE

if &t_Co >= 256
    hi Search ctermbg=193
endif

hi SpecialKey ctermfg=DarkCyan ctermbg=NONE cterm=NONE guifg=DarkCyan guibg=NONE gui=NONE

if has('gui_running')
    hi link Terminal Normal
endif

hi Todo ctermfg=DarkMagenta ctermbg=NONE cterm=NONE guifg=DarkMagenta guibg=LightCyan gui=bold

hi WarningMsg ctermfg=DarkRed ctermbg=NONE cterm=NONE guifg=DarkRed guibg=NONE gui=NONE

hi diffAdded ctermfg=DarkGreen ctermbg=NONE cterm=NONE guifg=DarkGreen guibg=NONE gui=NONE
hi diffFile ctermfg=NONE ctermbg=NONE cterm=bold guifg=NONE guibg=NONE gui=bold
hi diffLine ctermfg=DarkCyan ctermbg=NONE cterm=NONE guifg=DarkCyan guibg=NONE gui=NONE
hi diffRemoved ctermfg=DarkRed ctermbg=NONE cterm=NONE guifg=DarkRed guibg=NONE gui=NONE

hi gitHash ctermfg=DarkCyan ctermbg=NONE cterm=NONE guifg=DarkCyan guibg=NONE gui=NONE
hi gitHead ctermfg=DarkBlue ctermbg=NONE cterm=NONE guifg=Blue guibg=NONE gui=bold
hi link gitIdentityKeyword gitKeyword
hi gitKeyword ctermfg=DarkMagenta ctermbg=NONE cterm=NONE guifg=Brown guibg=NONE gui=bold

hi gitcommitBranch ctermfg=DarkMagenta ctermbg=NONE cterm=NONE guifg=DarkMagenta guibg=NONE gui=NONE
hi gitcommitHeader ctermfg=DarkMagenta ctermbg=NONE cterm=NONE guifg=DarkMagenta guibg=NONE gui=NONE
hi gitcommitSummary ctermfg=DarkMagenta ctermbg=NONE cterm=NONE guifg=DarkMagenta guibg=NONE gui=bold
hi link gitcommitType gitcommitComment
hi link gitcommitFile gitcommitComment
hi gitcommitUntracked ctermfg=DarkRed ctermbg=NONE cterm=NONE guifg=DarkRed guibg=NONE gui=NONE

hi gitrebaseCommand ctermfg=DarkGreen ctermbg=NONE cterm=NONE guifg=DarkGreen guibg=NONE gui=NONE
hi gitrebaseDrop ctermfg=DarkRed ctermbg=NONE cterm=NONE guifg=DarkRed guibg=NONE gui=NONE
hi gitrebaseEdit ctermfg=DarkMagenta ctermbg=NONE cterm=NONE guifg=DarkMagenta guibg=NONE gui=NONE
hi gitrebaseExec ctermfg=DarkRed ctermbg=NONE cterm=NONE guifg=DarkRed guibg=NONE gui=NONE
hi gitrebaseFixup ctermfg=DarkMagenta ctermbg=NONE cterm=NONE guifg=DarkMagenta guibg=NONE gui=NONE
hi gitrebaseHash ctermfg=DarkCyan ctermbg=NONE cterm=NONE guifg=DarkCyan guibg=NONE gui=NONE
hi gitrebasePick ctermfg=DarkBlue ctermbg=NONE cterm=NONE guifg=DarkBlue guibg=NONE gui=NONE
hi gitrebaseReword ctermfg=DarkCyan ctermbg=NONE cterm=NONE guifg=DarkCyan guibg=NONE gui=NONE
hi gitrebaseSquash ctermfg=DarkMagenta ctermbg=NONE cterm=NONE guifg=DarkMagenta guibg=NONE gui=NONE

hi helpSpecial ctermfg=NONE ctermbg=NONE cterm=bold guifg=NONE guibg=NONE gui=bold
hi helpHyperTextJump ctermfg=DarkBlue ctermbg=NONE cterm=NONE guifg=Blue guibg=NONE gui=underline
hi link helpCommand NONE
hi link helpExample NONE

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

hi manOptionDesc ctermfg=NONE ctermbg=NONE cterm=bold guifg=NONE guibg=NONE gui=bold
hi link manCFuncDefinition manOptionDesc
hi link manLongOptionDesc manOptionDesc
hi manReference ctermfg=NONE ctermbg=NONE cterm=underline guifg=NONE guibg=NONE gui=underline
hi manTitle ctermfg=NONE ctermbg=NONE cterm=bold guifg=NONE guibg=NONE gui=bold
hi link manSectionHeading manTitle
hi link manSubHeading manTitle

hi netrwClassify ctermfg=DarkCyan ctermbg=NONE cterm=NONE guifg=DarkCyan guibg=NONE gui=NONE
hi link netrwDir netrwPlain
hi link netrwSymLink netrwPlain

hi pythonBuiltin ctermfg=NONE ctermbg=NONE cterm=underline guifg=NONE guibg=NONE gui=underline

hi qfFileName ctermfg=DarkMagenta ctermbg=NONE cterm=NONE guifg=DarkMagenta guibg=NONE gui=NONE
hi qfLineNr ctermfg=DarkRed ctermbg=NONE cterm=NONE guifg=Brown guibg=NONE gui=NONE

hi link vimCommentString vimComment
hi link vimCommentTitle vimComment
hi link vimCommentTitleLeader vimComment

hi link xmlProcessingDelim NONE
