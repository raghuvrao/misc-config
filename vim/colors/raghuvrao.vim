" Plain colorscheme for vim.
" Maintainer:	Raghu V. Rao

" This colorscheme's goal is to disable most things from being colorized.

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
hi clear Type

if &background == "light"
  hi Comment
	\ term=bold
	\ cterm=NONE ctermfg=Brown ctermbg=NONE
	\ gui=NONE guifg=Brown guibg=NONE
else
  hi Comment
	\ term=bold
	\ cterm=NONE ctermfg=DarkGreen ctermbg=NONE
	\ gui=NONE guifg=DarkGreen guibg=NONE
endif
if &background == "light"
  hi SpecialKey
	\ term=bold
	\ cterm=NONE ctermfg=DarkCyan ctermbg=NONE
	\ gui=NONE guifg=DarkCyan guibg=NONE
else
  hi SpecialKey
	\ term=bold
	\ cterm=NONE ctermfg=DarkMagenta ctermbg=NONE
	\ gui=NONE guifg=DarkMagenta guibg=NONE
endif

if &background == "light"
  hi diffAdded
	\ term=underline
	\ cterm=NONE ctermfg=DarkGreen ctermbg=NONE
	\ gui=NONE guifg=DarkGreen guibg=NONE
else
  hi diffAdded
	\ term=underline
	\ cterm=NONE ctermfg=Green ctermbg=NONE
	\ gui=NONE guifg=Green guibg=NONE
endif
if &background == "light"
  hi diffFile
	\ term=bold
	\ cterm=NONE ctermfg=DarkBlue ctermbg=NONE
	\ gui=bold guifg=DarkBlue guibg=NONE
else
  hi diffFile
	\ term=bold
	\ cterm=NONE ctermfg=LightBlue ctermbg=NONE
	\ gui=bold guifg=LightBlue guibg=NONE
endif
if &background == "light"
  hi diffLine
	\ term=bold
	\ cterm=NONE ctermfg=Brown ctermbg=NONE
	\ gui=NONE guifg=Brown guibg=NONE
else
  hi diffLine
	\ term=bold
	\ cterm=NONE ctermfg=Yellow ctermbg=NONE
	\ gui=NONE guifg=Yellow guibg=NONE
endif
if &background == "light"
  hi diffRemoved
	\ term=bold
	\ cterm=NONE ctermfg=DarkRed ctermbg=NONE
	\ gui=NONE guifg=DarkRed guibg=NONE
else
  hi diffRemoved
	\ term=bold
	\ cterm=NONE ctermfg=LightRed ctermbg=NONE
	\ gui=NONE guifg=LightRed guibg=NONE
endif

if &background == "light"
  hi gitcommitBranch
	\ term=bold,underline
	\ cterm=NONE ctermfg=DarkBlue ctermbg=NONE
	\ gui=NONE guifg=DarkBlue guibg=NONE
else
  hi gitcommitBranch
	\ term=bold,underline
	\ cterm=NONE ctermfg=LightBlue ctermbg=NONE
	\ gui=NONE guifg=LightBlue guibg=NONE
endif

if &background == "light"
  hi pythonString
	\ term=underline
	\ cterm=NONE ctermfg=DarkRed ctermbg=NONE
	\ gui=NONE guifg=DarkRed guibg=NONE
else
  hi pythonString
	\ term=underline
	\ cterm=NONE ctermfg=DarkCyan ctermbg=NONE
	\ gui=NONE guifg=DarkCyan guibg=NONE
endif
hi link pythonEscape pythonString
hi link pythonQuotes pythonString
hi link pythonTripleQuotes pythonString

hi link vimCommentTitle vimComment
hi link vimCommentString vimComment
