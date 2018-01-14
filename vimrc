set nobackup
if has("persistent_undo")
	set noundofile
endif
set nomodeline modelines=0
set backspace=indent,eol,start
let &softtabstop=&tabstop
set ignorecase smartcase
set nowrap sidescroll=1
nnoremap <Leader>w :set invwrap<CR>
if has("wildmenu")
	set nowildmenu wildmode=longest,list,full
endif
if has("cmdline_info")
	set ruler showcmd
endif
if has("folding")
	set foldmethod=indent foldlevel=100
	if has("windows")
		set fillchars=vert:\|,stl:-,stlnc:\ ,fold:-,diff:-
	endif
endif
if has("extra_search")
	nnoremap <Leader><Leader> :nohlsearch<CR>
	if has("reltime")
		set incsearch
	endif
endif
set list
if has("multi_byte")
	set encoding=utf-8
	set listchars=tab:→·,trail:¬,extends:>,precedes:<,nbsp:%
else
	set listchars=tab:>-,trail:#,extends:>,precedes:<,nbsp:%
endif
set laststatus=2
if has("statusline")
	set statusline=%(\ %f%M%)%(\ %h%r%w%)%(\ %l,%c%V\ %)
endif
if exists("+autochdir")
	set autochdir
endif
set formatoptions=1jlmnq  " ftplugins should add to / remove from this default.
if has("autocmd")
	filetype plugin indent on
	runtime ftplugin/man.vim
	augroup raghu_augroup
		autocmd!
		autocmd BufNewFile,BufWinEnter * setlocal formatoptions-=c formatoptions-=o formatoptions-=r formatoptions-=t
		autocmd FileType help setlocal nolist keywordprg=:help
		autocmd FileType json setlocal expandtab softtabstop=2 shiftwidth=2
		autocmd FileType man setlocal nolist
		autocmd FileType text setlocal linebreak wrap
		autocmd FileType vim setlocal keywordprg=:help
	augroup END
endif
" When running vim in tmux, allow C-<arrow> to work as expected.  These
" mappings are useful in the cmdline and in insert mode.  tmux sends
" xterm-style key sequences when its xterm-keys option is on.
if &term =~ '^screen' && exists('$TMUX')
	" Need `execute' because we want \e to be parsed into literal Esc
	" before being assigned.
	execute "set <xUp>=\e[1;*A"
	execute "set <xDown>=\e[1;*B"
	execute "set <xRight>=\e[1;*C"
	execute "set <xLeft>=\e[1;*D"
endif
nnoremap <Leader>e :silent edit<CR>
nnoremap <Leader>tu i<C-R>=strftime('%s')<CR><Esc>
nnoremap <Leader>ts i<C-R>=strftime('%Y-%m-%d %H:%M:%S %Z(UTC%z)')<CR><Esc>
nnoremap <Leader>m :set invmodifiable<CR>
nnoremap <Leader>I :edit $MYVIMRC<CR>
if has("syntax")
	function! ToggleSyntaxHighlight()
		if exists("g:syntax_on")
			syntax off
		else
			syntax enable
		endif
	endfunction
	nnoremap <Leader>P :call ToggleSyntaxHighlight()<CR>
	if has("autocmd")
		augroup raghu_colors
			autocmd!
			autocmd ColorScheme default highlight SpecialKey ctermfg=DarkMagenta
		augroup END
		colorscheme default  " Just to trigger the above autocmd.
	endif
	set background=light
	syntax off
endif
" Make vim more secure.  See `:h trojan-horse'.
set noexrc secure
