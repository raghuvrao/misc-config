set nobackup

set noundofile

set nomodeline
set modelines=0

set backspace=indent,eol,start

let &softtabstop=&tabstop

set ignorecase
set smartcase

set sidescroll=1

set nowrap
nnoremap <Leader>w :set invwrap<CR>

set nowildmenu
set wildmode=longest,list,full

set ruler

set showcmd

set foldmethod=indent
set foldlevel=100

set fillchars=vert:\|,stl:-,stlnc:\ ,fold:-,diff:-

set incsearch

nnoremap <Leader><Leader> :nohlsearch<CR>

set list
if has("multi_byte")
  set encoding=utf-8
  set listchars=tab:→·,trail:¬,extends:>,precedes:<,nbsp:%
else
  set listchars=tab:>-,trail:#,extends:>,precedes:<,nbsp:%
endif

set laststatus=2
set statusline=%(\ %f%M%)%(\ %h%r%w%)%(\ %l,%c%V\ %)

set autochdir

set formatoptions=1jlmnq  " ftplugins should add to / remove from this default.

filetype plugin indent on

runtime ftplugin/man.vim

augroup raghu_augroup
  autocmd!
  autocmd BufNewFile,BufWinEnter * setlocal formatoptions-=c formatoptions-=o formatoptions-=r formatoptions-=t
  autocmd FileType help setlocal nolist keywordprg=:help
  autocmd FileType json setlocal expandtab shiftwidth=2 softtabstop=-1
  autocmd FileType man setlocal nolist
  autocmd FileType text setlocal linebreak wrap
  autocmd FileType vim setlocal keywordprg=:help shiftwidth=2 softtabstop=-1
augroup END

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

function! ToggleSyntaxHighlight()
  if exists("g:syntax_on")
    syntax off
  else
    syntax enable
  endif
endfunction
nnoremap <Leader>P :call ToggleSyntaxHighlight()<CR>

augroup raghu_colors
  autocmd!
  autocmd ColorScheme default highlight SpecialKey ctermfg=DarkMagenta
augroup END
colorscheme default  " Just to trigger the above autocmd.

set background=light
syntax off

" Make vim more secure.  See `:h trojan-horse'.
set secure
set noexrc
