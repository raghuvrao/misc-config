set nocompatible
set laststatus=1
set mouse=
set nobackup
if has("persistent_undo") | set noundofile | endif
set nomodeline modelines=0
set backspace=indent,eol,start
set lazyredraw
set softtabstop=8  " Matches default values of tabstop and shiftwidth.
set ignorecase smartcase
set nowrap sidescroll=1
if has("wildmenu") | set nowildmenu wildmode=longest,list,full | endif
set autoindent
if has("smartindent") | set nosmartindent | endif
if has("cmdline_info") | set ruler showcmd | endif
if has("folding") | set foldmethod=indent foldlevel=100 | endif
if has("extra_search")
  set incsearch
  nnoremap <Leader>\ :nohlsearch<CR>
endif
set list
if has("multi_byte")
  set encoding=utf-8
  set listchars=tab:¬\ ,trail:¶,extends:>,precedes:<,nbsp:·
  if has("windows") && has("folding")
    " Has the side-effect of setting vert and stlnc to blank when highlighting
    " is available, which I find acceptable.
    set fillchars=stl:^,fold:-,diff:-
  endif
else
  set listchars=tab:\|\ ,trail:#,extends:>,precedes:<,nbsp:_
endif
set textwidth=0

if has("autocmd")
  filetype plugin indent on
  augroup raghu_augroup
    au!
    au BufWinEnter,BufNewFile * setlocal formatoptions=qnm1
    au FileType python setlocal expandtab softtabstop=4 shiftwidth=4
    au FileType help wincmd L
    au FileType json,sh,vim setlocal expandtab softtabstop=2 shiftwidth=2
    au BufWinEnter,BufNewFile *.json,*.bash,*.sh,*.vim
          \ setlocal expandtab softtabstop=2 shiftwidth=2
  augroup END
endif

" When running vim in tmux, allow C-<arrow> to work, useful in the cmdline and
" in insert mode.
if &term =~ '^screen'
  " tmux sends xterm-style key sequences when its xterm-keys option is on.
  " Need `execute' because we want \e to be parsed into literal Esc before
  " being assigned.
  execute "set <xUp>=\e[1;*A"
  execute "set <xDown>=\e[1;*B"
  execute "set <xRight>=\e[1;*C"
  execute "set <xLeft>=\e[1;*D"
endif

nnoremap <Leader>e :silent edit<CR>

if has("syntax") | syntax off | endif

" Make vim more secure.  See |trojan-horse|.
set noexrc secure
