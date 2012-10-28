set nocompatible
filetype plugin indent off

"if has('vim_starting')
"	set runtimepath+=~/.vim/bundle/neobundle.vim/
"	call neobundle#rc(expand('~/.vim/bundle/'))
"endif

"NeoBundle 'Lokaltog/vim-powerline'

filetype plugin indent on

se nocompatible
se laststatus=2
se encoding=utf-8
se ruler
se showcmd
se showmatch
se nowrap
se autoindent
se noswapfile
se nobackup
se ignorecase
se smartcase
se nowrapscan
se hlsearch
se hidden
se foldmethod=indent
se foldlevel=2
se helplang=ja
se clipboard=unnamed
se fileformats=unix,dos,mac
se lazyredraw
se ttyfast
se t_ti=
se t_te=
se helplang=ja,en
se visualbell
se vb t_vb=
se t_Co=256
se ts=4 sts=4 sw=4 tw=0 noet
se modeline

autocmd QuickfixCmdPost make,grep,grepadd,vimgrep if len(getqflist()) != 0 | copen | endif

augr MyBuf
	au!
	au BufEnter * execute ":lcd " . expand("%:p:h")
	au BufNewFile * silent! 0r $HOME/.vim/template/%:e
	au BufReadPost * if line("'\"") > 0 && line("'\"") <= line("$") | exe "normal g`\"" | endif
augr END

let mapleader = " "
let g:netrw_banner=0
noremap ; :
nmap <esc><esc> :nohlsearch<CR><esc>
nnoremap <tab> %
vnoremap <tab> %
nnoremap <leader>m :<c-u>marks
"nnoremap <leader>r :<c-u>registers
nnoremap <leader>b :ls<cr>:buf 
nnoremap <leader>a :ls<cr>:buf 
nnoremap <leader>e :!perl -MData::Dumper %<cr>
nnoremap <leader>c :!perl -c %<cr>
nnoremap <c-t>c :tabnew<cr>
nnoremap <c-t>x :tabclose<cr>
nnoremap <c-t>n :tabnext<cr>
nnoremap <c-t>p :tabprev<cr>
cnoremap <c-a> <home>
cnoremap <c-e> <end>
cnoremap <c-f> <right>
cnoremap <c-b> <left>
inoremap <c-r>t <c-r>=strftime('%Y-%m-%dT%H:%M:%S')<cr>
inoremap <c-r>T <c-r>=strftime('%Y-%m-%d')<cr>

if has("autocmd")
	au filetype perl compiler perl
endif

syntax on
filetype on

se fillchars=vert:\|
hi Folded gui=bold term=standout ctermbg=NONE ctermfg=2 guifg=Grey80
hi FoldColumn gui=bold term=standout ctermbg=NONE ctermfg=2 guifg=DarkBlue

let g:Powerline_symbolls='fancy'

let g:NERDCreateDefaultMappings = 0
let NERDSpaceDelims = 1
nmap <Leader>/ <Plug>NERDCommenterToggle
vmap <Leader>/ <Plug>NERDCommenterToggle

if &encoding !=# 'utf-8'
  set encoding=japan
  set fileencoding=japan
endif
if has('iconv')
  let s:enc_euc = 'euc-jp'
  let s:enc_jis = 'iso-2022-jp'
  if iconv("\x87\x64\x87\x6a", 'cp932', 'eucjp-ms') ==# "\xad\xc5\xad\xcb"
    let s:enc_euc = 'eucjp-ms'
    let s:enc_jis = 'iso-2022-jp-3'
  elseif iconv("\x87\x64\x87\x6a", 'cp932', 'euc-jisx0213') ==# "\xad\xc5\xad\xcb"
    let s:enc_euc = 'euc-jisx0213'
    let s:enc_jis = 'iso-2022-jp-3'
  endif
  if &encoding ==# 'utf-8'
    let s:fileencodings_default = &fileencodings
    if has('mac')
      let &fileencodings = s:enc_jis .','. s:enc_euc
      let &fileencodings = &fileencodings .','. s:fileencodings_default
    else
      let &fileencodings = s:enc_jis .','. s:enc_euc .',cp932'
      let &fileencodings = &fileencodings .','. s:fileencodings_default
    endif
    unlet s:fileencodings_default
  else
    let &fileencodings = &fileencodings .','. s:enc_jis
    set fileencodings+=utf-8,ucs-2le,ucs-2
    if &encoding =~# '^\(euc-jp\|euc-jisx0213\|eucjp-ms\)$'
      set fileencodings+=cp932
      set fileencodings-=euc-jp
      set fileencodings-=euc-jisx0213
      set fileencodings-=eucjp-ms
      let &encoding = s:enc_euc
      let &fileencoding = s:enc_euc
    else
      let &fileencodings = &fileencodings .','. s:enc_euc
    endif
  endif
  unlet s:enc_euc
  unlet s:enc_jis
endif

"vim:se ts=2 sts=2 sw=2 tw=0 noet
