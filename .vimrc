"vim:se ts=2 sts=2 sw=2 tw=0 noet

syntax on
filetype plugin indent on

set nocompatible
set laststatus=2
set encoding=utf-8
set ruler
set showcmd
set showmatch
set nowrap
set autoindent
set noswapfile
set nobackup
set ignorecase
set smartcase
set nowrapscan
set hlsearch
set hidden
set foldmethod=indent
set foldlevel=2
set helplang=ja
set clipboard=unnamed
set fileformats=unix,dos,mac
set lazyredraw
set ttyfast
set t_ti=
set t_te=
set helplang=ja,en
set visualbell
set vb t_vb=
set t_Co=256
set ts=4 sts=4 sw=4 tw=0 noet
set modeline
set statusline=%<[%n]%m%r%h%w%{'['.(&fenc!=''?&fenc:&enc).':'.&ff.']'}%y\ %F%=%l/%L,%c%V

let mapleader = " "
let g:netrw_banner=0
noremap ; :
nmap <esc><esc> :nohlsearch<CR><esc>
nnoremap <tab> %
vnoremap <tab> %
nnoremap <leader>m :<c-u>marks
nnoremap <leader>r :<c-u>registers
nnoremap <leader>b :ls<cr>:buf 
nnoremap <leader>a :ls<cr>:buf 
nnoremap <leader>c :!perl -c %<cr>
nnoremap <leader>eu :e ++enc=utf-8<cr>
nnoremap <leader>es :e ++enc=shift_jis<cr>
nnoremap <leader>ee :e ++enc=euc-jp<cr>
nnoremap <c-t>c :tabnew<cr>
nnoremap <c-t>x :tabclose<cr>
nnoremap <c-t>n :tabnext<cr>
nnoremap <c-t>p :tabprev<cr>
cnoremap <c-a> <home>
cnoremap <c-e> <end>
cnoremap <c-f> <right>
cnoremap <c-b> <left>
inoremap <c-r>t <c-r>=strftime('%Y/%m/%d %H:%M:%S')<cr>
inoremap <c-r>T <c-r>=strftime('%Y/%m/%d')<cr>

if has("autocmd")
	au filetype perl compiler perl
	au QuickfixCmdPost make,grep,grepadd,vimgrep if len(getqflist()) != 0 | copen | endif
	au BufEnter * execute ":lcd " . expand("%:p:h")
	au BufNewFile * silent! 0r $HOME/.vim/template/%:e
	au BufReadPost * if line("'\"") > 0 && line("'\"") <= line("$") | exe "normal g`\"" | endif
endif

se fillchars=vert:\|
hi Folded gui=bold term=standout ctermbg=NONE ctermfg=2 guifg=Grey80
hi FoldColumn gui=bold term=standout ctermbg=NONE ctermfg=2 guifg=DarkBlue

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
