syntax on
filetype plugin indent on

set nocompatible
set laststatus=2
set encoding=utf-8
set fileencodings=iso-2022-jp,utf-8,cp932,euc-jp,default,latin
set fileformats=unix,dos,mac
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
set clipboard=unnamed
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
set runtimepath+=$MYVIM

com! Kwbd let kwbd_bn= bufnr("%")|enew|exe "bdel ".kwbd_bn|unlet kwbd_bn

let mapleader = " "
let g:netrw_banner=0
noremap ; :
nmap <esc><esc> :nohlsearch<CR><esc>
nnoremap <tab> %
vnoremap <tab> %
nnoremap <leader>a :!pushfile $(realpath %) $MYHOME/var/vim/memo.md<cr>
nnoremap <leader>l :e $MYHOME/var/vim/memo.md<cr>
nnoremap <leader>m :<c-u>marks<cr>
nnoremap <leader>r :<c-u>registers<cr>
nnoremap <leader>b :ls<cr>:buf 
nnoremap <leader>x :Kwbd<cr>
nnoremap <leader>eu :e ++enc=utf-8<cr>
nnoremap <leader>es :e ++enc=cp932<cr>
nnoremap <leader>ej :e ++enc=iso-2022-jp<cr>
nnoremap <leader>ee :e ++enc=euc-jp<cr>
nnoremap <c-t>c :tabnew<cr>
nnoremap <c-t>x :tabclose<cr>
nnoremap <c-t>n :tabnext<cr>
nnoremap <c-t>p :tabprev<cr>
cnoremap <c-a> <home>
cnoremap <c-e> <end>
cnoremap <c-f> <right>
cnoremap <c-b> <left>
inoremap jj <esc>
inoremap <c-r>t <c-r>=strftime('%Y/%m/%d %H:%M:%S')<cr>
inoremap <c-r>T <c-r>=strftime('%Y/%m/%d')<cr>

se fillchars=vert:\|
hi Folded gui=bold term=standout ctermbg=NONE ctermfg=2 guifg=Grey80
hi FoldColumn gui=bold term=standout ctermbg=NONE ctermfg=2 guifg=DarkBlue

if has("wildmenu")
	set wildmenu
	set wildmode=list:longest
	set wildignore+=*.a,*.o
	set wildignore+=*.bmp,*.gif,*.ico,*.jpg,*.png
	set wildignore+=*~,*.swp,*.tmp
	set wildignore+=*/.git/*,*/.hg/*,*/.svn/*
endif

if has("autocmd")
	au QuickfixCmdPost make,grep,grepadd,vimgrep if len(getqflist()) != 0 | copen | endif
	"au BufEnter * execute ":lcd " . expand("%:p:h")
	au BufNewFile * silent! 0r $HOME/.vim/template/%:e
	au BufReadPost * if line("'\"") > 0 && line("'\"") <= line("$") | exe "normal g`\"" | endif
	
	"perl
	au BufNewFile,BufRead *.pl,*.cgi,*.pm,*.psgi call _perl()
	fu _perl()
		set filetype=perl
		nn <buffer> <leader>c :!perl -c %<cr>
		nn <buffer> <leader>r :!perl -MData::Dumper -w  %<left><left>
		au filetype perl compiler perl
		setl ts=4 sts=4 sw=4 tw=0 noet
	endf

	"shell
	au BufNewFile,BufRead *.sh call _sh()
	fu _sh()
		set filetype=sh
		nn <buffer> <leader>c :!sh -n %<cr>
		nn <buffer> <leader>r :!sh %<cr>
		setl ts=4 sts=4 sw=4 tw=0 noet
	endf
	
	"markdown
	au BufNewFile,BufRead *.md call _markdown()
	fu _markdown()
		setl filetype=markdown
		setl ts=4 sts=4 sw=4 tw=0 noet
		setl fileencoding=utf-8
	endf

	"forth
	au BufNewFile,BufRead *.fh call _forth()
	fu _forth()
		setl filetype=forth
		nn <buffer> <leader>r :!gforth %<cr>
		setl ts=2 sts=2 sw=2 tw=0 noet
	endf
	
	"ocaml
	au BufNewFile,BufRead *.ml,*.mli call _ocaml()
	fu _ocaml()
		nn <buffer> <leader>r :!ocaml %<cr>
		setl ts=2 sts=2 sw=2 tw=0 noet
	endf
endif
