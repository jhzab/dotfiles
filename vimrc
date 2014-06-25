set nocompatible               " be iMproved
filetype off                   " required!

set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

" let Vundle manage Vundle
" required! 
Bundle 'gmarik/vundle'

" My Bundles here:
"
" original repos on github
Bundle 'tpope/vim-fugitive'
Bundle 'tpope/vim-abolish'
Bundle 'rstacruz/sparkup', {'rtp': 'vim/'}
Bundle 'vim-ruby/vim-ruby'
Bundle 'tpope/vim-rails.git'
Bundle 'mileszs/ack.vim'
Bundle 'tpope/vim-surround'
Bundle 'scrooloose/nerdcommenter'
Bundle 'scrooloose/nerdtree'
" vim-scripts repos
Bundle 'L9'
Bundle 'FuzzyFinder'
Bundle 'scrooloose/syntastic'
Bundle 'klen/python-mode'
Bundle 'kien/ctrlp.vim'
Bundle 'w0ng/vim-hybrid'
Bundle 'Lokaltog/powerline'
"Bundle 'bling/vim-airline'
Bundle 'Lokaltog/vim-easymotion'
Bundle 'goldfeld/vim-seek'
" like the one above, but with two chars
Bundle 'justinmk/vim-sneak'
" following is for snippets only
Bundle 'MarcWeber/vim-addon-mw-utils'
Bundle 'tomtom/tlib_vim'
Bundle 'garbas/vim-snipmate'
Bundle 'honza/vim-snippets'
" awesome completion with syntastic integration
Bundle 'Valloric/YouCompleteMe'
Bundle 'tpope/vim-haml'
Bundle 'othree/html5.vim'
"Bundle 'Blackrush/vim-gocode'
Bundle 'fatih/vim-go'
Bundle 'derekwyatt/vim-scala'
Bundle 'zhaocai/GoldenView.Vim'
Bundle 'Shougo/unite.vim'


let g:vundle_default_git_proto = 'git'

filetype plugin indent on     " required!
"
" Brief help
" :BundleList          - list configured bundles
" :BundleInstall(!)    - install(update) bundles
" :BundleSearch(!) foo - search(or refresh cache first) for foo
" :BundleClean(!)      - confirm(or auto-approve) removal of unused bundles
"
" see :h vundle for more details or wiki for FAQ
" NOTE: comments after Bundle command are not allowed..

filetype on
syntax on
set encoding=utf-8
set laststatus=2 
set gdefault
set bs=2
set scrolloff=2
set sidescrolloff=5
set hlsearch
set ignorecase
set gdefault
set ruler
set wildmenu
set wildmode=longest:full,full
set wildignore+=tags
set winaltkeys=no

let g:hybrid_use_Xresources = 1
colorscheme hybrid

set rtp+=/home/gothos/.vim/bundle/powerline/powerline/bindings/vim
let g:Powerline_symbols = 'fancy'

" open nerdtree
noremap <Leader>n :NERDTreeToggle<CR>
" auto close vim if nerdtree is the last open window
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTreeType") && b:NERDTreeType == "primary") | q | endif

" syntastic
"let g:syntastic_auto_loc_list=1

let g:syntastic_enable_signs=1

let g:EclimCompletionMethod = 'omnifunc'

" C++ code formatting options
autocmd FileType cpp set textwidth=80
autocmd FileType cpp set formatoptions+=cro
autocmd FileType cpp set tabstop=4
autocmd FileType cpp set softtabstop=4
autocmd FileType cpp set shiftwidth=4

let g:ycm_autoclose_preview_window_after_completion=1

set guifont="Ubuntu Mono derivative Powerline 10"

" start NERDtree if no files/arguments were specified
autocmd vimenter * if !argc() | NERDTree | endif
" close vim if NERDtree is last open window
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTreeType") && b:NERDTreeType == "primary") | q | endif

let g:pymode_rope_complete_on_dot = 0

" seek
let g:seek_enable_jumps = 1
