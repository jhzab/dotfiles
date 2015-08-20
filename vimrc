set nocompatible               " be iMproved
filetype off                   " required!

set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

" let Vundle manage Vundle
" required! 
Plugin 'VundleVim/Vundle.vim'

" My Plugins here:
"
" original repos on github
Plugin 'tpope/vim-fugitive'
Plugin 'tpope/vim-abolish'
Plugin 'rstacruz/sparkup', {'rtp': 'vim/'}
Plugin 'vim-ruby/vim-ruby'
Plugin 'tpope/vim-rails.git'
Plugin 'mileszs/ack.vim'
Plugin 'tpope/vim-surround'
Plugin 'scrooloose/nerdcommenter'
Plugin 'scrooloose/nerdtree'
" vim-scripts repos
Plugin 'L9'
Plugin 'FuzzyFinder'
Plugin 'scrooloose/syntastic'
Plugin 'klen/python-mode'
Plugin 'kien/ctrlp.vim'
Plugin 'w0ng/vim-hybrid'
"Plugin 'Lokaltog/powerline'
Plugin 'bling/vim-airline'
Plugin 'Lokaltog/vim-easymotion'
Plugin 'goldfeld/vim-seek'
" like the one above, but with two chars
Plugin 'justinmk/vim-sneak'
" following is for snippets only
Plugin 'MarcWeber/vim-addon-mw-utils'
Plugin 'tomtom/tlib_vim'
Plugin 'garbas/vim-snipmate'
Plugin 'honza/vim-snippets'
" awesome completion with syntastic integration
Plugin 'Valloric/YouCompleteMe'
Plugin 'tpope/vim-haml'
Plugin 'othree/html5.vim'
"Plugin 'Blackrush/vim-gocode'
Plugin 'fatih/vim-go'
Plugin 'derekwyatt/vim-scala'
Plugin 'zhaocai/GoldenView.Vim'
Plugin 'Shougo/unite.vim'

call vundle#end()

filetype plugin indent on     " required!

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
colorscheme default

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
