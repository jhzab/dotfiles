call plug#begin('~/.config/nvim/bundle')

" original repos on github
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-abolish'
Plug 'rstacruz/sparkup', {'rtp': 'vim/'}
Plug 'vim-ruby/vim-ruby'
Plug 'mileszs/ack.vim'
Plug 'tpope/vim-surround'
Plug 'scrooloose/nerdcommenter'
Plug 'scrooloose/nerdtree'
" vim-scripts repos
Plug 'L9'
Plug 'FuzzyFinder'
Plug 'scrooloose/syntastic'
Plug 'klen/python-mode'
Plug 'ctrlpvim/ctrlp.vim'
Plug 'w0ng/vim-hybrid'
"Plugin 'Lokaltog/powerline'
Plug 'bling/vim-airline'
Plug 'easymotion/vim-easymotion'
"Plug 'goldfeld/vim-seek'
" like the one above, but with two chars
Plug 'justinmk/vim-sneak'
" following is for snippets only
Plug 'MarcWeber/vim-addon-mw-utils'
Plug 'tomtom/tlib_vim'
Plug 'garbas/vim-snipmate'
Plug 'honza/vim-snippets'
" awesome completion with syntastic integration
"Plug 'Valloric/YouCompleteMe'
Plug 'tpope/vim-haml'
Plug 'othree/html5.vim'
Plug 'derekwyatt/vim-scala'
Plug 'zhaocai/GoldenView.Vim'
Plug 'Shougo/unite.vim'
Plug 'ensime/ensime-vim'
Plug 'Shougo/deoplete.nvim'
Plug 'eagletmt/neco-ghc'

call plug#end()

filetype plugin indent on     " required!

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

" Disable haskell-vim omnifunc, this is done by necoghc
let g:haskellmode_completion_ghc = 0
autocmd FileType haskell setlocal omnifunc=necoghc#omnifunc
