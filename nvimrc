call plug#begin('~/.config/nvim/bundle')


" original repos on github
Plug 'tpope/vim-fugitive'
"Plug 'rstacruz/sparkup', {'rtp': 'vim/'}
Plug 'scrooloose/nerdcommenter'
Plug 'scrooloose/nerdtree'
Plug 'scrooloose/syntastic'
Plug 'klen/python-mode'
"Plug 'davidhalter/jedi-vim'
Plug 'zchee/deoplete-jedi'
Plug 'ctrlpvim/ctrlp.vim'
Plug 'w0ng/vim-hybrid'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'easymotion/vim-easymotion'
"Plug 'goldfeld/vim-seek'
" like the one above, but with two chars
Plug 'justinmk/vim-sneak'
" following is for snippets only
" awesome completion with syntastic integration
"Plug 'zhaocai/GoldenView.Vim'
Plug 'Shougo/denite.nvim'
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
Plug 'eagletmt/neco-ghc'
Plug 'eagletmt/ghcmod-vim'
Plug 'neovimhaskell/haskell-vim'
Plug 'airblade/vim-gitgutter'
Plug 'reedes/vim-pencil'
Plug 'junegunn/limelight.vim'
Plug 'reedes/vim-wordy'
Plug 'reedes/vim-lexical'
Plug 'reedes/vim-colors-pencil'
Plug 'lervag/vimtex'
Plug 'rust-lang/rust.vim'
Plug 'racer-rust/vim-racer'

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
set guicursor=
set number
set cursorline

set background=dark
colorscheme hybrid

" open nerdtree
noremap <Leader>n :NERDTreeToggle<CR>
" auto close vim if nerdtree is the last open window
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTreeType") && b:NERDTreeType == "primary") | q | endif

let g:deoplete#enable_at_startup = 1

" syntastic
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

" seek
let g:seek_enable_jumps = 1

" Disable haskell-vim omnifunc, this is done by necoghc
let g:haskellmode_completion_ghc = 0
autocmd FileType haskell setlocal omnifunc=necoghc#omnifunc

" autocmd FileType python setlocal foldmethod=indent
"autocmd FileType python setlocal foldmethod=indent
autocmd FileType python setlocal tabstop=4
autocmd FileType python setlocal softtabstop=4
autocmd FileType python setlocal shiftwidth=4
autocmd FileType python setlocal textwidth=79
autocmd FileType python setlocal expandtab
autocmd FileType python setlocal autoindent
autocmd FileType python setlocal fileformat=unix

let g:pymode_python = 'python3'
let g:pymode_rope = 0

let g:airline#extensions#tabline#enabled = 1

" CtrlP
let g:ctrlp_custom_ignore = {
  \ 'dir':  '\v[\/](\.(git|hg|svn)|\_site)$',
  \ 'file': '\v\.(exe|so|dll|class|png|jpg|jpeg)$',
\}

let g:ctrlp_working_path_mode = 'r'

nmap <leader>p :CtrlP<cr>
nmap <leader>pb :CtrlPBuffer<cr>

augroup writing
  autocmd!
  autocmd FileType markdown,mkd call pencil#init()
                            \ | call lexical#init()
  autocmd FileType text call pencil#init()
                            \ | call lexical#init()
augroup END

let g:jedi#force_py_version = 3
let g:deoplete#sources#jedi#python_path = "/usr/bin/python3"
