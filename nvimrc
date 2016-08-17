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
Plug 'L9'
Plug 'FuzzyFinder'
Plug 'scrooloose/syntastic'
Plug 'klen/python-mode'
Plug 'ctrlpvim/ctrlp.vim'
Plug 'w0ng/vim-hybrid'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
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
Plug 'Valloric/YouCompleteMe'
Plug 'tpope/vim-haml'
Plug 'othree/html5.vim'
Plug 'derekwyatt/vim-scala'
Plug 'zhaocai/GoldenView.Vim'
Plug 'Shougo/unite.vim'
Plug 'ensime/ensime-vim'
Plug 'Shougo/deoplete.nvim'
Plug 'eagletmt/neco-ghc'
Plug 'eagletmt/ghcmod-vim'
Plug 'neovimhaskell/haskell-vim'
Plug 'altercation/vim-colors-solarized'
Plug 'airblade/vim-gitgutter'
Plug 'crusoexia/vim-monokai'
Plug 'aloiscochard/sarsi'
Plug 'Shougo/vimproc.vim'
Plug 'reedes/vim-pencil'
Plug 'junegunn/limelight.vim'
Plug 'reedes/vim-wordy'
Plug 'reedes/vim-lexical'
Plug 'reedes/vim-colors-pencil'

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

set background=light
colorscheme pencil

" open nerdtree
noremap <Leader>n :NERDTreeToggle<CR>
" auto close vim if nerdtree is the last open window
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTreeType") && b:NERDTreeType == "primary") | q | endif

let g:deoplete#enable_at_startup = 1

" syntastic
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_enable_signs=1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0

let g:ycm_autoclose_preview_window_after_completion=1

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

let g:airline#extensions#tabline#enabled = 1

" CtrlP
let g:ctrlp_custom_ignore = {
  \ 'dir':  '\v[\/](\.(git|hg|svn)|\_site)$',
  \ 'file': '\v\.(exe|so|dll|class|png|jpg|jpeg)$',
\}

let g:ctrlp_working_path_mode = 'r'

nmap <leader>p :CtrlP<cr>
nmap <leader>pb :CtrlPBuffer<cr>
call rpcstart('sarsi-nvim') 

augroup writing
  autocmd!
  autocmd FileType markdown,mkd call pencil#init()
                            \ | call lexical#init()
  autocmd FileType text call pencil#init()
                            \ | call lexical#init()
augroup END
