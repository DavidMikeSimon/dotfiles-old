execute pathogen#infect()

" Set the leader to comma, more convenient in Dvorak keyboard
let mapleader=","

set nocompatible | filetype plugin on | syn on

set showcmd      " Show (partial) command in status line.
set showmatch    " Show matching brackets.
set ignorecase   " Do case insensitive matching
set smartcase    " Do smart case matching
set incsearch    " Incremental search
set autowrite    " Automatically save before commands like :next and :make
set hidden       " Hide buffers when they are abandoned
set mouse=a      " Enable mouse usage (all modes)
set ruler        " Always show current position
set autoindent   " Automatically copy indent to new lines
set nohlsearch     " Highlight search results
set shiftround   " Don't allow uneven indentation

" Comment settings:
" c - auto-wrap comments
" g - format coments with gq
au BufReadPost * set formatoptions=cq

" Spell checking and no indentation magic in markdown files
au BufReadPost *.md,*.markdown set spell
au BufReadPost *.md set wrap linebreak nolist textwidth=0 wrapmargin=0 cc=0

" FIXME Temporary hack to deal with weird issues in PHP comments
au BufReadPost *.php set comments=sO:*\ -,mO:*\ \ ,ex0:*/,s1:/*,mb:*,ex:*/1

" When doing numeric operations, don't tread 0-padded numbers as octal
set nrformats-=octal

" Convenience commands for window switching
map <C-h> <C-w>h
map <C-j> <C-w>j
map <C-k> <C-w>k
map <C-l> <C-w>l

" Tab completion settings
set completeopt=longest,menuone
set wildmode=longest,list,full
set wildmenu

" Set path to include everything below working directory
set path=,,**

" Nobody likes Ex mode
map Q <Nop>

" 80 columns
set cc=80
set showbreak=+++++

" Scroll through long lines one visual line at a time
map j gj
map k gk

" Searches wrap around the file
set wrapscan

" Shortcuts to work with the X clipboard
map <leader>xc :w !xsel -i<CR>
map <leader>xp :r!xsel<CR>

" Let backspace key in Insert mode delete anything
set backspace=indent,eol,start

" Reticule the cursor
au WinLeave * set nocursorline nocursorcolumn
au WinEnter * set cursorline cursorcolumn
set cursorline cursorcolumn

" The jinja2 plugin doesn't recognize the j2 extension by default
autocmd BufNewFile,BufRead *.j2 set ft=jinja

" Keep undo history
silent !mkdir -p $HOME/.vim/undo > /dev/null 2>&1
set undodir=$HOME/.vim/undo
set undofile

" Solarized color scheme
set background=dark
let g:solarized_termtrans=1
let g:solarized_contrast="high"
let g:solarized_visibility="high"
colorscheme solarized

" Plugin settings: Tagbar
let g:tagbar_usearrows = 1
nnoremap <leader>t :TagbarToggle<CR>

" Plugin settings: CtrlP
let g:ctrlp_user_command = {
  \ 'types': {
    \ 1: ['.git', 'cd %s && git ls-files -co --exclude-standard'],
    \ 2: ['.hg', 'hg --cwd %s locate -I .'],
    \ },
  \ 'fallback': 'find %s -type f'
  \ }

let g:ctrlp_root_markers = [
  \ 'composer.phar',
  \ 'package.json',
  \ 'components.json',
  \ 'Rakefile',
  \ 'CMakeLists.txt'
  \ ]

" Default indentation is 2 spaces
set ts=2 sts=2 sw=2 expandtab

" Keep some space above/below cursor
set scrolloff=10

" Plugin settings: DetectIndent
au BufReadPost * DetectIndent

" Plugin settings: Supertab
let g:SuperTabLongestEnhanced=1
let g:SuperTabCrMapping=0

" Plugin settings: Gundo
nnoremap <leader>u :GundoToggle<CR>

" Center screen with space bar
nmap <space> zz

" Very magic regex mode by default
" nnoremap / /\v
" cnoremap s/ s/\v

" Save file position on exit
set viminfo='10,\"100,:20,%,n~/.viminfo
function! ResCur()
  if line("'\"") <= line("$")
    normal! g`"
    normal! zz
    return 1
  endif
endfunction
augroup resCur
  autocmd!
  autocmd BufWinEnter * call ResCur()
augroup END

" Autoload when file is changed outside of vim
source ~/.vim/plugin/watchforchanges.vim
let autoreadargs={'autoread':1}
execute WatchForChanges('*',autoreadargs)
