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

" trust that the terminal supports 256 colors
set t_Co=256

" Comment settings:
" c - auto-wrap comments
" g - format coments with gq
au BufReadPost * set formatoptions=croq textwidth=100

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

" 100 columns
set cc=100

" Indicate noisily when vim has to do a soft break to show an entire line
set showbreak=+++++

" Scroll through long lines one visual line at a time
map j gj
map k gk

map U <C-u>
map D <C-d>

" Searches wrap around the file
set wrapscan

" Shortcuts to work with the X clipboard
map <leader>xc :w !xsel -i<CR>
map <leader>xp :r!xsel<CR>

" Show current line number surrounded by relative numbers
set number
set relativenumber

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
  \ 'bower.json',
  \ 'Rakefile',
  \ 'CMakeLists.txt'
  \ ]
let g:ctrlp_switch_buffer='t'
map ; :CtrlP<CR>

" Using matcher with CtrlP: https://github.com/burke/matcher#using-with-ctrlpvim
let g:path_to_matcher = "~/bin-utils/matcher"
let g:ctrlp_match_func = { 'match': 'GoodMatch' }
function! GoodMatch(items, str, limit, mmode, ispath, crfile, regex)
  " Create a cache file if not yet exists
  let cachefile = ctrlp#utils#cachedir().'/matcher.cache'
  if !( filereadable(cachefile) && a:items == readfile(cachefile) )
    call writefile(a:items, cachefile)
  endif
  if !filereadable(cachefile)
    return []
  endif
  " a:mmode is currently ignored. In the future, we should probably do
  " something about that. the matcher behaves like "full-line".
  let cmd = g:path_to_matcher.' --limit '.a:limit.' --manifest '.cachefile.' '
  if !( exists('g:ctrlp_dotfiles') && g:ctrlp_dotfiles )
    let cmd = cmd.'--no-dotfiles '
  endif
  let cmd = cmd.a:str
  return split(system(cmd), "\n")
endfunction

" Default indentation is 2 spaces
set ts=2 sts=2 sw=2 expandtab

" Keep some space above/below cursor
set scrolloff=10

" Plugin settings: DetectIndent
au BufReadPost * DetectIndent

" Plugin settings: Gundo
nnoremap <leader>u :GundoToggle<CR>
let g:gundo_preview_bottom=1

" Open new split panes to the right and bottom
set splitbelow
set splitright

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

" Open results window after grepping
autocmd QuickFixCmdPost *grep* cwindow

" Resize internal window scale when console resized
autocmd VimResized * :wincmd =

" vim-easy-align mappings
vmap <Enter> <Plug>(EasyAlign)
nmap <Leader>a <Plug>(EasyAlign)

" syntastic settings
let g:syntastic_less_use_less_lint = 1

" additional files to ignore in ctrlp, even if they aren't in gitignore
set wildignore+=*.lock,npm-shrinkwrap.json

" some filetype configs enable folding, i hate folding
set nofoldenable

" airline
set laststatus=2
let g:airline_theme="base16"
if !exists('g:airline_symbols')
  let g:airline_symbols = {}
endif
" unicode symbols
let g:airline_left_sep = '▶'
let g:airline_right_sep = '◀'
let g:airline_symbols.linenr = '¶'
let g:airline_symbols.branch = '⎇'
let g:airline_symbols.paste = 'ρ'
let g:airline_symbols.whitespace = 'Ξ'
let g:airline#extensions#default#layout = [ ['a','b','c'], ['x','z','warning' ] ]
let g:airline#extensions#branch#displayed_head_limit = 12

" git gutter
let g:gitgutter_override_sign_column_highlight=0
highlight SignColumn ctermbg=black

" neocomplete
let g:neocomplete#enable_at_startup = 1
let g:neocomplete#enable_smart_case = 1
let g:neocomplete#enable_auto_select = 0
inoremap <expr><TAB>  pumvisible() ? "\<C-n>" : "\<TAB>"
inoremap <expr><S-TAB>  pumvisible() ? "\<C-p>" : "\<TAB>"
" <CR>: close popup and save indent.
inoremap <silent> <CR> <C-r>=<SID>my_cr_function()<CR>
function! s:my_cr_function()
  return neocomplete#close_popup() . "\<CR>"
endfunction
call neocomplete#custom#source('buffer', 'rank', 5000)

" vim-g (google search)
let g:vim_g_query_url="https://www.google.com/search?btnI&q="
let g:vim_g_command = "Go"
let g:vim_g_f_command = "Gf"
nmap <Enter> :Gf<CR>
