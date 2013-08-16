" put this line first in ~/.vimrc
set nocompatible | filetype plugin on | syn on

fun! EnsureVamIsOnDisk(plugin_root_dir)
  " windows users may want to use http://mawercer.de/~marc/vam/index.php
  " to fetch VAM, VAM-known-repositories and the listed plugins
  " without having to install curl, 7-zip and git tools first
  " -> BUG [4] (git-less installation)
  let vam_autoload_dir = a:plugin_root_dir.'/vim-addon-manager/autoload'
  if isdirectory(vam_autoload_dir)
    return 1
  else
    if 1 == confirm("Clone VAM into ".a:plugin_root_dir."?","&Y\n&N")
      " I'm sorry having to add this reminder. Eventually it'll pay off.
      call confirm("Remind yourself that most plugins ship with ".
            \"documentation (README*, doc/*.txt). It is your ".
            \"first source of knowledge. If you can't find ".
            \"the info you're looking for in reasonable ".
            \"time ask maintainers to improve documentation")
      call mkdir(a:plugin_root_dir, 'p')
      execute '!git clone --depth=1 git://github.com/MarcWeber/vim-addon-manager '.
            \       shellescape(a:plugin_root_dir, 1).'/vim-addon-manager'
      " VAM runs helptags automatically when you install or update 
      " plugins
      exec 'helptags '.fnameescape(a:plugin_root_dir.'/vim-addon-manager/doc')
    endif
    return isdirectory(vam_autoload_dir)
  endif
endfun

fun! SetupVAM()
  " Set advanced options like this:
  " let g:vim_addon_manager = {}
  " let g:vim_addon_manager.key = value
  "     Pipe all output into a buffer which gets written to disk
  " let g:vim_addon_manager.log_to_buf =1

  " Example: drop git sources unless git is in PATH. Same plugins can
  " be installed from www.vim.org. Lookup MergeSources to get more control
  " let g:vim_addon_manager.drop_git_sources = !executable('git')
  " let g:vim_addon_manager.debug_activation = 1

  " VAM install location:
  let plugin_root_dir = expand('$HOME/.vim/vim-addons')
  if !EnsureVamIsOnDisk(plugin_root_dir)
    echohl ErrorMsg | echomsg "No VAM found!" | echohl NONE
    return
  endif
  let &rtp.=(empty(&rtp)?'':',').plugin_root_dir.'/vim-addon-manager'

  " Tell VAM which plugins to fetch & load:
  call vam#ActivateAddons([
    \ 'vim-twig',
    \ 'vim-less',
    \ 'vim-coffee-script',
    \ 'DetectIndent',
    \ 'a',
    \ 'github:scrooloose/nerdcommenter',
    \ 'github:ervandew/supertab',
    \ 'Tagbar',
    \ 'github:altercation/vim-colors-solarized',
    \ 'ctrlp',
    \ 'BufOnly',
    \ 'keepcase',
    \ 'Tabular'
  \ ], {'auto_install' : 0})
  " sample: call vam#ActivateAddons(['pluginA','pluginB', ...], {'auto_install' : 0})

  " Addons are put into plugin_root_dir/plugin-name directory
  " unless those directories exist. Then they are activated.
  " Activating means adding addon dirs to rtp and do some additional
  " magic

  " How to find addon names?
  " - look up source from pool
  " - (<c-x><c-p> complete plugin names):
  " You can use name rewritings to point to sources:
  "    ..ActivateAddons(["github:foo", .. => github://foo/vim-addon-foo
  "    ..ActivateAddons(["github:user/repo", .. => github://user/repo
  " Also see section "2.2. names of addons and addon sources" in VAM's documentation
endfun
call SetupVAM()
" experimental [E1]: load plugins lazily depending on filetype, See
" NOTES
" experimental [E2]: run after gui has been started (gvim) [3]
" option1:  au VimEnter * call SetupVAM()
" option2:  au GUIEnter * call SetupVAM()
" See BUGS sections below [*]
" Vim 7.0 users see BUGS section [3]

syntax on

au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif
au BufReadPost * set formatoptions=croq
au BufReadPost * :DetectIndent

au BufReadPost *.md,*.markdown set spell

set showcmd      " Show (partial) command in status line.
set showmatch    " Show matching brackets.
set showcmd      " Show counts of matching lines
set ignorecase   " Do case insensitive matching
set smartcase    " Do smart case matching
set incsearch    " Incremental search
set autowrite    " Automatically save before commands like :next and :make
set hidden       " Hide buffers when they are abandoned
set mouse=a      " Enable mouse usage (all modes)
set ruler
set autoindent

" Indentation is 4 spaces by default
set ts=4 sts=4 sw=4 expandtab
let g:detectindent_preferred_expandtab = 1
let g:detectindent_preferred_indent = 4

" Don't treat 0-padded numbers as octal when doing numeric stuff
set nrformats-=octal

let g:SuperTabDefaultCompletionType = "context"

let g:tagbar_usearrows = 1
nnoremap <leader>t :TagbarToggle<CR>

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
  \ 'Rakefile'
  \ ]

map <C-h> <C-w>h
map <C-j> <C-w>j
map <C-k> <C-w>k
map <C-l> <C-w>l

set completeopt=longest,menuone
set wildmode=longest,list,full
set wildmenu

let g:alternateNoDefaultAlternate = 1
let g:alternateRelativeFiles = 1

" Set path to include everything below working directory
set path=,,**

" Efene/ifene support
au BufRead,BufNewFile *.fn,*.ifn set filetype=efene
au! Syntax efene source ~/.vim/syntax/efene.vim

" Nobody likes Ex mode
map Q <Nop>

set background=dark
let g:solarized_termtrans=1
let g:solarized_contrast="high"
let g:solarized_visibility="high"
colorscheme solarized

" Disable arrow keys, they are for the weak
"noremap  <Up> ""
"noremap! <Up> <Esc>
"noremap  <Down> ""
"noremap! <Down> <Esc>
"noremap  <Left> ""
"noremap! <Left> <Esc>
"noremap  <Right> ""
"noremap! <Right> <Esc>

set cc=80
set magic
set showbreak=+++++

" Set long lines as break lines
map j gj
map k gk

map 0 ^

set wrapscan

" TODO Figure out why PHP files get munged comment settings
au BufReadPost *.php set comments=sO:*\ -,mO:*\ \ ,ex0:*/,s1:/*,mb:*,ex:*/1

au BufReadPost *.md set wrap linebreak nolist textwidth=0 wrapmargin=0 cc=0

map <leader>pc :w !xsel -i<CR>
map <leader>pp :r!xsel<CR>

set backspace=2

let mapleader=","

au WinLeave * set nocursorline nocursorcolumn
au WinEnter * set cursorline cursorcolumn
set cursorline cursorcolumn
