"        _
"       (_)
" __   ___ _ __ ___  _ __ ___
" \ \ / / | '_ ` _ \| '__/ __|
"  \ V /| | | | | | | | | (__
"   \_/ |_|_| |_| |_|_|  \___|
"
" since 2020
" **************************
" Persudae Plain Vanila Vim
" **************************

"-----------------
" INITIAL SETTING
"-----------------
set nocompatible
set history=500  " default was 50 "
set autoread
set clipboard=unnamed
set ttimeoutlen=0 "elminating time delay to Normal mode
set sidescroll=1 " 0, 1, 2, ....
" set virtualedit=all
set modeline
set modelines=10
set spell

"--------
" NUMBER
"--------
set nu
set rnu "relativenumber
augroup auto_set_number
    autocmd InsertEnter * set nornu
    autocmd InsertLeave * set rnu
augroup END

"--------
" SEARCH
"--------
set hlsearch
set incsearch
set noignorecase
set nosmartcase


filetype off
" for vundle -> re-set after vundle like : filetype plug indent 'on'
"---------
" PLUG-IN
"---------

call plug#begin('~/.vim/plugged')
" Plug 'iamcco/markdown-preview.nvim', { 'do': { -> mkdp#util#install() }, 'for': ['markdown', 'vim-plug']}
" Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'chrisbra/csv.vim'
call plug#end()


set rtp+=~/.vim/bundle/Vundle.vim

call vundle#begin()

Plugin 'VundleVim/Vundle.vim'

"---------Themes------------
Plugin 'vim-airline/vim-airline'
Plugin 'vim-airline/vim-airline-themes'
" Plugin 'itchyny/lightline.vim'
"Plugin 'powerline/powerline'
Plugin 'flazz/vim-colorschemes'
Plugin 'altercation/vim-colors-solarized'


"--------SnipMate-----------
Plugin 'MarcWeber/vim-addon-mw-utils'
Plugin 'tomtom/tlib_vim'
Plugin 'garbas/vim-snipmate'
" Optional:
Plugin 'honza/vim-snippets'

"---------Coding------------
Plugin 'scrooloose/syntastic'
Plugin 'majutsushi/tagbar'
Plugin 'yggdroot/indentline'
Plugin 'tpope/vim-commentary'
"Plugin 'scrooloose/nerdcommenter'
Plugin 'tpope/vim-surround'
Plugin 'tpope/vim-repeat'
" Plugin 'terryma/vim-multiple-cursors'  " good but not Vim way
" Plugin 'gcmt/wildfire.vim'             " good but conflict with Tagbar keys like <C-m>|<enter>
" Plugin 'shougo/neocomplete.vim'        " lua required
" Plugin 'ervandew/supertab'             " code completion with tab
" Plugin 'valloric/youcompleteme'        " gave up due to too-hard to insall 2020-11-20
" Plugin 'davidhalter/jedi-vim'          " not working - hard to solve 2020-11-20
" Plugin 'mattn/emmet-vim'               " conflicted with <C-y>
" Plugin 'severin-lemaignan/vim-minimap'
Plugin 'terryma/vim-expand-region'
" Plugin 'w0rp/ale'                      " Asynchronous Lint Engine ??
" Plugin 'ap/vim-css-color'              " complicted with vim modeline filetype markdown
Plugin 'neoclide/coc.nvim'               " intellicense - popup suggestion 2020-12-21


"----------Git--------------
Plugin 'airblade/vim-gitgutter'
Plugin 'tpope/vim-fugitive'
Plugin 'junegunn/gv.vim'


"----------Data-------------
" Plugin 'chrisbra/csv.vim'              " not working. instead, >> Plug 'chrisbra/csv.vim'
Plugin 'mechatroner/rainbow_csv'


"---------Writing-----------
Plugin 'junegunn/goyo.vim'
Plugin 'junegunn/limelight.vim'
Plugin 'dhruvasagar/vim-table-mode'
Plugin 'godlygeek/tabular'
Plugin 'plasticboy/vim-markdown'
" Plugin 'iamcco/markdown-preview.nvim'
Plugin 'reedes/vim-wordy'
" Plugin 'reedes/vim-lexical'            " perhaps be included in internal Vim
" Plugin 'reedes/vim-pencil'             " perhaps be included in internal Vim
" Plugin 'xolox/vim-notes'
Plugin 'blueyed/vim-diminactive'
Plugin 'vim-pandoc/vim-pandoc'
Plugin 'vim-pandoc/vim-pandoc-syntax'

"------Functionality--------
" Plugin 'kien/ctrlp.vim'
Plugin 'scrooloose/nerdtree'
Plugin 'mbbill/undotree'
Plugin 'sjl/gundo.vim'                  " visualize your Vim undo tree
" Plugin 'wincent/command-t'            " Ruby required
" Plugin 'junegunn/fzf'                 " fzf require Go lang
" Plugin 'junegunn/fzf.vim'
Plugin 'easymotion/vim-easymotion'
Plugin 'haya14busa/incsearch.vim'
Plugin 'haya14busa/incsearch-fuzzy.vim'
Plugin 'haya14busa/incsearch-easymotion.vim'
Plugin 'mileszs/ack.vim'
Plugin 'searchfold.vim'               " embigiuous with marker


"------Other-plugins--------
Plugin 'itchyny/calendar.vim'


call vundle#end()

filetype plugin indent on
set omnifunc=syntaxcomplete#Complete
syntax on

let g:searchfold_maxdepth=1
" let g:vim_markdown_folding_disabled = 1
let g:solarized_termcolors=256
let g:airline_theme='tomorrow'  "default minimalist bubblegum raven angr
" air-line
let g:airline_powerline_fonts = 1

if !exists('g:airline_symbols')
    let g:airline_symbols = {}
endif
" the separator used on the left side
let g:airline_left_sep=''
" the separator used on the right side 
let g:airline_right_sep=''

" unicode symbols
let g:airline_left_sep = '»'
let g:airline_left_sep = '▶'
let g:airline_right_sep = '«'
let g:airline_right_sep = '◀'
let g:airline_symbols.linenr = '␊'
let g:airline_symbols.linenr = '␤'
let g:airline_symbols.linenr = '¶'
let g:airline_symbols.branch = '⎇'
let g:airline_symbols.paste = 'ρ'
let g:airline_symbols.paste = 'Þ'
let g:airline_symbols.paste = '∥'
let g:airline_symbols.whitespace = 'Ξ'

" airline symbols
let g:airline_left_sep = ''
let g:airline_left_alt_sep = ''
let g:airline_right_sep = ''
let g:airline_right_alt_sep = ''
let g:airline_symbols.branch = ''
let g:airline_symbols.readonly = ''
let g:airline_symbols.linenr = ''

set thesaurus+=~/.vim/thesaurus/mthesaur.txt
set thesaurus+=~/.vim/thesaurus/test_thesaur.txt

" -------------------------
" PERSISTENT UNDO TREE SAVE
" -------------------------
" if has("persistent_undo")
"     set undodir=$HOME."/.undodir"
"     set undofile
" endif
set undofile
set undodir=~/.undodir


"----------------------------------------------
 "MARKDOWN SUPPORT for 'plasticboy/vim-markdown'
 "A Text file as a Markdown file 2020-11-21
"----------------------------------------------
augroup text_to_markdown
    autocmd BufRead,BufNewFile *.txt set filetype=markdown
    let g:vim_markdown_folding_disabled = 1
    " foldmethod from 'expr' to 'manual'
    let g:indentLine_concealcursor=""
    let g:indentLine_conceallevel=2
    "autocmd BufRead,BufNewFile *.txt set concealcursor="" | set conceallevel=2
    " Default was 'inc' stands for insert, normal, and command 
    " except for visual by 'indentline' plugin
    " When cursor is on a markdown syntax, 
    " it will be show its markdown grammar automatically
augroup END

" this is for markdown files only
" foldmethod from 'expr' to 'manual'


"--------
" TRAILS
"--------
set list
set listchars=tab:>·,trail:·


"------
" FOLD
"------
set foldenable              " required to make sure how it works"
set foldmethod=manual
set viewoptions-=options    "must
augroup remember_folds
    autocmd!
    autocmd BufWinLeave *.* mkview
    autocmd BufWinEnter *.* silent! loadview
augroup END


"--------
" COLOR
"--------
set background=dark
colorscheme papercolor
" colorscheme solarized
" colorscheme monokai
" highlight LineNr ctermfg=darkgray


"----------------------
" SPLIT VERTICAL COLOR
"----------------------
" hi VertSplit cterm=NONE
" hi VertSplit ctermbg=darkgray ctermfg=237
" set fillchars+=vert:\ 


"-----------------
" LIGHTLINE THEME
"-----------------
let g:lightline = {
      \ 'active': {
      \   'left': [ [ 'mode', 'paste' ],
      \             [ 'gitbranch', 'readonly', 'filename', 'modified' ] ]
      \ },
      \ 'component_function': {
      \   'gitbranch': 'FugitiveHead'
      \ },
      \ }

"------------
" TAB CONTROL
"------------
set smartindent
set autoindent
set tabstop=4
set softtabstop=4
set shiftwidth=4
set expandtab
set bs=2 "back space

"---------
" TABLINE
"---------
"set showtabline=2
" let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#left_sep = ' '
let g:airline#extensions#tabline#left_alt_sep = '|'
let g:airline#extensions#tabline#formatter = 'unique_tail'


"--------
" CURSOR
"--------
set cursorcolumn
set cursorline
"hi CursorLine cterm=NONE ctermbg=Black
"highlight CursorLineNR term=Bold cterm=Bold ctermbg=Black "number column


"------------
" STATUSLINE
"------------
set ruler
set laststatus=1
set showcmd

set statusline+=\               " blank
set statusline=\[%{mode()}\]    " current mode
"set statusline+=\ Osic
"set statusline+=\ World
set statusline+=\               " blank
set statusline+=%f              " path
"set statusline+=\ -\            " separator
"set statusline+=FileType:       " label
set statusline+=\               " blank
set statusline+=%y              " filetype of the file
set statusline+=%m              " modified flag [+]
"------------------
set statusline+=%=              " right align
set statusline+=\ %{&fileencoding?&fileencoding:&encoding}
set statusline+=\               " blank
set statusline+=\[%{&fileformat}\]
set statusline+=\               " blank
set statusline+=Col:\ %c          " coloumn
set statusline+=\               " blank
set statusline+=Line:\ %l/%L           " line of total
set statusline+=\               " blank
set statusline+=%p              " percentage of lines
set statusline+=\%%
"set statusline+=%2*0x%04B\ %*   " character under cursor
set statusline+=\               " blank
"set statusline+=%P              " percentage of file/buffer


"--------------
" TEXT EDITING
"--------------
set linebreak


"----------------
" SPLIT DIRECTION
"----------------
set splitbelow
set splitright


"-------------------
" FIND or EDIT FILE
"-------------------
set wildmenu  " :find <tab> -> show wildmenu (suggesting files)
set wildmode=list,full


"-----------
" KEY-REMAP
"-----------

nnoremap <space> :
vnoremap <space> :

nnoremap <up>    <nop>
nnoremap <down>  <nop>
nnoremap <left>  <nop>
nnoremap <right> <nop>

" nnoremap <up>    <C-w>k
" nnoremap <down>  <C-w>j
" nnoremap <left>  <C-w>h
" nnoremap <right> <C-w>l

" nnoremap <C-k> <C-w>k
" nnoremap <C-j> <C-w>j
" nnoremap <C-h> <C-w>h
" nnoremap <C-l> <C-w>l

" Window Control in Normal Mode

" useful but not vimway
" nnoremap <tab> <C-w>w
" nnoremap <S-tab> <C-w>W


"RESIZING WINDOWS but Not Vim Way
" nnoremap <up>       :3wincmd +<CR>
" nnoremap <down>     :3wincmd -<CR>
" nnoremap <left>     :3wincmd <<CR>
" nnoremap <right>    :3wincmd ><CR>

" nnoremap <M-k> :3wincmd +<cr>
" nnoremap <M-j> :3wincmd -<cr>
" nnoremap <M-h> :3wincmd <<cr>
" nnoremap <M-l> :3wincmd ><cr>

nnoremap <C-k> :3wincmd +<cr>
nnoremap <C-j> :3wincmd -<cr>
nnoremap <C-h> :3wincmd <<cr>
nnoremap <C-l> :3wincmd ><cr>

" Hard Mode
nnoremap hh <nop>
nnoremap jj <nop>
nnoremap kk <nop>
nnoremap ll <nop>

inoremap <up> <nop>
inoremap <down> <nop>
inoremap <left> <nop>
inoremap <right> <nop>

"inoremap <C-h> <Left>
"inoremap <C-j> <C-o>gj
"inoremap <C-k> <C-o>gk
"inoremap <C-l> <Right>

" Emacs style Keybinding
inoremap <C-a> <Home>
inoremap <C-f> <Right>
inoremap <C-b> <Left>
inoremap <C-d> <Delete>
inoremap <C-e> <End>

" Escape in many ways in Insert mode"
inoremap <silent>jj <Esc>
inoremap <silent>kk <Esc>
" inoremap kj <Esc>
" inoremap jk <Esc>

" pair
" inoremap ( ()<Left>
" inoremap { {}<Left>
" inoremap [ []<Left>
" inoremap < <><Left>
" inoremap " ""<Left>

vnoremap <up> <nop>
vnoremap <down> <nop>
vnoremap <left> <nop>
vnoremap <right> <nop>

cnoremap <up> <nop>
cnoremap <down> <nop>
cnoremap <left> <nop>
cnoremap <right> <nop>

" Emacs style Keybinding in command mode
"----------------------------------------
cnoremap <C-a> <Home>
cnoremap <C-b> <Left>
cnoremap <C-f> <Right>
cnoremap <C-d> <Delete>
cnoremap <M-b> <S-Left>
cnoremap <M-f> <S-Right>
cnoremap <M-d> <S-Right><Delete>
cnoremap <Esc>b <S-Left>
cnoremap <Esc>f <S-Right>
cnoremap <Esc>d <S-Right><Delete>
cnoremap <C-g> <C-c>


"-----------
" FUNCTIONS
"-----------

" Draw Line
"-----------
function! UnderLine()
    normal yy
    normal p
    s/./-/g
    noh
    " echo "underlined"
endfunction
command! UnderLine call UnderLine()

function! DoubleLine()
    normal yy
    normal p
    s/./=/g
    noh
endfunction
command! DoubleLine call DoubleLine()

function! SurroundLine()
    normal yy
    normal p
    s/./-/g
    t-2  " copy current line to above 2 up
    noh
endfunction
command! SurroundLine call SurroundLine()

function! HardMode()
    silent! nnoremap hh <nop>
    silent! nnoremap jj <nop>
    silent! nnoremap kk <nop>
    silent! nnoremap ll <nop>
    silent! unmap j
    silent! unmap k
endfunction
command! HardMode call HardMode()

"function! MediumMode()
"    silent! nnoremap hh <nop>
"    "silent! nnoremap jj <nop>
"    "silent! nnoremap kk <nop>
"    silent! nnoremap ll <nop>
"    silent! nnoremap j gj
"    silent! nnoremap k gk
"endfunction
"command! Medium call MediumMode()

function! EasyMode()
    silent! unmap hh
    silent! unmap jj
    silent! unmap kk
    silent! unmap ll
    silent! nnoremap j gj
    silent! nnoremap k gk
endfunction
command! EasyMode call EasyMode()


" Focusing 
"----------

function! FocusMode()
    " let g:line_size_before = &lines
    " let g:column_size_before = &columns
    if exists('g:OnFocusing')
        call UnFocusMode()
        unlet g:OnFocusing
        echo "debug logging: UnFocusMode executed"
        return 0
    elseif exists('g:OnEditing')
        call UnFocusMode()
        unlet g:OnEditing
    endif
    Goyo 100%x100%
    Limelight 0.8
    autocmd InsertLeave * :set norelativenumber
    set scrolloff=999  " centering
    set ignorecase
    set smartcase
    call EasyMode()
    let g:OnFocusing=1
    if has('gui_running')
        " set guifont=Menlo-Regular:h20
        " set lines=99 columns=999   " to maximize window size"
    elseif exists('$TMUX')
        silent !tmux set status off
    endif
endfunction
command! FocusMode call FocusMode()

function! DarkFocusMode()
    " let g:line_size_before = &lines
    " let g:column_size_before = &columns
    if exists('g:OnFocusing')
        call UnFocusMode()
        unlet g:OnFocusing
        echo "debug logging: UnFocusMode executed"
        return 0
    elseif exists('g:OnEditing')
        call UnFocusMode()
        unlet g:OnEditing
    endif
    Goyo
    Limelight 0.8
    autocmd InsertLeave * :set norelativenumber
    set scrolloff=999  " centering
    set ignorecase
    set smartcase
    call EasyMode()
    let g:OnFocusing=1
    if has('gui_running')
        " set guifont=Menlo-Regular:h20
        " set lines=99 columns=999   " to maximize window size"
        colorscheme dark
        set cursorline!
    elseif exists('$TMUX')
        silent !tmux set status off
    endif
endfunction
command! DarkFocusMode call DarkFocusMode()

function! EditMode()
    " let g:line_size_before = &lines
    " let g:column_size_before = &columns
    if exists('g:OnEditing')
        call UnFocusMode()
        unlet g:OnEditing
        echo "debug logging: UnFocusMode executed"
        return 0
    elseif exists('g:OnFocusing')
        call UnFocusMode()
        unlet g:OnFocusing
    endif
    Goyo 100%x100%
    Limelight!
    autocmd InsertLeave * :set norelativenumber
    set ignorecase
    set smartcase
    call EasyMode()
    let g:OnEditing=1
    if has('gui_running')
        " set guifont=Menlo-Regular:h15
        " set lines=99 columns=999
        " colorscheme github
    elseif exists('$TMUX')
        silent !tmux set status off
    endif
    normal zz
endfunction
command! EditMode call EditMode()

function! UnFocusMode()
    Goyo!
    Limelight!
    autocmd InsertLeave * :set relativenumber
    set scrolloff=0
    set noignorecase
    set nosmartcase
    syntax enable  " redraw markdown highlighting
    call HardMode()
    if has('gui_running')
        " set guifont=Menlo-Regular:h15
        " execute "set lines =" . g:line_size_before
        " execute "set columns =" . g:column_size_before
        " unlet g:line_size_before
        " unlet g:column_size_before
        set background=light
        colorscheme PaperColor
        set cursorline
    elseif exists('$TMUX')
        silent !tmux set status on
    endif
endfunction
command! UnFocusMode call UnFocusMode()

" ==========================
" Debugging & Test Function"
" ==========================

" --------------
" argument test"
" --------------
function! TestFunction(arg)
    if a:arg == "abc" 
        echo "abc"
    else
        echo "456"
    endif
endfunction
" command! TF call TestFunction()


" -----------------------
" test search and replace
" -----------------------
function! TestFunction2()

    call EasyMode()

    " let v = "\*\{2\}"
    " echo v
    " execute "%s/\\v(" . v . ")/x/"
    " " execute "%s/\\v*{2}.*/xxx/"
    " echo "Test Done"

    " Original Pattern: (\*{2}).{-}\1
    let bold_pattern = "\*\{2\}.\{-\}\*\{2\}"
    " non greedy regex in vim -> {-} (standard: '.*?')
    normal qaq
    execute "%s/\\v" . bold_pattern . "/\\=setreg('A', submatch(0), 'V')/gn"
    " 'Very Magic Mode' -> '\v' => escaped -> '\\v' in script
    put A  " same as "AP

endfunction
command! T2 call TestFunction2()



" [FYI] checking terminal mode
" -----------------------------
"  if !has('gui_running')"

" ---
" GUI
" ---

if has('gui_running')
    set guioptions=     " disabled mac style tab"
    set guifont=Meslo\ LG\ S\ Regular\ for\ Powerline:h14
    set linespace=1
    " set colorcolumn=105
    set cursorcolumn!
    set background=light
    colorscheme PaperColor
    let g:airline_theme='powerlineish'  "default raven luna monochrome
    hi EasyMotionTarget guifg=red guibg=black
endif


"------------------------------------------
" LEADER KEY SETTING 
" > could be, 'a', 'ab', 'A', 'AB', etc
" > The Less The Better
"------------------------------------------
"let mapleader = ','

" delete all contents for whole ine
" nnoremap <leader>d :normal ggVGD<cr>i

" Focusing Mode
"---------------
" nnoremap <silent> <leader>f :FocusToggle<cr>
nnoremap <silent> <leader>r :FocusMode<cr>
nnoremap <silent> <leader>d :DarkFocusMode<cr>
nnoremap <silent> <leader>e :EditMode<cr>
nnoremap <silent> <leader>q :UnFocusMode<cr>

" Utilitises
"------------
nnoremap <silent> <leader>n  :NERDTreeToggle<cr>
nnoremap <silent> <leader>t  :TagbarToggle<cr>
" nnoremap <silent> <leader>b  :bro old<cr>
" maximize window size -> insted, vanila Vim: c-w _ / c-w 
" nnoremap <silent> <leader>mw  :vert resize 999<cr>
" nnoremap <silent> <leader>mh  :resize 999<cr>

nnoremap <silent> <leader><leader>r :source $MYVIMRC<cr> 
    \ :echo "<<< .vimrc reloaded >>>"<cr>
" undo leader shortcuts "
nnoremap <silent> <leader>u   :UndotreeToggle<cr>
nnoremap <silent> <leader>uf  :UndotreeFocus<cr>
nnoremap <silent> <leader>us  :UndotreeShow<cr>
nnoremap <silent> <leader>uh  :UndotreeHide<cr>
" markdown support"
vnoremap <silent> <leader>* s**<C-r>"**<esc>

" EasyMotion
"------------
" <Leader>f{char} to move to {char}
map  <Leader>f <Plug>(easymotion-bd-f)
nmap <Leader>f <Plug>(easymotion-overwin-f)
" s{char}{char} to move to {char}{char}
nmap <leader>s <Plug>(easymotion-overwin-f2)
" Move to line
map <Leader>L <Plug>(easymotion-bd-jk)
nmap <Leader>L <Plug>(easymotion-overwin-line)
" Move to word
map  <Leader>w <Plug>(easymotion-bd-w)
nmap <Leader>w <Plug>(easymotion-overwin-w)

" IncSearch
"------------
" You can use other keymappings like <C-l> instead of <CR> if you want to
" use these mappings as default search and sometimes want to move cursor with
" EasyMotion.

function! s:incsearch_config(...) abort
  return incsearch#util#deepextend(deepcopy({
  \   'modules': [incsearch#config#easymotion#module({'overwin': 1})],
  \   'keymap': {
  \     "\<CR>": '<Over>(easymotion)'
  \   },
  \   'is_expr': 0
  \ }), get(a:, 1, {}))
endfunction
noremap <silent><expr> /  incsearch#go(<SID>incsearch_config())
noremap <silent><expr> ?  incsearch#go(<SID>incsearch_config({'command': '?'}))
noremap <silent><expr> g/ incsearch#go(<SID>incsearch_config({'is_stay': 1}))

" IncSearch - Fuzzy "

function! s:config_easyfuzzymotion(...) abort
  return extend(copy({
  \   'converters': [incsearch#config#fuzzyword#converter()],
  \   'modules': [incsearch#config#easymotion#module({'overwin': 1})],
  \   'keymap': {"\<CR>": '<Over>(easymotion)'},
  \   'is_expr': 0,
  \   'is_stay': 1
  \ }), get(a:, 1, {}))
endfunction
noremap <silent><expr> <Space>/ incsearch#go(<SID>config_easyfuzzymotion())


"------------------
" Command Shortcut
"------------------
" command NE NERDTreeToggle
" command TB TagbarToggle
command PI PluginInstall
command MD set filetype=markdown
command HT set filetype=html
command TX set filetype=text
command PD :w | set filetype=pandoc | Pandoc html
command WH windo wincmd H


" --------------"
" Abbreviation "
" --------------"
" capture only matched pattern"
" before -> qaq
" cab xx %s/PATTEN/\=setreg('A', submatch(0), 'V')/gn
" cab xx %s/\v(\*{2}).{-}\1/\=setreg('A', submatch(0), 'V')/gn
" --- very magic mode -> \v
" --- lazy mode(non-greedy) -> .{-}
" after -> :put A or (insert mode)"AP


"-----------
" SHORTCUTS
"-----------
" map <silent> <C-b> :UndotreeToggle<cr>
" map <silent> <C-n> :NERDTreeToggle<CR>
" map <silent> <C-m> :TagbarToggle<CR>
" <Enter> also works, but I don't know why 2020-11-20
" <C-m> means <Enter> in Vim default
" <Enter> conflicted with 'wildfire' plugin


"------
" JAVA
"------
autocmd Filetype java set makeprg=javac\ %
set errorformat=%A%f:%l:\ %m,%-Z%p^,%-C%.%#
map <F9> :make<Return>:copen<Return>
map <F10> :cprevious<Return>
map <F11> :cnext<Return>


"---------------
" MODE SETTINGS
"---------------
let &t_SI.="\e[5 q" "SI = INSERT mode
"let &t_SR.="\e[4 q" "SR = REPLACE mode
let &t_EI.="\e[1 q" "EI = NORMAL mode (ELSE)

"Cursor settings:
"  1 -> blinking block
"  2 -> solid block
"  3 -> blinking underscore
"  4 -> solid underscore
"  5 -> blinking vertical bar
"  6 -> solid vertical bar


" test ------------------------------------------------
"
" https://vim.fandom.com/wiki/Word_frequency_statistics_for_a_file
" Sorts numbers in ascending order.
" Examples:
" [2, 3, 1, 11, 2] --> [1, 2, 2, 3, 11]
" ['2', '1', '10','-1'] --> [-1, 1, 2, 10]
function! Sorted(list)
  " Make sure the list consists of numbers (and not strings)
  " This also ensures that the original list is not modified
  let nrs = ToNrs(a:list)
  let sortedList = sort(nrs, "NaturalOrder")
  echo sortedList
  return sortedList
endfunction

" Comparator function for natural ordering of numbers
function! NaturalOrder(firstNr, secondNr)
  if a:firstNr < a:secondNr
    return -1
  elseif a:firstNr > a:secondNr
    return 1
  else 
    return 0
  endif
endfunction

" Coerces every element of a list to a number. Returns a new list without
" modifying the original list.
function! ToNrs(list)
  let nrs = []
  for elem in a:list
    let nr = 0 + elem
    call add(nrs, nr)
  endfor

  return nrs
endfunction

function! WordFrequency() range
  " Words are separated by whitespace or punctuation characters
  let wordSeparators = '[[:blank:][:punct:]]\+'
  let allWords = split(join(getline(a:firstline, a:lastline)), wordSeparators)
  let wordToCount = {}
  for word in allWords
    let wordToCount[word] = get(wordToCount, word, 0) + 1
  endfor

  let countToWords = {}
  for [word,cnt] in items(wordToCount)
    let words = get(countToWords,cnt,"")
    " Append this word to the other words that occur as many times in the text
    let countToWords[cnt] = words . " " . word
  endfor

  " Create a new buffer to show the results in
  new
  setlocal buftype=nofile bufhidden=hide noswapfile tabstop=20

  " List of word counts in ascending order
  let sortedWordCounts = Sorted(keys(countToWords))

  call append("$", "count \t words")
  call append("$", "--------------------------")
  " Show the most frequent words first -> Descending order
  for cnt in reverse(sortedWordCounts)
    let words = countToWords[cnt]
    call append("$", cnt . "\t" . words)
  endfor
endfunction

command! -range=% WordFrequency <line1>,<line2>call WordFrequency()

"----------------------------------------"
