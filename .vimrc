" vim: set foldmethod=marker foldcolumn=2 ignorecase smartcase nospell :
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
"{{{
set nocompatible
set history=500                       " default was 50
" set autoread                          " preventing something that I just write
" au FocusGained,BufEnter * :silent! !  " -- reload when entering the buffer or gaining focus
" au FocusLost,WinLeave * :silent! w    " -- save when exiting the buffer or losing focus

set clipboard=unnamed
set ttimeoutlen=0                     " eliminating time delay to Normal mode
set sidescroll=1                      " options: 0, 1, 2, ....
" set virtualedit=all
set modeline
set modelines=10
set spell
set spelllang=en_ca
" set spelllang=EN_ca
" set colorcolumn=80,120
set path+=**                  " include sub directories when searching 2021-01-06
set updatetime=1000           " for gitgutter 2021-01-13
"}}}


"--------
" NUMBER
"--------
set nu
set rnu "relativenumber


"-----------
" Autogroup
"-----------
"{{{
augroup auto_set_number
    autocmd InsertEnter * set nornu | set nocursorline " | set nocursorcolumn
    autocmd InsertLeave * set rnu   | set cursorline   " | set cursorcolumn
augroup END
" augroup auto_set_number
"     autocmd InsertEnter * set nornu
"         \ | hi StatusLine guifg=NONE ctermfg=NONE
"         \ | hi CursorLine gui=NONE guibg=lightyellow
"         \ | hi CursorLineNr guibg=orange guifg=white
"     autocmd InsertLeave * set rnu
"         \ | hi StatusLine guifg=#cfd8dc ctermfg=66
"         \ | hi CursorLine gui=underline guibg=NONE
"         \ | hi CursorLineNr guibg=black
" augroup END
"}}}
"

"--------
" SEARCH
"--------
set hlsearch
set incsearch
set noignorecase
set nosmartcase



"-------
" NETRW
"-------
let g:netrw_altv=1             " open split to the right
" let g:netrw_browse_split=4     " open in prior window
let g:netrw_liststyle=3        " treeview


filetype off
" for vundle -> re-set after vundle like : filetype plug indent 'on'
"---------
" PLUG-IN
"---------

call plug#begin('~/.vim/plugged')
" Plug 'iamcco/markdown-preview.nvim', { 'do': { -> mkdp#util#install() }, 'for': ['markdown', 'vim-plug']}
" Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'chrisbra/csv.vim'
" Plug 'jceb/vim-orgmode'
call plug#end()


set rtp+=~/.vim/bundle/Vundle.vim

call vundle#begin()

Plugin 'VundleVim/Vundle.vim'

"---------Themes------------
" Plugin 'vim-airline/vim-airline'
Plugin 'vim-airline/vim-airline-themes'
" Plugin 'itchyny/lightline.vim'
" Plugin 'powerline/powerline'
Plugin 'flazz/vim-colorschemes'
" Plugin 'altercation/vim-colors-solarized'

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
" Plugin 'scrooloose/nerdcommenter'
Plugin 'tpope/vim-surround'
Plugin 'tpope/vim-repeat'
" Plugin 'terryma/vim-multiple-cursors'  " good but not Vim way
" Plugin 'gcmt/wildfire.vim'             " good but conflict with Tagbar keys like <C-m>|<enter>
" Plugin 'shougo/neocomplete.vim'        " lua required
" Plugin 'valloric/youcompleteme'        " gave up due to too-hard to insall 2020-11-20
" Plugin 'davidhalter/jedi-vim'          " not working - hard to solve 2020-11-20
" Plugin 'mattn/emmet-vim'               " conflicted with <C-y>
Plugin 'severin-lemaignan/vim-minimap'
" Plugin 'terryma/vim-expand-region'
" Plugin 'w0rp/ale'                      " Asynchronous Lint Engine ??
" Plugin 'ap/vim-css-color'              " complicted with vim modeline filetype markdown
" Plugin 'neoclide/coc.nvim'               " intellicense - popup suggestion 2020-12-21
Plugin 'w0rp/ale'

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
" Plugin 'plasticboy/vim-markdown'
" Plugin 'iamcco/markdown-preview.nvim'
Plugin 'masukomi/vim-markdown-folding'
Plugin 'reedes/vim-wordy'
Plugin 'dbmrq/vim-ditto'                 " find repeated words
" Plugin 'reedes/vim-lexical'            " no idea
" Plugin 'reedes/vim-pencil'             " no idea
" Plugin 'xolox/vim-notes'
" Plugin 'blueyed/vim-diminactive'
Plugin 'vim-pandoc/vim-pandoc'
Plugin 'vim-pandoc/vim-pandoc-syntax'
Plugin 'beloglazov/vim-online-thesaurus' " 2021-02-26

"------Functionality--------
Plugin 'kien/ctrlp.vim'
Plugin 'scrooloose/nerdtree'          " not a vimway instead use find command
Plugin 'mbbill/undotree'
" Plugin 'sjl/gundo.vim'                  " visualize your Vim undo tree
" Plugin 'wincent/command-t'            " Ruby required
" Plugin 'junegunn/fzf'                 " fzf require Go lang
" Plugin 'junegunn/fzf.vim'
Plugin 'easymotion/vim-easymotion'
" Plugin 'haya14busa/incsearch.vim'
" Plugin 'haya14busa/incsearch-fuzzy.vim'
" Plugin 'haya14busa/incsearch-easymotion.vim'
Plugin 'mileszs/ack.vim'
" Plugin 'searchfold.vim'               " embigiuous with marker
" Plugin 'wincent/command-t'            " ruby required
Plugin 'ervandew/supertab'
Plugin 'machakann/vim-highlightedyank'  " 2021-02-26
" Plugin 'justinmk/vim-dirvish'
Plugin 'rking/ag.vim'                   " Silver Searcher (ag) 2021-03-28
Plugin 'liuchengxu/vim-which-key'       " 2021-04-16
Plugin 'itchyny/vim-cursorword'         " 2021-04-16

"----------Python-----------
Plugin 'nvie/vim-flake8'
Plugin 'davidhalter/jedi-vim'
Plugin 'sirver/ultisnips'
Plugin 'klen/python-mode'

"------Other_plugins--------
" Plugin 'itchyny/calendar.vim'
Plugin 'jceb/vim-orgmode'
Plugin 'tpope/vim-speeddating'
" Plugin 'takac/vim-hardtime'

call vundle#end()

filetype plugin indent on
set omnifunc=syntaxcomplete#Complete
syntax on

" let g:indentLine_char = '¦'
" let g:indentLine_char_list = ['|', '¦', '┆', '┊']
let g:searchfold_maxdepth=1
let g:solarized_termcolors=256
let g:NERDTreeWinSize=40
" let g:org_indent=1
let g:highlightedyank_highlight_duration = 500


"----------------------------------------------
 "MARKDOWN SUPPORT for 'plasticboy/vim-markdown'
 "A Text file as a Markdown file 2020-11-21
"----------------------------------------------
"{{{
augroup text_to_markdown
    " autocmd BufRead,BufNewFile *.txt set filetype=markdown
    " let g:vim_markdown_folding_disabled = 1
    " foldmethod from 'expr' to 'manual'
    " let g:vim_markdown_follow_anchor = 0  " to use vim `ge` command avoiding in markdown mode
    let g:indentLine_concealcursor=""
    let g:indentLine_conceallevel=2
    " autocmd BufRead,BufNewFile *.txt set concealcursor="" | set conceallevel=2
    autocmd BufRead,BufNewFile markdown set concealcursor="" | set conceallevel=0
    " Default was 'inc' stands for insert, normal, and command
    " except for visual by 'indentline' plugin
    " When cursor is on a markdown syntax,
    " it will be show its markdown grammar automatically
    " -------------------
    let g:vim_markdown_no_default_key_mappings = 1  " enable `ge` command
    " let g:vim_markdown_folding_style_pythonic = 1   " folding including title
    let g:vim_markdown_override_foldtext = 0
augroup END

" this is for markdown files only
" foldmethod from 'expr' to 'manual'
"}}}

" for markdown-folding plugin setting
autocmd FileType markdown set foldexpr=NestedMarkdownFolds()

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
"{{{
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
"}}}

set thesaurus+=~/.vim/thesaurus/mthesaur.txt
set thesaurus+=~/.vim/thesaurus/test_thesaur.txt


" -------------------------
" PERSISTENT UNDO TREE SAVE
" -------------------------
" must create '~/.undodir' directory first
if has("persistent_undo")
    set undodir=$HOME."/.undodir"
    set undofile
endif
set undodir=~/.undodir
set undofile


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
" colorscheme papercolor
" colorscheme solarized
" colorscheme monokai
" highlight LineNr ctermfg=darkgray
highlight SignColumn ctermbg=NONE  " for gitgutter
highlight FoldColumn ctermbg=NONE  " for foldcolumn

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
      \ 'colorscheme': 'default',
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
set bs=2              " back space


"---------
" TABLINE
"---------
set showtabline=2

" let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#left_sep = ' '
let g:airline#extensions#tabline#left_alt_sep = '|'
let g:airline#extensions#tabline#formatter = 'unique_tail'


"--------
" CURSOR
"--------
" set cursorcolumn
set cursorline
"hi CursorLine cterm=NONE ctermbg=Black
"highlight CursorLineNR term=Bold cterm=Bold ctermbg=Black "number column


"------------
" STATUSLINE
"------------
set ruler
set laststatus=2
set showcmd
"{{{
" Status Line Custom
let g:currentmode={
    \ 'n'  : 'Normal',
    \ 'no' : 'Normal·Operator Pending',
    \ 'v'  : 'Visual',
    \ 'V'  : 'V·Line',
    \ "\<C-V>" : 'V·Block',
    \ 's'  : 'Select',
    \ 'S'  : 'S·Line',
    \ '^S' : 'S·Block',
    \ 'i'  : 'Insert',
    \ 'R'  : 'Replace',
    \ 'Rv' : 'V·Replace',
    \ 'c'  : 'Command',
    \ 'cv' : 'Vim Ex',
    \ 'ce' : 'Ex',
    \ 'r'  : 'Prompt',
    \ 'rm' : 'More',
    \ 'r?' : 'Confirm',
    \ '!'  : 'Shell',
    \ 't'  : 'Terminal'
    \}
"}}}

function! GitBranch()
  return system("git rev-parse --abbrev-ref HEAD 2>/dev/null | tr -d '\n'")
endfunction

function! StatuslineGit()
  let l:branchname = GitBranch()
  return strlen(l:branchname) > 0?'  '.l:branchname.' ':''
endfunction

function! FileSize()
    let bytes = getfsize(expand("%:p"))
    if bytes <= 0
        return ""
    endif
    if bytes < 1024
        " return bytes
        return "┆ " . (bytes) . "b"
    else
        " return "┆ " . (bytes / 1024) . "k"  " binary
        return "┆ " . (bytes / 1000) . "k"
    endif
endfunction

""{{{
set statusline=
" set statusline+=\               " blank
" set statusline=\[%{mode()}\]    " current mode
set statusline+=%0*\ %{toupper(g:currentmode[mode()])}  " The current mode
set statusline+=┆\             " separator
" -----------------------------
" https://vi.stackexchange.com/questions/10458/how-do-i-conditionally-add-items-to-the-statusline
" 2021-03-15
" set stl+=%{&spell\ ?\ line(\".\")\ :\ \"\"}
" set stl+=%{&spell?'SPELL':'NO_SPELL'}
" set stl+=%{&spell?'SPELL\ 🅂\ ┆\ ':''}
" set stl+=%{&spell?'SPELL\ 🅂\ >\ ':''}
set stl+=%{&spell?'🅂\ ':''}
" set stl+=%{&spell?'SPELL':''}
" set stl+=%{&spell?':':''}
" set stl+=%{&spell?'[':''}
" set stl+=%{&spell?'(':''}
set stl+=%{&spell?&spelllang:''}
" set stl+=%{&spell?']':''}
" set stl+=%{&spell?')':''}
set stl+=%{&spell?'\ ┆\ ':''}
" set statusline+=%{&spelllang}
" set statusline+=\ \|\            " separator
"set statusline+=%#Tabline#
"set statusline+=\               " blank
" set statusline+=\ %{fugitive#statusline()}\   
set statusline+=\ %{fugitive#head()}\   
"⭠
" GitGutter 2021-03-15
" https://github.com/airblade/vim-gitgutter/pull/709#issuecomment-635856742
function! GitStatus()
  let [a,m,r] = GitGutterGetHunkSummary()
  return [a,m,r] == [0,0,0] ? '' : printf('+%d ~%d -%d', a, m, r)
endfunction
set statusline+=%{GitStatus()}
" set statusline+=\ -\            " separator
set statusline+=┆\             " separator
set stl+=%{&modified?'**⤴\ ':''}  " test not working
" set stl+=%{&modified?'𝓔𝓭𝓲𝓽𝓮𝓭\ ':''}  " test not working
" 𝓜𝓸𝓸𝓭𝓲𝓯𝓲𝓮𝓭 / 𝓒𝓱𝓪𝓷𝓰𝓮𝓭 / 𝓔𝓭𝓲𝓽𝓮𝓭 / 𝓡𝓮𝓿𝓲𝓼𝓮𝓭 / ✘ / ☡ / ⤴  
" 𝘌𝘋𝘐𝘛 / ℰ𝒹𝒾𝓉 
set statusline+=%f              " path
" set statusline+=\               " blank
set stl+=%{&modified?'\ [+]':''}  " 
" set statusline+=%m              " modified flag [+]
" set statusline+=\ -\            " separator
" set statusline+=%y              " [filetype] of the file
" set statusline+=┆\             " separator
set statusline+=%{FileSize()}
" set statusline+=%#PmenuSel#
" set statusline+=%{StatuslineGit()}
" set statusline+=┆\             " separator
" set statusline+=%{wordcount().words}\ words
" set stl+=%{&ignorecase?'┆\ IGNORECASE\ ã':''}
set stl+=%{&ignorecase?'┆\ IGNORE\ ã':''}
""~~~~~~~~~~~~~~~~<center>~~~~~~~~~~~~~~~~~~~~~~~~
set statusline+=%=              " right align
set statusline+=\ %Y              " FILETYPE of the file
set statusline+=┆            " separator
" set statusline+=%#PmenuSel#
" set statusline+=%#MoreMsg#
set statusline+=\ %{&fileencoding?&fileencoding:&encoding}
" set statusline+=\               " blank
set statusline+=\(%{&fileformat}\)
" set statusline+=\ %{&fileformat}
" set statusline+=%2*0x%04B\ %* " character under cursor
" set statusline+=%b " decimal byte '98'
" set statusline+=\               " blank
set statusline+=\[x%02B] " hex byte 'x62'
set statusline+=┆\            " separator
" set statusline+=\ ☰ \          " trigram seperator
" set statusline+=\             " blank
"" set statusline+=Line:\ %l/%L  " line of total
" set statusline+=LN⭡:\ %l\ of\ %L\ ☰\ [%p%%]
" set statusline+=LN⭡:\ %l/%L\ ☰\ [%p%%]
set statusline+=\ %l/%L\ ☰\ [%p%%]
"" set statusline+=\%%           " percent sign only
" set statusline+=\             " blank
"" set statusline+=%P            " percentage of file/buffer
" set statusline+=\ \∞\ Col:\ %c      " coloumn
set statusline+=\ ꜛ%c      " coloumn ∞
"" set statusline+=%-7.(%l of %L [%p%%] - Col: %c%V%) "Current line, percentage of size, column,
"" required to know how to apply statuline grouping grammar 2020-12-31
" set statusline+=\             " blank
set statusline+=┆\           " separator
" set statusline+=\ ¦           " separator
set statusline+=%{strftime('%H:%M')}
set statusline+=\         " blank
""}}}

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


"===========
" KEY-REMAP
"============

" --------
"  Space
" --------
nnoremap <space> :
vnoremap <space> :

" Buffer: show list and ready to choose
" nnoremap <silent><space>bl :buffers<CR>:buffer<Space>
" better way -> :b {keyword that I remember}<tab>


" --------
"  Normal
" --------
"{{{
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
nnoremap <tab> za
nnoremap <S-tab> zM
nnoremap <S-M-tab> zR

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

" nnoremap <silent><space>wh <C-w>h
" nnoremap <silent><space>wj <C-w>j
" nnoremap <silent><space>wk <C-w>k
" nnoremap <silent><space>wl <C-w>l

nnoremap j gj
nnoremap k gk

" Hard Mode (Anti-Pattern)
" tips: '+' and '-' move lines, or 'gj' and 'gk' 2021-03-09
" nnoremap hh <nop>
" nnoremap jj <nop>
" nnoremap kk <nop>
" nnoremap ll <nop>
"}}}

" --------
"  Insert
" --------
"{{{
inoremap <up>    <nop>
inoremap <down>  <nop>
inoremap <left>  <nop>
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
inoremap <silent>ii <Esc>
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
"}}}

" ---------
"  Command
" ---------
"{{{
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
" delay esc storke
" cnoremap <Esc>b <S-Left>
" cnoremap <Esc>f <S-Right>
" cnoremap <Esc>d <S-Right><Delete>
cnoremap <C-g> <C-c>
"}}}

"===========
" FUNCTIONS
"===========

"-----------
" Draw Line
"-----------

"{{{
function! UnderLine()
    normal yy
    normal p
    s/./-/g
    noh
    " echo "underlined"
endfunction
command! UnderLine call UnderLine()
"}}}

"{{{
function! DoubleLine()
    normal yy
    normal p
    s/./=/g
    noh
endfunction
command! DoubleLine call DoubleLine()
"}}}

"{{{
function! SurroundLine()
    normal yy
    normal p
    s/./-/g
    t-2  " copy current line to above 2 up
    noh
endfunction
command! SurroundLine call SurroundLine()
"}}}


function! HardMode()
"{{{
    silent! nnoremap hh <nop>
    silent! nnoremap jj <nop>
    silent! nnoremap kk <nop>
    silent! nnoremap ll <nop>
    silent! unmap j
    silent! unmap k
    silent! noremap <up>    <nop>
    silent! noremap <down>  <nop>
    silent! noremap <left>  <nop>
    silent! noremap <right> <nop>
    " silent! unmap <tab>
    " silent! unmap <S-tab>
    Limelight!
    set scrolloff=0
    set noignorecase
    set nosmartcase
endfunction
command! HardMode call HardMode()
"}}}

" Disabled to prevent from overuse
function! EasyMode()
"{{{
    silent! unmap hh
    silent! unmap jj
    silent! unmap kk
    silent! unmap ll
    silent! nnoremap j gj
    silent! nnoremap k gk
    silent! unmap <tab>
    Limelight!
    set scrolloff=0
    set ignorecase
    set smartcase
endfunction
command! EasyMode call EasyMode()
"}}}

function! SuperEasyMode()
"{{{
    call EasyMode()
    silent! unmap   <up>
    silent! unmap   <down>
    silent! unmap   <left>
    silent! unmap   <right>
    silent! iunmap   <up>
    silent! iunmap   <down>
    silent! iunmap   <left>
    silent! iunmap   <right>
    silent! noremap <up> gk
    silent! noremap <down> gj
    silent! inoremap <up> <c-\><c-o>gk
    silent! inoremap <down> <c-\><c-o>gj
    nnoremap <tab> za
    " nnoremap <tab> zo
    " nnoremap <S-tab> zc
    Limelight
    set scrolloff=999
endfunction
command! SuperEasyMode call SuperEasyMode()
"}}}


"----------
" Focusing
"----------

function! FocusMode()"{{{
    " let g:line_size_before = &lines
    " let g:column_size_before = &columns
    if exists('g:OnFocusing')
        unlet g:OnFocusing
        call UnFocusMode()
        echo "debug logging: UnFocusMode executed"
        return 0
    elseif exists('g:OnEditing')
        unlet g:OnEditing
        call UnFocusMode()
    endif
    Goyo
    Limelight
    autocmd InsertLeave * :set norelativenumber | hi CursorLine gui=NONE
    set scrolloff=999  " centering
    set sidescrolloff=30
    set ignorecase
    set smartcase
    call SuperEasyMode()
    " set focusing variable
    let g:OnFocusing=1
    if has('gui_running')
        " set guifont=Menlo-Regular:h20
        " set lines=99 columns=999   " to maximize window size"
        highlight Visual guifg=bg guibg=DarkGreen gui=NONE
        highlight CursorLine gui=NONE guibg=NONE
    elseif exists('$TMUX')
        silent !tmux set status off
    endif
endfunction
command! FocusMode call FocusMode()"}}}

function! DarkFocusMode()"{{{
    " let g:line_size_before = &lines
    " let g:column_size_before = &columns
    if exists('g:OnFocusing')
        unlet g:OnFocusing
        call UnFocusMode()
        echo "debug logging: UnFocusMode executed"
        return 0
    elseif exists('g:OnEditing')
        unlet g:OnEditing
        call UnFocusMode()
    endif
    Goyo
    Limelight 0.8
    autocmd InsertLeave * :set norelativenumber
    set scrolloff=999  " centering
    set sidescrolloff=30
    set ignorecase
    set smartcase
    call EasyMode()
    " set variable
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
command! DarkFocusMode call DarkFocusMode()"}}}

function! EditMode()"{{{
    " let g:line_size_before = &lines
    " let g:column_size_before = &columns
    if exists('g:OnEditing')
        unlet g:OnEditing
        call UnFocusMode()
        echo "debug logging: UnFocusMode executed"
        return 0
    elseif exists('g:OnFocusing')
        unlet g:OnFocusing
        call UnFocusMode()
    endif
    Goyo 100%x100%
    Limelight!
    autocmd InsertLeave * :set norelativenumber
    set ignorecase
    set smartcase
    call EasyMode()
    " set variable
    let g:OnEditing=1
    if has('gui_running')
        " set guifont=Menlo-Regular:h15
        " set lines=99 columns=999
        " colorscheme github
        highlight Visual guifg=bg guibg=DarkGreen gui=NONE
    elseif exists('$TMUX')
        silent !tmux set status off
    endif
    " normal zz
endfunction
command! EditMode call EditMode()"}}}

function! UnFocusMode()
"{{{
    Goyo!
    Limelight!
    silent! unlet g:OnFocusing
    silent! unlet g:OnEditing
    autocmd InsertLeave * :set relativenumber
    set scrolloff=0
    set sidescrolloff=0
    set noignorecase
    set nosmartcase
    syntax enable  " redraw markdown highlighting
    " syntax on
    call HardMode()
    if has('gui_running')
        " set guifont=Menlo-Regular:h15
        " execute "set lines =" . g:line_size_before
        " execute "set columns =" . g:column_size_before
        " unlet g:line_size_before
        " unlet g:column_size_before
        set background=light
        colorscheme basic-light
        " highlight LineNr guibg=white
        " highlight SignColumn guibg='#f0f0f0'  " for gitgutter
        " highlight FoldColumn guibg=white  " for foldcolumn
        highlight CursorLineNr guibg=black guifg=white
        highlight CursorLine gui=underline guibg=NONE
        highlight Visual guifg=bg guibg=DarkGreen gui=NONE
        set cursorline
        " hi EasyMotionTarget guifg=red guibg=yellow
    elseif exists('$TMUX')
        silent !tmux set status on
    endif
endfunction
command! UnFocusMode call UnFocusMode()
"}}}

" ==========================
" Debugging & Test Function"
" ==========================
"{{{
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
    " call EasyMode()
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
"}}}


" [FYI] checking terminal mode
" -----------------------------
"  if !has('gui_running')"

" ---
" GUI
" notice: all gui seeting shoud be located inside this if statement 2021-03-18
" ---

if has('gui_running')"{{{
    set guioptions=     " disabled mac style tab"
    set guifont=Meslo\ LG\ S\ Regular\ for\ Powerline:h14
    " set guifont=MesloLGS\ Nerd\ Font:h14
    set lines=999 columns=9999  " full size windows 2021-04-21
    cd ~/Documents/nvALT/
    set linespace=1
    " set colorcolumn=105
    " set nocursorcolumn
    set background=light
    " colorscheme basic-light
    colorscheme default
    " highlight LineNr guibg=white
    highlight SignColumn guibg='white'       " for gitgutter
    highlight FoldColumn guibg='white'       " for foldcolumn
    " highlight Visual guifg=black guibg=Cyan gui=NONE
    " highlight CursorLineNr guibg=black guifg=white
    " highlight CursorLine gui=underline guibg=NONE
    hi TabLine gui=NONE guibg=black guifg=white  " Deactive Area
    " hi TabLineSel gui=NONE guibg=black guifg=white
    " hi TabLineFill gui=NONE guibg=black guifg=white
    let g:airline_theme='serene'  "default raven serene luna monochrome powerlineish term transparent distinguished
    " hi EasyMotionTarget guifg=red guibg=yellow
    highlight Folded guibg=grey guifg=blue
endif"}}}

"========================================
" LEADER KEY
" > could be, 'a', 'ab', 'A', 'AB', etc
" > The Less The Better
"========================================
" let mapleader = ','

" delete all contents for whole ine
" nnoremap <leader>d :normal ggVGD<cr>i

"---------------
" Focusing Mode
"---------------
" nnoremap <silent> <leader>f :FocusToggle<cr>
nnoremap <silent> <leader>r :FocusMode<cr>
nnoremap <silent> <leader>d :DarkFocusMode<cr>
nnoremap <silent> <leader>e :EditMode<cr>
nnoremap <silent> <leader>q :UnFocusMode<cr>

"------------
" Utilities
"------------
nnoremap <silent> <leader>n  :NERDTreeToggle<cr>
nnoremap <silent> <leader>t  :TagbarToggle<cr>
nnoremap <silent> <leader>b  :bro old<cr>
" maximize window size -> insted, vanila Vim: c-w _ / c-w
" nnoremap <silent> <leader>mw  :vert resize 999<cr>
" nnoremap <silent> <leader>mh  :resize 999<cr>

nnoremap <silent> <leader><leader>r :source $MYVIMRC<cr>
    \ :echo "<<< .vimrc reloaded >>>"<cr>
" undo leader shortcuts "
nnoremap <silent> <leader>uu  :UndotreeToggle<cr>
nnoremap <silent> <leader>uf  :UndotreeFocus<cr>
nnoremap <silent> <leader>us  :UndotreeShow<cr>
nnoremap <silent> <leader>uh  :UndotreeHide<cr>
" markdown support"
vnoremap <silent> <leader>* s**<C-r>"**<esc>

"------------
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
" Easy to Use (Vimium?)
" map  f <Plug>(easymotion-bd-f)
" nmap f <Plug>(easymotion-overwin-f)

"------------
" IncSearch
"------------
" You can use other keymappings like <C-l> instead of <CR> if you want to{{{
" use these mappings as default search and sometimes want to move cursor with
" EasyMotion.

" function! s:incsearch_config(...) abort
"   return incsearch#util#deepextend(deepcopy({
"   \   'modules': [incsearch#config#easymotion#module({'overwin': 1})],
"   \   'keymap': {
"   \     "\<CR>": '<Over>(easymotion)'
"   \   },
"   \   'is_expr': 0
"   \ }), get(a:, 1, {}))
" endfunction
" noremap <silent><expr> /  incsearch#go(<SID>incsearch_config())
" noremap <silent><expr> ?  incsearch#go(<SID>incsearch_config({'command': '?'}))
" noremap <silent><expr> g/ incsearch#go(<SID>incsearch_config({'is_stay': 1}))

" IncSearch - Fuzzy "

" function! s:config_easyfuzzymotion(...) abort
"   return extend(copy({
"   \   'converters': [incsearch#config#fuzzyword#converter()],
"   \   'modules': [incsearch#config#easymotion#module({'overwin': 1})],
"   \   'keymap': {"\<CR>": '<Over>(easymotion)'},
"   \   'is_expr': 0,
"   \   'is_stay': 1
"   \ }), get(a:, 1, {}))
" endfunction
"" noremap <silent><expr> <Space>/ incsearch#go(<SID>config_easyfuzzymotion())
" **** Occured Space Delay ****}}}

"------------------
" Command Shortcut
"------------------
" command NE NERDTreeToggle
" command TB TagbarToggle
command PI PluginInstall
command MD set filetype=markdown
" command HT set filetype=html
" command TX set filetype=text
command PD :w | set filetype=pandoc | Pandoc html
command WH windo wincmd H
command HA call HardMode() | echo "HardMode Activated"
" command EA call EasyMode() | echo "EasyMode Activated"
" command SE call SuperEasyMode() | echo "SuperEasyMode Activated"


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
ab xorg -*- mode: org; -*-
ab xftp ftp://osickwon@ftp.osickwon.heliohost.org/public_html/


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


"----------------------
" PaperColor Filetypes
"----------------------
autocmd BufEnter *.py    colorscheme PaperColor | set background=dark
autocmd BufEnter *.vimrc colorscheme PaperColor | set background=dark
autocmd BufEnter *.php colorscheme Pencil | set background=dark
autocmd BufEnter *.html colorscheme Pencil | set background=dark
autocmd BufEnter *.phtml colorscheme Pencil | set background=dark


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

