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
" Pursue Plain Vanila Vim
" **************************

"-----------------
" INITIAL SETTING
"-----------------
"{{{
set nocompatible
set history=500                         " default was 50
" set autoread                          " preventing something that I just write
" au FocusGained,BufEnter * :silent! !  " -- reload when entering the buffer or gaining focus
" au FocusLost,WinLeave * :silent! w    " -- save when exiting the buffer or losing focus
" https://morgan.cugerone.com/blog/troubleshooting-vim-error-while-processing-cursorhold-autocommands-in-command-line-window/
autocmd CursorHold * silent! checktime

set encoding=utf-8
" set noswapfile                          " edit duplicated opened file anywhere
set clipboard=unnamed
" set ttimeoutlen=0                     " eliminating time delay in Normal mode
" set timeoutlen=3000
set sidescroll=1                        " options: 0, 1, 2, ....
" set virtualedit=all
set complete+=kspell
set completeopt=menuone,longest
set completeopt+=longest,menuone,noinsert
" https://www.reddit.com/r/vim/comments/5w6wac/vim_users_of_reddit_whats_your_favorite/
set completeopt-=preview
set completeopt+=menu,menuone,noinsert,noselect
set shortmess+=c

set modeline
set modelines=10
set nospell
set spelllang=en_ca
" set spelllang=EN_ca
" set colorcolumn=80,120
set path+=**                  " include sub directories when searching 2021-01-06
set updatetime=1000           " for gitgutter 2021-01-13
" set textwidth=80
"
" https://www.reddit.com/r/vim/comments/h8pgor/til_conceal_in_vim/
set concealcursor=""
set conceallevel=0 " Nothing is hidden
" set conceallevel=1 " Hide stuff as concealchar, or as listchar.
" set conceallevel=2 " Hide stuff as concealchar, or completely.
" set conceallevel=3 " Hide completely.
set matchpairs+=<:>
" show more
" set scrolloff=5
" set sidescrolloff=5
set mouse=a

"}}}


"--------
" NUMBER
"--------
set nu
set rnu "relativenumber
" https://jeffkreeftmeijer.com/vim-number/
" interrupted functional buffers like Tagbar ...
" augroup numbertoggle
"   autocmd!
"   autocmd BufEnter,FocusGained,InsertLeave * set relativenumber
"   autocmd BufLeave,FocusLost,InsertEnter   * set norelativenumber
" augroup END
" augroup numbertoggle
"   autocmd!
"   autocmd FocusGained * set rnu
"   autocmd FocusLost   * set nu
" augroup END

"-----------
" Autogroup
"-----------
"{{{
augroup auto_set_number
    autocmd InsertEnter * set nornu | set nocursorline " | set nocursorcolumn
    autocmd InsertLeave * set rnu   | set cursorline   " | set cursorcolumn
    " autocmd InsertEnter * set nocursorline " | set nocursorcolumn
    " autocmd InsertLeave * set cursorline   " | set cursorcolumn
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


"--------
" SEARCH
"--------
set hlsearch
set incsearch
set ignorecase " Search 'This' > this, This, THIS -- selected all
" set noignorecase
set smartcase  " Search 'This' > this, This, THIS -- only 'This' selected



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
" Plug 'chrisbra/csv.vim'
" Plug 'jceb/vim-orgmode'
" Plug 'davidhalter/jedi-vim'
" Plug 'sheerun/vim-polyglot'
" Plug 'neoclide/coc.nvim', {'branch': 'release'}  " Dot Net core 2.1 required
" https://github.com/neovim/neovim/issues/4612#issuecomment-354718725
Plug 'tyru/open-browser.vim' "{
  " Disable netrw gx mapping.
  let g:netrw_nogx = get(g:, 'netrw_nogx', 1)
  nmap gx <Plug>(openbrowser-open)
  vmap gx <Plug>(openbrowser-open)
"}
" Plug 'mg979/vim-visual-multi', {'branch': 'master'}  " multi-cursor
call plug#end()


" ------
" VUNDLE
" ------
set rtp+=~/.vim/bundle/Vundle.vim

call vundle#begin()

Plugin 'VundleVim/Vundle.vim'

"---------Themes------------
Plugin 'vim-airline/vim-airline'
Plugin 'vim-airline/vim-airline-themes'
" Plugin 'itchyny/lightline.vim'
" Plugin 'powerline/powerline'
Plugin 'flazz/vim-colorschemes'
" Plugin 'altercation/vim-colors-solarized'
" Plugin 'kyoz/purify'
Plugin 'kyoz/purify', { 'rtp': 'vim' }
Plugin 'joshdick/onedark.vim'
Plugin 'rakr/vim-one'
Plugin 'gosukiwi/vim-atom-dark'
Plugin 'arcticicestudio/nord-vim'
Plugin 'nanotech/jellybeans.vim'

"--------SnipMate-----------
Plugin 'MarcWeber/vim-addon-mw-utils'
Plugin 'tomtom/tlib_vim'
" Plugin 'garbas/vim-snipmate'
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
" Plugin 'terryma/vim-multiple-cursors'  " good but not Vim way :: change to Plug 'mg979/vim-visual-multi', {'branch': 'master'}
" Plugin 'gcmt/wildfire.vim'             " good but conflict with Tagbar keys like <C-m>|<enter>
Plugin 'severin-lemaignan/vim-minimap'
Plugin 'terryma/vim-expand-region'
Plugin 'w0rp/ale'                        " Asynchronous Lint Engine ?? -> very powerful
" Plugin 'ap/vim-css-color'              " complicted with vim modeline filetype markdown
" Plugin 'puremourning/vimspector'        " require hight version of mac
Plugin 'jpalardy/vim-slime'               " syntax highlight
Plugin 'prettier/vim-prettier'

"------AutoComplete---------
" Plugin 'valloric/youcompleteme'        " gave up due to too-hard to insall 2020-11-20
Plugin 'sirver/ultisnips'
" Plugin 'mattn/emmet-vim'               " conflicted with <C-y>
" Plugin 'shougo/neocomplete.vim'        " lua required
" Plugin 'neoclide/coc.nvim'               " intellicense - popup suggestion 2020-12-21

"----------Git--------------
Plugin 'airblade/vim-gitgutter'
Plugin 'tpope/vim-fugitive'
Plugin 'junegunn/gv.vim'

"----------Python-----------
" Plugin 'davidhalter/jedi-vim'
Plugin 'nvie/vim-flake8'
" Plugin 'klen/python-mode'
" Plugin 'hanschen/vim-ipython-cell'  " ipython
Plugin 'jmcantrell/vim-virtualenv'
Plugin 'psf/black'
Plugin 'integralist/vim-mypy'
Plugin 'fisadev/vim-isort'

"----------Javascript-----------
Plugin 'pangloss/vim-javascript'

"-----------PHP-------------
Plugin 'stanangeloff/php.vim'
"
"----------Data-------------
" Plugin 'chrisbra/csv.vim'              " not working. instead, >> in Plug 'chrisbra/csv.vim'
Plugin 'mechatroner/rainbow_csv'

"-------PlantUML------------
Plugin 'aklt/plantuml-syntax'
Plugin 'weirongxu/plantuml-previewer.vim'
Plugin 'tyru/open-browser.vim'

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
" Plugin 'kien/ctrlp.vim'
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
Plugin 'ervandew/supertab'
Plugin 'machakann/vim-highlightedyank'  " 2021-02-26
" Plugin 'justinmk/vim-dirvish'
Plugin 'rking/ag.vim'                   " Silver Searcher (ag) 2021-03-28
" Plugin 'liuchengxu/vim-which-key'       " 2021-04-16
Plugin 'itchyny/vim-cursorword'         " 2021-04-16
Plugin 'francoiscabrol/ranger.vim'      " 2021-04-24 - I don't know how
" Plugin 'mhinz/vim-startify'
Plugin 'szw/vim-maximizer'
Plugin 'AutoComplPop'

"------Other_plugins--------
Plugin 'itchyny/calendar.vim'
Plugin 'jceb/vim-orgmode'
Plugin 'tpope/vim-speeddating'
" Plugin 'takac/vim-hardtime'
" Plugin 'psliwka/vim-smoothie'

call vundle#end()

filetype plugin indent on
set omnifunc=syntaxcomplete#Complete
syntax on

" == ale ==
" https://www.youtube.com/watch?v=4FKPySR6HLk
let g:ale_linters = {
            \ 'python': ['flake8', 'pydocstyle', 'mypy'],
            \ 'javascript': ['eslint'],
            \ }
let g:ale_fixers = {
            \ '*': ['remove_trailing_lines', 'trim_whitespace'],
            \ 'python': ['black', 'isort'],
            \ 'javascript': ['prettier', 'eslint'],
            \ }
" \ 'python': ['black', 'isort'], << isort not working for now 2021-07-26
let g:ale_fix_on_save = 1

" https://github.com/dense-analysis/ale/issues/2885
" let g:ale_python_isort_options = '--settings-path .'

" let g:indentLine_char = '¦'
let g:indentLine_char = '┆'
" let g:indentLine_char = '┊'
" let g:indentLine_char_list = ['|', '¦', '┆', '┊']
let g:searchfold_maxdepth=1
let g:solarized_termcolors=256
let g:NERDTreeWinSize=40
" let g:org_indent=1
let g:highlightedyank_highlight_duration = 500

" https://stackoverflow.com/questions/21628743/cant-get-the-jedi-vim-plugin-to-work
let g:jedi#force_py_version = 3

let g:jedi#completions_enabled = 0

let g:ranger_map_keys = 0  " disable default ranger key -> <leader>f
" https://github.com/vim-syntastic/syntastic/issues/2242
" solved f-string issue (invalid syntax error)
let g:syntastic_python_checkers = ['python']
let g:syntastic_python_python_exec = 'python3'

let g:purify_bold = 0        " default: 1
let g:purify_italic = 0      " default: 1
let g:purify_underline = 1   " default: 1
let g:purify_undercurl = 0   " default: 1
let g:purify_inverse = 0     " default: 1
" let g:purify_override_colors = {
"     \ 'pink':  { 'gui': '#FF87FF', 'cterm': '213' },
"     \ 'yellow':  { 'gui': '#fdf6e3', 'cterm': '230' },
"     \ 'green': { 'gui': '#5FD700', 'cterm': '76' }
" \ }
let g:tagbar_sort = 0
let g:SuperTabDefaultCompletionType = "<c-n>"  " reverse selection order

" C-c C-c, to send, .1 (pane #, ensure <prefix>q in tmux)
let g:slime_target = "tmux"
" Color name (:help cterm-colors) or ANSI code
let g:limelight_conceal_ctermfg = 'gray'
let g:limelight_conceal_ctermfg = 240
" Color name (:help gui-colors) or RGB color
let g:limelight_conceal_guifg = 'DarkGray'
let g:limelight_conceal_guifg = '#777777'

autocmd FileType python setlocal completeopt-=preview

augroup OmniCompletionSetup
    autocmd!
    autocmd FileType c          set omnifunc=ccomplete#Complete
    autocmd FileType php        set omnifunc=phpcomplete#CompletePHP
    autocmd FileType python     set omnifunc=jedi#completions
    autocmd FileType ruby       set omnifunc=rubycomplete#Complete
    autocmd FileType javascript set omnifunc=javascriptcomplete#CompleteJS
    autocmd FileType html       set omnifunc=htmlcomplete#CompleteTags
    autocmd FileType css        set omnifunc=csscomplete#CompleteCSS
    autocmd FileType xml        set omnifunc=xmlcomplete#CompleteTags
augroup END


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

let g:airline_theme='wombat'  "default minimalist bubblegum raven angr tomorrow wombat powerlineish

let g:airline#extensions#virtualenv#enabled = 1
" let g:airline_section_y = airline#section#create('%{virtualenv#statusline()}')
"
" if !empty($PYENV_VIRTUAL_ENV)
if !empty($VIRTUAL_ENV)
  " let g:airline_section_y = "%{split($PYENV_VIRTUAL_ENV, '/')[-1]}"
  let g:airline_section_y = airline#section#create('%{virtualenv#statusline()}')
el
  " let g:airline_section_y = "testtesttest   %{&fileencoding?&fileencoding:&encoding}  %{&fileformat}\ "
  let g:airline_section_y = "%{&fileencoding?&fileencoding:&encoding}[%{&fileformat}]"
endif

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
" let g:airline_left_sep = '»'
" let g:airline_left_sep = '▶'
" let g:airline_right_sep = '«'
" let g:airline_right_sep = '◀'
let g:airline_symbols.linenr = '␊'
let g:airline_symbols.linenr = '␤'
let g:airline_symbols.linenr = '¶'
let g:airline_symbols.branch = '⎇'
let g:airline_symbols.paste = 'ρ'
let g:airline_symbols.paste = 'Þ'
let g:airline_symbols.paste = '∥'
let g:airline_symbols.whitespace = 'Ξ'

" powerline symbols
" let g:airline_left_sep = ''
" let g:airline_left_alt_sep = ''
" let g:airline_right_sep = ''
" let g:airline_right_alt_sep = ''
let g:airline_symbols.branch = ''
let g:airline_symbols.colnr = ' :'
" let g:airline_symbols.colnr = 'C:'
let g:airline_symbols.readonly = ''
let g:airline_symbols.linenr = ' :'
let g:airline_symbols.maxlinenr = '☰  '
let g:airline_symbols.dirty='⚡'
let g:airline_symbols.dirty=''

" airline symbols
" let g:airline_left_sep = ''
" let g:airline_left_alt_sep = ''
" let g:airline_right_sep = ''
" let g:airline_right_alt_sep = ''
let g:airline_symbols.branch = ''
let g:airline_symbols.readonly = ''
let g:airline_symbols.linenr = ' :'
" let g:airline_symbols.linenr = ' L:'
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
colorscheme PaperColor
" colorscheme solarized
" colorscheme monokai
" highlight LineNr ctermfg=darkgray
highlight SignColumn ctermbg=NONE  " for gitgutter
highlight FoldColumn ctermbg=NONE  " for foldcolumn
highlight EndOfBuffer ctermfg=black

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
" set statusline+=%#Tabline#
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
" set statusline+=┆            " separator
set statusline+=\               " separator
set statusline+=%y              " [filetype] of the file
" set statusline+=\ %Y              " FILETYPE of the file
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
" set statusline+=\ %Y              " FILETYPE of the file
" set statusline+=┆            " separator
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

" mouse enabled
map <ScrollWheelUp> <C-Y>
map <ScrollWheelDown> <C-E>


" --------
"  Normal
" --------
"{{{
"
"
nnoremap Y y$

" nnoremap n nzzzv
" nnoremap N Nzzzv
" nnoremap * *zzzv
" nnoremap # #zzzv
" nnoremap } }zzzv
" nnoremap { {zzzv
" nnoremap ) )zzzv
" nnoremap ( (zzzv
" nnoremap <c-d> <c-d>zzzv
" nnoremap <c-u> <c-u>zzzv
"
" == ALE plugin ==
" https://til.hashrocket.com/posts/szvpivakjq-go-to-next-ale-error
:nmap ]a :ALENextWrap<CR>
:nmap [a :ALEPreviousWrap<CR>
:nmap ]A :ALELast<CR>
:nmap [A :ALEFirst<CR>
"
"
" https://www.youtube.com/watch?v=p2SbmpVl6zw
" nnoremap <CR> :nohlsearch<cr>  "interrupted in mini window
"
"
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
" ---
" nnoremap <tab> za
" nnoremap <S-tab> zM
" nnoremap <S-M-tab> zR

"RESIZING WINDOWS but Not Vim Way
" nnoremap <up>       :3wincmd +<CR>
" nnoremap <down>     :3wincmd -<CR>
" nnoremap <left>     :3wincmd <<CR>
" nnoremap <right>    :3wincmd ><CR>

" nnoremap <C-h> <C-w>h
" nnoremap <C-j> <C-w>j
" nnoremap <C-k> <C-w>k
" nnoremap <C-l> <C-w>l
"
nnoremap <C-k> :3wincmd +<cr>
nnoremap <C-j> :3wincmd -<cr>
nnoremap <C-h> :3wincmd <<cr>
nnoremap <C-l> :3wincmd ><cr>

" on mac, meta M is recognized a prefix for special character
" nnoremap <M-k> :3wincmd +<cr>
" nnoremap <M-j> :3wincmd -<cr>
" nnoremap <M-h> :3wincmd <<cr>
" nnoremap <M-l> :3wincmd ><cr>

" so, use a special key as a M-hjkl for macvim
" in terminal, turn off 'option key' as a meta
" in iterm2, meta key as a 'Noraml'
" but, emacs needs the meta key!!
" https://stackoverflow.com/questions/7501092/can-i-map-alt-key-in-vim
" nnoremap ˙ :3wincmd <<cr>
" nnoremap ∆ :3wincmd -<cr>
" nnoremap ˚ :3wincmd +<cr>
" nnoremap ¬ :3wincmd ><cr>

" nnoremap <silent><space>wh <C-w>h
" nnoremap <silent><space>wj <C-w>j
" nnoremap <silent><space>wk <C-w>k
" nnoremap <silent><space>wl <C-w>l

nnoremap j gj
nnoremap k gk

" Hard Mode (Anti-Pattern)
" tips: '+' and '-' move lines, or 'gj' and 'gk' 2021-03-09
nnoremap hh <nop>
nnoremap jj <nop>
nnoremap kk <nop>
nnoremap ll <nop>
""
" nnoremap K ggVGD
" nnoremap K ggVGp
" nnoremap K ggVGDI
" https://www.reddit.com/r/vim/comments/ilp32r/deleting_a_line_without_copying_it/
" not save to clipboard. instead, use register '_'
nnoremap K ggVG"_d
"
"}}}

" --------
"  Insert
" --------
"{{{
"
" undo break points
inoremap ,       ,<c-g>u
inoremap .       .<c-g>u
inoremap !       !<c-g>u
inoremap ?       ?<c-g>u
inoremap [       [<c-g>u
inoremap {       {<c-g>u
inoremap (       (<c-g>u
" inoremap <space> <space><c-g>u
inoremap <cr>    <cr><c-g>u
"
"
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
" https://superuser.com/questions/846854/in-vim-how-do-you-delete-to-end-of-line-while-in-command-mode
cnoremap <C-k> <C-\>estrpart(getcmdline(),0,getcmdpos()-1)<CR>
cnoremap <M-b> <S-Left>
cnoremap <M-f> <S-Right>
cnoremap <M-d> <S-Right><Delete>
" delay esc storke
" cnoremap <Esc>b <S-Left>
" cnoremap <Esc>f <S-Right>
" cnoremap <Esc>d <S-Right><Delete>
cnoremap <C-g> <C-c>
"}}}


" conflicted with commantary.vim
" == Capitalization 2021-04-24 ==
" https://github.com/shanestillwell/dotfiles/blob/master/vimrc
" https://vim.fandom.com/wiki/Capitalize_words_and_regions_easily
" gcw        - capitalize word (from cursor position to end of word)
" gcW        - capitalize WORD (from cursor position to end of WORD)
" gciw       - capitalize inner word (from start to end)
" gciW       - capitalize inner WORD (from start to end)
" gcis       - capitalize inner sentence
" gc$        - capitalize until end of line (from cursor postition)
" gcgc       - capitalize whole line (from start to end)
" gcc        - capitalize whole line
" {Visual}gc - capitalize highlighted text
if (&tildeop)
  nmap gCw guw~l
  nmap gCW guW~l
  nmap gCiw guiw~l
  nmap gCiW guiW~l
  nmap gCis guis~l
  nmap gC$ gu$~l
  nmap gCgc guu~l
  nmap gCC guu~l
  vmap gC gu~l
else
  nmap gCw guw~h
  nmap gCW guW~h
  nmap gCiw guiw~h
  nmap gCiW guiW~h
  nmap gCis guis~h
  nmap gC$ gu$~h
  nmap gCgc guu~h
  nmap gCC guu~h
  vmap gC gu~h
endif


" Search for (visual)selected text, forwards or backwards.
" https://vim.fandom.com/wiki/Search_for_visually_selected_text
vnoremap <silent> * :<C-U>
  \let old_reg=getreg('"')<Bar>let old_regtype=getregtype('"')<CR>
  \gvy/<C-R>=&ic?'\c':'\C'<CR><C-R><C-R>=substitute(
  \escape(@", '/\.*$^~['), '\_s\+', '\\_s\\+', 'g')<CR><CR>
  \gVzv:call setreg('"', old_reg, old_regtype)<CR>
vnoremap <silent> # :<C-U>
  \let old_reg=getreg('"')<Bar>let old_regtype=getregtype('"')<CR>
  \gvy?<C-R>=&ic?'\c':'\C'<CR><C-R><C-R>=substitute(
  \escape(@", '?\.*$^~['), '\_s\+', '\\_s\\+', 'g')<CR><CR>
  \gVzv:call setreg('"', old_reg, old_regtype)<CR>


"===========
" FUNCTIONS
"===========

" follow-mode like emacs 2021-08-04
function! FollowMode()
    let g:CurrentLineNumber=line(".")
    if exists('g:OnFollowing')
        unlet g:OnFollowing
        :TagbarClose
        windo set noscrollbind
        windo set wrap
        windo set rnu
        wincmd o  " only window
        norm zz
    else
        let g:OnFollowing=1
        execute "TagbarClose"
        wincmd o  " only window
        norm gg
        setlocal foldlevel=99  " unfold all
        windo set noscrollbind
        windo set nornu
        vsplit
        vsplit
        " 1/3
        " wincmd H  " move far left to choose first window
        wincmd h  " navigate to left
        wincmd h  " navigate to left
        " 2/3
        wincmd l  " navigate to right
        execute "normal \<C-f>"
        " 3/3
        wincmd l  " navigate to right
        :exe "normal \<C-f>"
        execute "normal \<PageDown>"
        " whole
        windo set scrollbind
        windo set nowrap
        autocmd InsertLeave * set nornu | set nocursorline " | set nocursorcolumn
        call EasyMode()
        wincmd h
        wincmd h
    endif
    execute "normal!" . (g:CurrentLineNumber) . "gg"
    norm zz
endfunction
command! FollowMode call FollowMode()
nnoremap <silent> <leader>F : FollowMode<cr>
" -------------------
" all the same belows
" -------------------
" execute "normal \<C-f>"
" :exe "normal \<C-f>"
" execute "normal \<PageDown>"


"-----------
" Draw Line
"-----------

"{{{
function! UnderLine()
    normal yy
    normal p
    s/\S/-/g
    noh
    echo "underlined"
endfunction
command! UnderLine call UnderLine()
"}}}

"{{{
function! DoubleLine()
    normal yy
    normal p
    s/\S/=/g
    noh
endfunction
command! DoubleLine call DoubleLine()
"}}}

"{{{
function! SurroundLine()
    normal yy
    normal p
    s/\S/-/g
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
    " set noignorecase
    " set nosmartcase
endfunction
command! HardMode call HardMode()
nnoremap <silent> <leader>h : HardMode<cr>
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
    silent! noremap <up>    <nop>
    silent! noremap <down>  <nop>
    silent! noremap <left>  <nop>
    silent! noremap <right> <nop>
    Limelight!
    set scrolloff=0
    set ignorecase
    set smartcase
endfunction
command! EasyMode call EasyMode()
nnoremap <silent> <leader>e : EasyMode<cr>
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
nnoremap <silent> <leader>s : SuperEasyMode<cr>
"}}}


"----------
" Focusing
"----------

function! FocusMode()
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
    Goyo 100
    Limelight
    autocmd InsertLeave * :set norelativenumber | hi CursorLine gui=NONE
    set scrolloff=999  " centering
    " set sidescrolloff=30
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
command! FocusMode call FocusMode()
" nnoremap <silent> <leader>S :FocusMode<cr>


function! DarkFocusMode()
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
    Goyo 100
    autocmd InsertLeave * :set norelativenumber
    set scrolloff=999  " centering
    " set sidescrolloff=30
    set ignorecase
    set smartcase
    " call EasyMode()
    call SuperEasyMode()
    " set variable
    let g:OnFocusing=1
    if has('gui_running')
        " set guifont=Menlo-Regular:h20
        " set lines=99 columns=999   " to maximize window size"
        colorscheme dark
        Limelight
        set cursorline!
    elseif exists('$TMUX')
        silent !tmux set status off
    endif
endfunction
" command! DarkFocusMode call DarkFocusMode()
" nnoremap <silent> <leader>D :DarkFocusMode<cr>
command! DistractionFreeMode call DarkFocusMode()
nnoremap <silent> <leader>D :DistractionFreeMode<cr>

function! EditMode()"
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
    set nu
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
    setlocal foldlevel=99
endfunction
command! EditMode call EditMode()"
nnoremap <silent> <leader>E :EditMode<cr>


function! UnFocusMode()
"{{{
    Goyo!
    Limelight!
    silent! unlet g:OnFocusing
    silent! unlet g:OnEditing
    autocmd InsertLeave * :set relativenumber
    set scrolloff=0
    set sidescrolloff=0
    " set noignorecase
    " set nosmartcase
    setlocal foldlevel=99  " unfold all
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
nnoremap <silent> <leader>U :UnFocusMode<cr>


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

" https://superuser.com/questions/198981/show-gvim-scrollbar-only-when-needed
" scrollbar when only needed
" au VimEnter * if line('$') > &lines | set go+=r | else | set go-=r | endif
" au VimResized * if line('$') > &lines | set go+=r | else | set go-=r | endif

if has('gui_running')"{{{
    set guioptions=     " disabled mac style tab"
    " set guioptions+=c   " to confirm exit
    " set guifont=Meslo\ LG\ S\ Regular\ for\ Powerline:h14
    set guifont=Meslo\ LG\ S\ Regular\ Nerd\ Font\ Complete\ Mono:h18
    " set guifont=MesloLGS\ Nerd\ Font:h14
    set lines=999 columns=9999  " full size windows 2021-04-21
    " cd ~/Documents/nvALT/
    set linespace=1
    " set colorcolumn=105
    " set nocursorcolumn
    set background=light
    " colorscheme basic-light
    colorscheme default
    " colorscheme github
    " highlight LineNr guibg=white
    highlight SignColumn guibg='white'       " for gitgutter
    highlight FoldColumn guibg='white'       " for foldcolumn
    " highlight Visual guifg=black guibg=Cyan gui=NONE
    " highlight CursorLineNr guibg=black guifg=white
    " highlight CursorLine gui=underline guibg=NONE
    hi TabLine gui=NONE guibg=black guifg=gray  " Deactive Area
    hi TabLineSel gui=NONE guibg=black guifg=white
    " hi TabLineFill gui=NONE guibg=black guifg=white  " Backgroud Aera
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
" let mapleader = ' '
" nnoremap <leader>; :

" delete all contents for whole ine
" nnoremap <leader>d :normal ggVGD<cr>i

"---------------
" Focusing Mode
"---------------
" nnoremap <silent> <leader>f :FocusToggle<cr>
" nnoremap <silent> <leader>r :FocusMode<cr>
" nnoremap <silent> <leader>d :DarkFocusMode<cr>
" nnoremap <silent> <leader>e :EditMode<cr>
" nnoremap <silent> <leader>q :UnFocusMode<cr>

"------------
" Utilities
"------------
nnoremap <silent> <leader>p  :PlantumlOpen<cr>
nnoremap <silent> <leader>r  :Ranger<cr>
nnoremap <silent> <leader>n  :NERDTreeToggle<cr>

" :TagbarOpen f -> jump opening / j -> jump if opened already
" https://github.com/preservim/tagbar/blob/master/doc/tagbar.txt
" https://vi.stackexchange.com/questions/3885/how-to-map-two-commands-with-only-one-key
nnoremap <silent> <leader>t  :TagbarToggle<cr> :wincmd =<cr>
nnoremap <silent> <leader>T  :TagbarOpen f<cr> :set noscrollbind<cr> :wincmd =<cr>

nnoremap <silent> <leader>m  :MaximizerToggle<cr>
" nnoremap <silent> <leader>x  :MaximizerToggle<cr>  " risky in x deleting
nnoremap <silent> <leader>b  :bro old<cr>
" maximize window size -> insted, vanila Vim: c-w _ / c-w
" nnoremap <silent> <leader>mw  :vert resize 999<cr>
" nnoremap <silent> <leader>mh  :resize 999<cr>

" nnoremap <silent> <leader><leader>r :source $MYVIMRC<cr>
    \ :echo " .vimrc reloaded "<cr>
" undo leader shortcuts "
nnoremap <silent> <leader>uu  :UndotreeToggle<cr>
nnoremap <silent> <leader>uf  :UndotreeFocus<cr>
nnoremap <silent> <leader>us  :UndotreeShow<cr>
nnoremap <silent> <leader>uh  :UndotreeHide<cr>
" markdown support"
vnoremap <silent> <leader>* s**<C-r>"**<esc>
" Others
" nnoremap <silent> <leader>s  :Startify<cr>

"------------
" EasyMotion
"------------
" <Leader>f{char} to move to {char}
map  <Leader>f <Plug>(easymotion-bd-f)
nmap <Leader>f <Plug>(easymotion-overwin-f)
" s{char}{char} to move to {char}{char}
" nmap <leader>s <Plug>(easymotion-overwin-f2)
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
ab ttt testtesttest
iab xorg -*- mode: org; -*-
iab xti #+TITLE:
iab xftp ftp://osickwon@ftp.osickwon.heliohost.org/public_html/
iab bp breakpoint()
iab tt time.sleep(1)
iab ip from IPython import embed; embed()
" run python with current buffer
cab rp !clear; python3 %


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


" -------
" Python
" -------
" Finally, make your code look pretty:
" https://realpython.com/vim-and-python-a-match-made-in-heaven/
let python_highlight_all=1


"----------------------
" File Type Setting
"----------------------
autocmd BufEnter *.sh    colorscheme PaperColor | set background=dark | set nospell
autocmd BufEnter *.py    colorscheme PaperColor | set background=dark | set nospell
autocmd BufEnter *.vimrc colorscheme PaperColor | set background=dark

autocmd BufEnter *.php   colorscheme purify | set background=dark
autocmd BufEnter *.html  colorscheme purify | set background=dark
autocmd BufEnter *.phtml colorscheme purify | set background=dark

" autocmd BufEnter *.json, json set conceallevel=0

autocmd FileType html,xml,py,lisp,js,org,dat,csv set nospell


"----------------
" Korean / Hangul
"----------------
" https://johngrib.github.io/blog/2017/05/04/input-source/
" 2021-06-09
" set langmap=ㅁa,ㅠb,ㅊc,ㅇd,ㄷe,ㄹf,ㅎg,ㅗh,ㅑi,ㅓj,ㅏk,ㅣl,ㅡm,ㅜn,ㅐo,ㅔp,ㅂq,ㄱr,ㄴs,ㅅt,ㅕu,ㅍv,ㅈw,ㅌx,ㅛy,ㅋz


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
