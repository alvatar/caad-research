let SessionLoad = 1
if &cp | set nocp | endif
let s:cpo_save=&cpo
set cpo&vim
inoremap <F11> :call Compile(1)
map! <S-Insert> *
cnoremap <C-Space><C-Space> vert scscope find
cnoremap <C-Space> cscope find
noremap  :ScreenSend
nnoremap <NL> /<++.\{-1,}++>c//e
map K <Nop>
map Q gq
vmap [% [%m'gv``
nmap \ihn :IHN
nmap \is :IHS:A
nmap \ih :IHS
vmap <silent> \x <Plug>VisualTraditional
vmap <silent> \c <Plug>VisualTraditionalj
nmap <silent> \x <Plug>Traditional
nmap <silent> \c <Plug>Traditionalj
vmap ]% ]%m'gv``
vmap a% [%v]%
nmap gx <Plug>NetrwBrowseX
nnoremap <F3> :vimgrep // **
noremap <F9> :call RunProgram()
noremap <F10> :call CleanProgram()
noremap <F11> :call Compile(1)
nnoremap <F12> :call BuildCTagsAndCSCopeDatabase("d")
nnoremap <silent> <Plug>NetrwBrowseX :call netrw#NetrwBrowseX(expand("<cWORD>"),0)
noremap <Plug>VisualFirstLine :call EnhancedCommentify('', 'first',   line("'<"), line("'>"))
noremap <Plug>VisualTraditional :call EnhancedCommentify('', 'guess',   line("'<"), line("'>"))
noremap <Plug>VisualDeComment :call EnhancedCommentify('', 'decomment',   line("'<"), line("'>"))
noremap <Plug>VisualComment :call EnhancedCommentify('', 'comment',   line("'<"), line("'>"))
noremap <Plug>FirstLine :call EnhancedCommentify('', 'first')
noremap <Plug>Traditional :call EnhancedCommentify('', 'guess')
noremap <Plug>DeComment :call EnhancedCommentify('', 'decomment')
noremap <Plug>Comment :call EnhancedCommentify('', 'comment')
nmap <C-Space><C-Space>d :vert scs find d 
nmap <C-Space><C-Space>i :vert scs find i 
nmap <C-Space><C-Space>f :vert scs find f 
nmap <C-Space><C-Space>e :vert scs find e 
nmap <C-Space><C-Space>t :vert scs find t 
nmap <C-Space><C-Space>c :vert scs find c 
nmap <C-Space><C-Space>g :vert scs find g 
nmap <C-Space><C-Space>s :vert scs find s 
nmap <C-Space>d :cs find d 
nmap <C-Space>i :cs find i 
nmap <C-Space>f :cs find f 
nmap <C-Space>e :cs find e 
nmap <C-Space>t :cs find t 
nmap <C-Space>c :cs find c 
nmap <C-Space>g :cs find g 
nmap <C-Space>s :cs find s 
nnoremap <silent> <F8> :TlistToggle
nnoremap <F2><F2> :!opera 
nnoremap <F2> :!opera http://www.google.es/search?q=&ie=utf-8&oe=utf-8&aq=t
inoremap <NL> /<++.\{-1,}++>c//e
imap \ihn :IHN
imap \is :IHS:A
imap \ih :IHS
imap <silent> \x <Plug>Traditional
imap <silent> \c <Plug>Traditionalji
let &cpo=s:cpo_save
unlet s:cpo_save
set paste
set backspace=2
set errorformat=%*[^\"]\"%f\"%*\\D%l:\ %m,\"%f\"%*\\D%l:\ %m,%-G%f:%l:\ (Each\ undeclared\ identifier\ is\ reported\ only\ once,%-G%f:%l:\ for\ each\ function\ it\ appears\ in.),%f:%l:%c:%m,%f(%l):%m,%f:%l:%m,\"%f\"\\,\ line\ %l%*\\D%c%*[^\ ]\ %m,%D%*\\a[%*\\d]:\ Entering\ directory\ `%f',%X%*\\a[%*\\d]:\ Leaving\ directory\ `%f',%D%*\\a:\ Entering\ directory\ `%f',%X%*\\a:\ Leaving\ directory\ `%f',%DMaking\ %*\\a\ in\ %f,%f|%l|\ %m,%f(%l):\ %m
set expandtab
set fileencodings=ucs-bom,utf-8,default,latin1,default
set guifont=montecarlo
set guiheadroom=0
set guioptions=ac
set helplang=en
set hidden
set hlsearch
set iminsert=0
set imsearch=0
set makeprg=./build.sh
set mouse=a
set mousemodel=popup
set path=.,/usr/include,,,.**,/data/projects/ensanche-core/**
set sessionoptions=blank,buffers,curdir,globals,folds,help,localoptions,options,tabpages,winsize
set shiftwidth=2
set showcmd
set showmatch
set spelllang=es,en
set statusline=%F%m%r%h%w\ [FORMAT=%{&ff}]\ [TYPE=%Y]\ [ASCII=%03.3b]\ [HEX=%02.2B]\ [POS=%04l,%04v][%p%%]\ [LEN=%L]
set suffixes=.bak,~,.o,.h,.info,.swp,.obj,.info,.aux,.log,.dvi,.bbl,.out,.o,.lo
set tabstop=2
set termencoding=utf-8
set wildmenu
set window=29
let s:so_save = &so | let s:siso_save = &siso | set so=0 siso=0
let v:this_session=expand("<sfile>:p")
let CTags_CScope_Dir_List = "/data/projects/ensanche-core/.."
let ScreenShellSession = "tmpvjTRA9U0"
let Tlist_Max_Submenu_Items =  20 
let Tlist_Enable_Fold_Column =  1 
let ScreenShellQuitOnVimExit =  1 
let Tlist_Show_One_File =  1 
let Tlist_Auto_Highlight_Tag =  1 
let Tlist_Compact_Format =  0 
let Tlist_Sort_Type = "order"
let Tlist_Use_Horiz_Window =  0 
let EnhCommentifyAlignRight = "no"
let Tlist_Ctags_Cmd = "exuberant-ctags"
let EnhCommentifyCallbackExists = "Yes"
let Tlist_Process_File_Always =  0 
let Tlist_Use_SingleClick =  0 
let ScreenShellServerName = "vim"
let EnhCommentifyPretty = "yes"
let Tlist_Display_Tag_Scope =  1 
let TagList_title = "__Tag_List__"
let ScreenShellExternal =  0 
let ScreenImpl = "GnuScreen"
let EnhCommentifyMultiPartBlocks = "yes"
let ScreenShellOrientation = "horizontal"
let NetrwMenuPriority =  80 
let Tlist_Highlight_Tag_On_BufEnter =  1 
let ScreenShellTerminal = "urxvt"
let ScreenShellInitialFocus = "vim"
let ScreenShellTmuxInitArgs = ""
let Tlist_GainFocus_On_ToggleOpen =  1 
let Tlist_WinHeight =  10 
let EnhCommentifyTraditionalMode = "Yes"
let EnhCommentifyRespectIndent = "yes"
let Tlist_Inc_Winwidth =  1 
let Tlist_Auto_Update =  1 
let CTags_CScope_Top_Dir = "/data/projects/ensanche-core/__scripts"
let Tlist_Exit_OnlyWindow =  1 
let Tlist_Display_Prototype =  0 
let Tlist_Max_Tag_Length =  10 
let DidEnhancedCommentify =  1 
let Tlist_WinWidth =  30 
let ScreenShellWindow = "screenshell"
let Tlist_Close_On_Select =  1 
let ScreenShellGnuScreenVerticalSupport = ""
let Tlist_File_Fold_Auto_Close =  0 
let Tlist_Auto_Open =  0 
let ScreenShellWidth =  -1 
let NetrwTopLvlMenu = "Netrw."
let ScreenShellHeight =  15 
let Tlist_Show_Menu =  0 
let Tlist_Use_Right_Window =  0 
let Make_Dir = "/data/projects/ensanche-core/__scripts"
silent only
cd /data/projects/ensanche-core/__scripts
if expand('%') == '' && !&modified && line('$') <= 1 && getline(1) == ''
  let s:wipebuf = bufnr('%')
endif
set shortmess=aoO
badd +23 /data/projects/ensanche-core/main.scm
badd +15 /data/projects/ensanche-core/graph.scm
badd +10 /data/projects/ensanche-core/input.scm
badd +15 /data/projects/ensanche-core/visualization.scm
badd +10 /data/projects/ensanche-core/context.scm
badd +12 /data/projects/ensanche-core/operations.scm
badd +8 /data/projects/ensanche-core/filters.scm
badd +10 /data/projects/ensanche-core/output.scm
badd +12 /data/projects/ensanche-core/analysis.scm
badd +1 /data/projects/ensanche-core/validators.scm
badd +57 /data/projects/ensanche-core/generation.scm
badd +11 /data/projects/ensanche-core/generation-elements.scm
badd +9 /data/projects/ensanche-core/fields/light.scm
badd +9 /data/projects/ensanche-core/fields/structure.scm
badd +8 /data/projects/ensanche-core/fields/pipes.scm
badd +10 /data/projects/ensanche-core/graph-visualization.scm
badd +8 /data/projects/ensanche-core/fields/entries.scm
badd +15 /data/projects/ensanche-core/fields-2d.scm
badd +16 /data/projects/ensanche-core/evolution.scm
badd +15 /data/projects/ensanche-core/selection.scm
badd +11 /data/projects/ensanche-core/strategies/hinted-brownian-agents.scm
badd +8 /data/projects/ensanche-core/context-tree.scm
badd +12 /data/projects/ensanche-core/auxiliary-operations.scm
badd +108 /data/projects/ensanche-core/geometry/kernel.scm
badd +43 /data/projects/ensanche-core/math/algebra.scm
badd +12 /data/projects/ensanche-core/ds/data-conversions.scm
badd +59 /data/projects/ensanche-core/core/debug.scm
badd +44 /data/projects/ensanche-core/core/functional.scm
badd +1 /data/projects/ensanche-core/core/list.scm
badd +23 /data/projects/ensanche-core/core/syntax.scm
silent! argdel *
edit /data/projects/ensanche-core/geometry/kernel.scm
set splitbelow splitright
wincmd _ | wincmd |
vsplit
1wincmd h
wincmd w
set nosplitbelow
set nosplitright
wincmd t
set winheight=1 winwidth=1
exe 'vert 1resize ' . ((&columns * 139 + 139) / 279)
exe 'vert 2resize ' . ((&columns * 139 + 139) / 279)
argglobal
setlocal keymap=
setlocal noarabic
setlocal autoindent
setlocal balloonexpr=
setlocal nobinary
setlocal bufhidden=
setlocal buflisted
setlocal buftype=
setlocal nocindent
setlocal cinkeys=0{,0},0),:,0#,!^F,o,O,e
setlocal cinoptions=
setlocal cinwords=if,else,while,do,for,switch
setlocal comments=
setlocal commentstring=/*%s*/
setlocal complete=.,w,b,u,t,i
setlocal completefunc=
setlocal nocopyindent
setlocal nocursorcolumn
set cursorline
setlocal cursorline
setlocal define=^\\s*(def\\k*
setlocal dictionary=
setlocal nodiff
setlocal equalprg=
setlocal errorformat=
setlocal expandtab
if &filetype != 'scheme'
setlocal filetype=scheme
endif
setlocal foldcolumn=0
set nofoldenable
setlocal nofoldenable
setlocal foldexpr=0
setlocal foldignore=#
setlocal foldlevel=0
setlocal foldmarker={{{,}}}
set foldmethod=syntax
setlocal foldmethod=syntax
setlocal foldminlines=1
setlocal foldnestmax=20
setlocal foldtext=foldtext()
setlocal formatexpr=
setlocal formatoptions=nroql2
setlocal formatlistpat=^\\s*\\d\\+[\\]:.)}\\t\ ]\\s*
setlocal grepprg=
setlocal iminsert=0
setlocal imsearch=0
setlocal include=
setlocal includeexpr=
setlocal indentexpr=
setlocal indentkeys=0{,0},:,0#,!^F,o,O,e
setlocal noinfercase
setlocal iskeyword=33,35-39,42-58,60-90,94,95,97-122,126,_
setlocal keywordprg=
setlocal nolinebreak
setlocal lisp
setlocal nolist
setlocal makeprg=
setlocal matchpairs=(:),{:},[:]
setlocal modeline
setlocal modifiable
setlocal nrformats=octal,hex
set number
setlocal number
setlocal numberwidth=4
setlocal omnifunc=
setlocal path=
setlocal nopreserveindent
setlocal nopreviewwindow
setlocal quoteescape=\\
setlocal noreadonly
setlocal norightleft
setlocal rightleftcmd=search
setlocal noscrollbind
setlocal shiftwidth=2
setlocal noshortname
setlocal nosmartindent
setlocal softtabstop=0
setlocal nospell
setlocal spellcapcheck=[.?!]\\_[\\])'\"\	\ ]\\+
setlocal spellfile=
setlocal spelllang=es,en
setlocal statusline=
setlocal suffixesadd=
setlocal swapfile
setlocal synmaxcol=3000
if &syntax != 'scheme'
setlocal syntax=scheme
endif
setlocal tabstop=2
setlocal tags=
setlocal textwidth=0
setlocal thesaurus=
setlocal nowinfixheight
setlocal nowinfixwidth
setlocal wrap
setlocal wrapmargin=0
let s:l = 24 - ((23 * winheight(0) + 46) / 92)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
24
normal! 0
wincmd w
argglobal
edit /data/projects/ensanche-core/strategies/hinted-brownian-agents.scm
setlocal keymap=
setlocal noarabic
setlocal autoindent
setlocal balloonexpr=
setlocal nobinary
setlocal bufhidden=
setlocal buflisted
setlocal buftype=
setlocal nocindent
setlocal cinkeys=0{,0},0),:,0#,!^F,o,O,e
setlocal cinoptions=
setlocal cinwords=if,else,while,do,for,switch
setlocal comments=:;;;,:;;,sr:#|,mb:|,ex:|#,:;
setlocal commentstring=/*%s*/
setlocal complete=.,w,b,u,t,i
setlocal completefunc=
setlocal nocopyindent
setlocal nocursorcolumn
set cursorline
setlocal cursorline
setlocal define=^\\s*(def\\k*
setlocal dictionary=
setlocal nodiff
setlocal equalprg=
setlocal errorformat=
setlocal expandtab
if &filetype != 'scheme'
setlocal filetype=scheme
endif
setlocal foldcolumn=0
set nofoldenable
setlocal nofoldenable
setlocal foldexpr=0
setlocal foldignore=#
setlocal foldlevel=0
setlocal foldmarker={{{,}}}
set foldmethod=syntax
setlocal foldmethod=syntax
setlocal foldminlines=1
setlocal foldnestmax=20
setlocal foldtext=foldtext()
setlocal formatexpr=
setlocal formatoptions=croql
setlocal formatlistpat=^\\s*\\d\\+[\\]:.)}\\t\ ]\\s*
setlocal grepprg=
setlocal iminsert=0
setlocal imsearch=0
setlocal include=
setlocal includeexpr=
setlocal indentexpr=
setlocal indentkeys=0{,0},:,0#,!^F,o,O,e
setlocal noinfercase
setlocal iskeyword=33,35-39,42-58,60-90,94,95,97-122,126,_
setlocal keywordprg=
setlocal nolinebreak
setlocal lisp
setlocal nolist
setlocal makeprg=
setlocal matchpairs=(:),{:},[:]
setlocal modeline
setlocal modifiable
setlocal nrformats=octal,hex
set number
setlocal number
setlocal numberwidth=4
setlocal omnifunc=
setlocal path=
setlocal nopreserveindent
setlocal nopreviewwindow
setlocal quoteescape=\\
setlocal noreadonly
setlocal norightleft
setlocal rightleftcmd=search
setlocal noscrollbind
setlocal shiftwidth=2
setlocal noshortname
setlocal nosmartindent
setlocal softtabstop=0
setlocal nospell
setlocal spellcapcheck=[.?!]\\_[\\])'\"\	\ ]\\+
setlocal spellfile=
setlocal spelllang=es,en
setlocal statusline=
setlocal suffixesadd=
setlocal swapfile
setlocal synmaxcol=3000
if &syntax != 'scheme'
setlocal syntax=scheme
endif
setlocal tabstop=2
setlocal tags=
setlocal textwidth=0
setlocal thesaurus=
setlocal nowinfixheight
setlocal nowinfixwidth
setlocal wrap
setlocal wrapmargin=0
let s:l = 475 - ((45 * winheight(0) + 46) / 92)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
475
normal! 033l
wincmd w
2wincmd w
exe 'vert 1resize ' . ((&columns * 139 + 139) / 279)
exe 'vert 2resize ' . ((&columns * 139 + 139) / 279)
tabnext 1
if exists('s:wipebuf')
  silent exe 'bwipe ' . s:wipebuf
endif
unlet! s:wipebuf
set winheight=1 winwidth=20 shortmess=filnxtToO
let s:sx = expand("<sfile>:p:r")."x.vim"
if file_readable(s:sx)
  exe "source " . fnameescape(s:sx)
endif
let &so = s:so_save | let &siso = s:siso_save
doautoall SessionLoadPost
unlet SessionLoad
" vim: set ft=vim :
