execute pathogen#infect()
call togglebg#map("<F5>")
set nu
set ruler
set nocompatible
filetype plugin on
filetype on
syntax on
set ts=2
"let g:solarized_termcolors=256
colorscheme solarized

let g:slimv_lisp = '/opt/local/bin/lisp'
let g:slimv_impl = 'cmu'
let g:slimv_swank_cmd = '!osascript -e "tell application \"Terminal\" to do script \"lisp --load ~/.vim/slime/start-swank.lisp\""'

set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0
