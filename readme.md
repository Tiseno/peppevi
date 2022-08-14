# Peppevi
An LR parser and formatter for a small language, implemented to learn about rust.

## Usage
The only dependency is rustc.
Nothing useful is implemented yet, only lexing, parsing, and formatting.

```
make
./pvic
```

## Vim Neomake and ALE examples
For file type detection, edit `~/.config/nvim/filetype.vim`:
```vim
if exists("did_load_filetypes")
  finish
endif
augroup filetypedetect
  au! BufRead,BufNewFile *.pvi     setfiletype pvi
augroup END
```

For neomake:
```vim
let g:neomake_pvi_enabled_makers = ['pvi']

let g:neomake_pvi_pvi_maker = {
    \ 'exe': '/home/tiseno/git/peppevi/pvic',
    \ 'args': [],
    \ 'errorformat': '%f:%l-%c:%e-%k: %trror: %m',
    \ }
```

As an ALE fixer:
```vim
function! FormatPvi(buffer) abort
  return {
  \   'command': '/home/tiseno/git/peppevi/pvic --stdin-format'
  \}
endfunction

execute ale#fix#registry#Add('pvifmt', 'FormatPvi', ['pvi'], 'Peppevi format for pvi files')

let g:ale_fixers = {
      \ 'pvi': ['pvifmt']
      \ }
```

