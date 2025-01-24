local opt = vim.opt
local g = vim.g
local fn = vim.fn

-- load .exrc, .nvimrc and .nvim.lua local files
vim.o.exrc = true

-- Use space as leader key
g.mapleader = ' '

-- Set mouse mode to all modes
opt.mouse = 'a'

-- The encoding displayed
opt.encoding = "utf-8"

-- The encoding written to file
opt.fileencoding = "utf-8"

-- Use the system clipboard
opt.clipboard = "unnamedplus"

opt.updatetime = 100

-- Enable persistent undo, backup, and swap files
opt.undofile = true
opt.undodir = vim.fn.stdpath("state") .. "/undo/"

opt.backup = true
opt.backupdir = vim.fn.stdpath("state") .. "/backup/"

opt.swapfile = true
opt.directory = vim.fn.stdpath("state") .. "/swap/"

-- Backspace works on every char in insert mode
opt.backspace = "indent,eol,start"

-- Enable background buffer
vim.o.hidden = true

-- Show line numbers
opt.number = true

-- Share the sign column with the number column to prevent text flicker
--opt.signcolumn = 'number'
opt.signcolumn = 'yes'

-- Display command in bottom bar
opt.showcmd = true

-- Tab control
opt.smarttab = true -- tab respects 'tabstop', 'shiftwidth', and 'softtabstop'
opt.tabstop = 4 -- the visible width of tabs
opt.softtabstop = 4 -- edit as if the tabs are 4 characters wide
opt.shiftwidth = 4 -- number of spaces to use for indent and unindent
opt.shiftround = true -- round indent to a multiple of 'shiftwidth'

-- Use soft tabs, hard tabs can be enabled on filetype basis
opt.expandtab = true

-- Autoindent
opt.autoindent = true
opt.smartindent = true

-- Search
opt.ignorecase = true -- case insensitive searching
opt.smartcase = true -- case-sensitive if expresson contains a capital letter
opt.hlsearch = true -- highlight search results
opt.incsearch = true -- set incremental search, like modern browsers
opt.lazyredraw = false -- don't redraw while executing macros
opt.magic = true -- set magic on, for regular expressions

-- Don't redraw while executing macros
opt.ttyfast = true
opt.lazyredraw = true

-- If ripgrep installed, use that as a grepper
if fn.executable("rg") then
    opt.grepprg = "rg --vimgrep --no-heading"
    opt.grepformat = "%f:%l:%c:%m,%f:%l:%m"
end

-- Show matching brackets
vim.o.showmatch = true

-- Split direction
opt.splitright = true
opt.splitbelow = true

-- Set completeopt to have a better completion experience
-- :help completeopt
-- menuone: popup even when there's only one match
-- noinsert: Do not insert text until a selection is made
-- noselect: Do not select, force user to select one from the menu
opt.completeopt = { 'menuone', 'noselect', 'noinsert' }

-- Prevent strange file save behaviour.
-- https://github.com/srid/emanote/issues/180
opt.backupcopy = 'yes'

-- Change decorative characters
vim.opt.fillchars = {
  horiz = '━',
  horizup = '┻',
  horizdown = '┳',
  vert = '┃',
  vertleft  = '┫',
  vertright = '┣',
  verthoriz = '╋',
  fold = ' ',
  eob = ' ', -- prevent '~' from showing on blank lines
  msgsep = '‾'
}

-- autoload files on buffer enter
vim.opt.autoread = true
vim.api.nvim_create_autocmd({ 'VimEnter', 'FocusGained', 'BufEnter' }, {
    group = vim.api.nvim_create_augroup('ReloadFileOnChange', {}),
    command = 'checktime',
})
