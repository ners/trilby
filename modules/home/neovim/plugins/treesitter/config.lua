local cfg = require('nvim-treesitter.configs')
cfg.setup {
    highlight = {
        enable = true,
        additional_vim_regex_highlighting = false,
        -- disable TS for large buffers because it is slow
        disable = function(_, buf)
            local max_filesize = 100 * 1024 -- 100 KiB
            local ok, stats = pcall(vim.loop.fs_stat, vim.api.nvim_buf_get_name(buf))
            if ok and stats and stats.size > max_filesize then
                return true
            end
        end
    },
    rainbow = {
        enable = true,
        query = 'rainbow-parens',
        strategy = require('rainbow-delimiters').strategy['global'],
    },
    textobjects = {
        select = {
            enable = true,
            -- Automatically jump forward to textobj, similar to targets.vim
            lookahead = true,
            keymaps = {
                ['af'] = '@function.outer',
                ['if'] = '@function.inner',
                ['ac'] = '@class.outer',
                ['ic'] = '@class.inner',
                ['aC'] = '@call.outer',
                ['iC'] = '@call.inner',
                ['a#'] = '@comment.outer',
                ['i#'] = '@comment.outer',
                ['ai'] = '@conditional.outer',
                ['ii'] = '@conditional.outer',
                ['al'] = '@loop.outer',
                ['il'] = '@loop.inner',
                ['aP'] = '@parameter.outer',
                ['iP'] = '@parameter.inner',
            },
            selection_modes = {
                ['@parameter.outer'] = 'v', -- charwise
                ['@function.outer'] = 'V', -- linewise
                ['@class.outer'] = '<c-v>', -- blockwise
            },
        },
        swap = {
            enable = true,
            swap_next = {
                ['<leader>a'] = '@parameter.inner',
            },
            swap_previous = {
                ['<leader>A'] = '@parameter.inner',
            },
        },
        move = {
            enable = true,
            set_jumps = true, -- whether to set jumps in the jumplist
            goto_next_start = {
                [']m'] = '@function.outer',
                [']P'] = '@parameter.outer',
            },
            goto_next_end = {
                [']m'] = '@function.outer',
                [']P'] = '@parameter.outer',
            },
            goto_previous_start = {
                ['[m'] = '@function.outer',
                ['[P'] = '@parameter.outer',
            },
            goto_previous_end = {
                ['[m'] = '@function.outer',
                ['[P'] = '@parameter.outer',
            },
        },
        lsp_interop = {
            enable = true,
            peek_definition_code = {
                ['df'] = '@function.outer',
                ['dF'] = '@class.outer',
            },
        },
    },
    incremental_selection = {
        enable = true,
        keymaps = {
            init_selection = 'gnn',
            node_incremental = 'grn',
            scope_incremental = 'grc',
            node_decremental = 'grm',
        },
    },
    refactor = {
        highlight_current_scope = {
            enable = true,
        },
        highlight_definitions = {
            enable = false, -- replaced with vim-illuminate plugin
            -- Set to false if you have an `updatetime` of ~100.
            clear_on_cursor_move = true,
        },
        smart_rename = {
            enable = true,
            keymaps = {
                smart_rename = 'grr',
            },
        },
        navigation = {
            enable = true,
            keymaps = {
                goto_definition = 'gnd',
                list_definitions = 'gnD',
                list_definitions_toc = 'gO',
                goto_next_usage = '<a-*>',
                goto_previous_usage = '<a-#>',
            },
        },
    },
}
