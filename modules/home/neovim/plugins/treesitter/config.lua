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
}
