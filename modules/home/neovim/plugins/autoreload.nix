{ ... }:

{
  # Reload file when it has changed
  programs.neovim.extraLuaConfig = ''
    vim.opt.autoread = true
    vim.api.nvim_create_autocmd({ 'VimEnter', 'FocusGained', 'BufEnter' }, {
        group = vim.api.nvim_create_augroup('ReloadFileOnChange', {}),
        command = 'checktime',
    })
  '';
}
