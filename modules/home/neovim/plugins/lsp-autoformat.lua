vim.api.nvim_create_autocmd("LspAttach", {
	group = vim.api.nvim_create_augroup("LspAttach_autoformat", {}),
	callback = function(args)
		if not (args.data and args.data.client_id) then
			return
		end

		local buffer = args.buf
		local client = vim.lsp.get_client_by_id(args.data.client_id)

		if client.server_capabilities.documentFormattingProvider then
			-- Format the file before it is written
			vim.api.nvim_create_autocmd('BufWritePre', {
				group = vim.api.nvim_create_augroup('FormatOnWrite', {}),
				callback = function() vim.lsp.buf.format(nil, 1000) end,
				buffer = buffer,
			})
		end
	end,
})
