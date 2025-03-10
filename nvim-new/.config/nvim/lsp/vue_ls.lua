return {
	cmd = { 'vue-language-server', '--stdio' },
	filetypes = {
		'vue',
	},
	root_markers = {'package.json'},
	init_options = {
		typescript = {
			tsdk = '/usr/local/lib/node_modules/typescript/lib'
		},
	},
}
-- vim:foldmethod=marker:foldlevel=0:filetype=nvimlua
