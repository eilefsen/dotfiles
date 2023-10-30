require('lspconfig').clangd.setup {
    cmd = {
        "clangd",
        "--inlay-hints=true",
    },
}
