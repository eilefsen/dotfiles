local lspconfig = require('lspconfig')

-- disable pyright hints
local capabilities = (function()
    local capabilities = vim.lsp.protocol.make_client_capabilities()
    capabilities.textDocument.publishDiagnostics.tagSupport.valueSet = { 2 }
    return capabilities
end)()

-- configure pyright
lspconfig.pyright.setup({
    capabilities = capabilities,

    on_attach = function(client, _)
        client.server_capabilities.codeActionProvider = false
    end,

    settings = {
        pyright = {
            disableOrganizeImports = true,
            autoImportCompletions = true,
            autoSearchPaths = true,
        },
        python = {
            analysis = {
                strictParameterNoneValue = false,
                typeCheckingMode = "basic",
                diagnosticMode = "workspace",
                diagnosticSeverityOverrides = {
                    ------------------------------------ typeCheckingMode | off:basic:strict
                    -- analyzeUnannotatedFunctions = true,              -- true:true:true
                    -- strictParameterNoneValue = true,                 -- true:true:true
                    -- enableTypeIgnoreComments = true,                 -- true:true:true
                    -- disableBytesTypePromotions = false,              -- false:false:true
                    -- strictListInference = false,                     -- false:false:true
                    -- strictDictionaryInference = false,               -- false:false:true
                    -- strictSetInference = false,                      -- false:false:true
                    -- deprecateTypingAliases = false,                  -- false:false:false
                    -- enableExperimentalFeatures = false,              -- false:false:false
                    reportMissingModuleSource = "none",	                -- "warning":"warning":"warning"
                    reportMissingImports = "none", 	                    -- "warning":"error":"error"
                    reportUndefinedVariable = "none",                   -- "warning":"error":"error"
                    reportAssertAlwaysTrue = "none",                    -- "none":"warning":"error"
                    reportInvalidStringEscapeSequence = "none",         -- "none":"warning":"error"
                    -- reportInvalidTypeVarUse = "warning"              -- "none":"warning":"error"
                    -- reportMissingTypeStubs = "warning"               -- "none":"warning":"error"
                    reportSelfClsParameterName = "none",                -- "none":"warning":"error"
                    reportUnsupportedDunderAll = "none",                -- "none":"warning":"error"
                    reportUnusedExpression ="none",                     -- "none":"warning":"error"
                    reportWildcardImportFromLibrary = "none",           -- "none":"warning":"error"
                    -- reportGeneralTypeIssues = "error",               -- "none":"error":"error"
                    -- reportOptionalSubscript = "error",               -- "none":"error":"error"
                    -- reportOptionalMemberAccess = "error",            -- "none":"error":"error"
                    -- reportOptionalCall =	"error",                    -- "none":"error":"error"
                    -- reportOptionalIterable =	"error",                -- "none":"error":"error"
                    -- reportOptionalContextManager = "error",          -- "none":"error":"error"
                    -- reportOptionalOperand = "error",                 -- "none":"error":"error"
                    -- reportTypedDictNotRequiredAccess = "error",      -- "none":"error":"error"
                    -- reportPrivateImportUsage = "error",	            -- "none":"error":"error"
                    reportUnboundVariable = "none",	                    -- "none":"error":"error"
                    reportUnusedCoroutine = "none",	                    -- "none":"error":"error"
                    -- reportConstantRedefinition = "none",	            -- "none":"none":"error"
                    -- reportDeprecated = "none",	                    -- "none":"none":"error"
                    -- reportDuplicateImport = "none",	                -- "none":"none":"error"
                    -- reportFunctionMemberAccess = "none",	            -- "none":"none":"error"
                    -- reportIncompatibleMethodOverride = "none",       -- "none":"none":"error"
                    -- reportIncompatibleVariableOverride = "none",     -- "none":"none":"error"
                    -- reportIncompleteStub = "none",	                -- "none":"none":"error"
                    -- reportInconsistentConstructor = "none",	        -- "none":"none":"error"
                    -- reportInvalidStubStatement = "none",	            -- "none":"none":"error"
                    -- reportMatchNotExhaustive = "none",	            -- "none":"none":"error"
                    -- reportMissingParameterType = "none",	            -- "none":"none":"error"
                    -- reportMissingTypeArgument = "none",	            -- "none":"none":"error"
                    -- reportOverlappingOverload = "none",	            -- "none":"none":"error"
                    -- reportPrivateUsage = "none",	                    -- "none":"none":"error"
                    -- reportTypeCommentUsage = "none",	                -- "none":"none":"error"
                    -- reportUnknownArgumentType = "none",	            -- "none":"none":"error"
                    -- reportUnknownLambdaType = "none",	            -- "none":"none":"error"
                    -- reportUnknownMemberType = "none",                -- "none":"none":"error"
                    -- reportUnknownParameterType = "none",	            -- "none":"none":"error"
                    -- reportUnknownVariableType = "none",	            -- "none":"none":"error"
                    -- reportUnnecessaryCast = "none",	                -- "none":"none":"error"
                    -- reportUnnecessaryComparison = "none",	        -- "none":"none":"error"
                    -- reportUnnecessaryContains = "none",	            -- "none":"none":"error"
                    -- reportUnnecessaryIsInstance = "none",	        -- "none":"none":"error"
                    -- reportUnusedClass = "none",	                    -- "none":"none":"error"
                    -- reportUnusedImport = "none",	                    -- "none":"none":"error"
                    -- reportUnusedFunction = "none",	                -- "none":"none":"error"
                    -- reportUnusedVariable = "none",	                -- "none":"none":"error"
                    -- reportUntypedBaseClass = "none",	                -- "none":"none":"error"
                    -- reportUntypedClassDecorator = "none",	        -- "none":"none":"error"
                    -- reportUntypedFunctionDecorator = "none",	        -- "none":"none":"error"
                    -- reportUntypedNamedTuple = "none",	            -- "none":"none":"error"
                    -- reportCallInDefaultInitializer = "none",	        -- "none":"none":"none"
                    -- reportImplicitOverride = "none",	                -- "none":"none":"none"
                    -- reportImplicitStringConcatenation = "none",      -- "none":"none":"none"
                    -- reportImportCycles = "none",	                    -- "none":"none":"none"
                    -- reportMissingSuperCall = "none",	                -- "none":"none":"none"
                    -- reportPropertyTypeMismatch = "none",	            -- "none":"none":"none"
                    -- reportShadowedImports = "none",	                -- "none":none":"none"
                    -- reportUninitializedInstanceVariable = "none",    -- "none":"none":"none"
                    -- reportUnnecessaryTypeIgnoreComment= "none",      -- "none":"none":"none"
                    -- reportUnusedCallResult= "none", 	                -- "none":"none":"none"
                },
            },
        },
    },
})
