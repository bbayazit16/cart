use cart::context::{FileContext, Span};
use cart::errors::CompileError;
use cart::hir::{Program, TypeChecker};
use cart::parser::Parser;
use cart::reporter::Reporter;
use std::collections::HashMap;
use std::sync::Arc;
use std::sync::RwLock as StdRwLock;
use tokio::io::{stdin, stdout};
use tokio::sync::RwLock;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

#[derive(Debug)]
struct ErrorCollector {
    errors: Arc<StdRwLock<Vec<Diagnostic>>>,
}

impl Reporter for ErrorCollector {
    fn report(&self, error: &CompileError) {
        match error {
            CompileError::Syntax(e) => {
                self.errors.write().unwrap().push(
                    Backend::create_diagnostic(&e.to_string(), e.span())
                );
            },
            CompileError::TypeError(e) => {
                self.errors.write().unwrap().push(
                    Backend::create_diagnostic(&e.to_string(), e.span())
                );
            },
            CompileError::IO(io_err) => {
                todo!("{}", io_err)
            }
        }
    }
}

#[derive(Debug)]
struct Backend {
    client: Client,
    documents: Arc<RwLock<HashMap<String, Program>>>,
}

impl Backend {
    async fn parse(&self, uri: &Url) {
        let reporter = ErrorCollector {
            errors: Arc::new(StdRwLock::new(Vec::new())),
        };

        let context = FileContext::try_new(&uri.to_file_path().unwrap(), &reporter).unwrap();

        let mut parser = Parser::new(context);
        let program= parser.parse();
        let hir = TypeChecker::new(&reporter).resolve_types(&program);
        self.documents.write().await.insert(uri.to_string(), hir);

        let errors = reporter.errors.read().unwrap().clone();

        self.report_diagnostics(uri.clone(), errors).await;
    }

    async fn report_diagnostics(&self, uri: Url, diagnostics: Vec<Diagnostic>) {
        self.client
            .publish_diagnostics(uri, diagnostics, None)
            .await;
    }

    fn create_diagnostic(
        message: &str,
        span: Span,
    ) -> Diagnostic {
        Diagnostic {
            range: Range {
                start: Position::new(span.start.line as u32, span.start.offset as u32),
                end: Position::new(span.end.line as u32, span.end.offset as u32),
            },
            severity: Some(DiagnosticSeverity::ERROR),
            message: message.to_string(),
            ..Default::default()
        }
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        let capabilities = ServerCapabilities {
            completion_provider: Some(CompletionOptions {
                resolve_provider: Some(false),
                ..Default::default()
            }),
            text_document_sync: Some(TextDocumentSyncCapability::Options(TextDocumentSyncOptions {
                open_close: Some(true),
                change: Some(TextDocumentSyncKind::FULL),
                ..Default::default()
            })),
            ..Default::default()
        };

        Ok(InitializeResult {
            capabilities,
            server_info: Some(ServerInfo {
                name: "Cart LSP".to_string(),
                version: Some(env!("CARGO_PKG_VERSION").to_string()),
            }),
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "Cart LSP initialized")
            .await;
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let uri = params.text_document.uri;
        self.parse(&uri).await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri;
        self.parse(&uri).await;
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let docs = self.documents.read().await;
        let all_functions = docs.get(&params.text_document_position.text_document.uri.to_string()).unwrap().get_all_high_level_functions();

        let completion_list = CompletionList {
            is_incomplete: false,
            items: all_functions.into_iter().map(|f| {
                CompletionItem {
                    label: f.signature.original_name.clone(),
                    kind: Some(CompletionItemKind::FUNCTION),
                    detail: Some(f.signature.original_name.clone()),
                    ..Default::default()
                }
            }).collect()
        };

        Ok(Some(CompletionResponse::List(completion_list)))
    }
}

#[tokio::main]
async fn main() {
    let stdin = stdin();
    let stdout = stdout();

    let (service, socket) = LspService::new(|client| Backend {
        client,
        documents: Arc::new(RwLock::new(HashMap::new())),
    });

    Server::new(stdin, stdout, socket).serve(service).await;
}
