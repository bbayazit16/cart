use cart::context::{FileContext, Span};
use cart::errors::CompileError;
use cart::parser::Parser;
use cart::reporter::Reporter;
use std::collections::HashMap;
use std::sync::Arc;
use tokio::io::{stdin, stdout};
use tokio::sync::RwLock;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

#[derive(Debug)]
struct ErrorCollector {
    errors: Arc<RwLock<Vec<Diagnostic>>>,
}

impl Reporter for ErrorCollector {
    fn report(&self, error: &CompileError) {
        match error {
            CompileError::Syntax(e) => {
                self.errors.write().await.push(
                    Backend::create_diagnostic(&e.to_string(), e.span())
                );
            },
            CompileError::TypeError(e) => {
                self.errors.write().await.push(
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
    documents: Arc<RwLock<HashMap<Url, String>>>,
}

impl Backend {
    async fn parse(&self, uri: &Url) {
        let reporter = ErrorCollector {
            errors: Arc::new(RwLock::new(Vec::new())),
        };

        let context = FileContext::try_new(&uri.to_file_path().unwrap(), &reporter).unwrap();

        let mut parser = Parser::new(context);
        let tree=  parser.parse();
        let errors = reporter.errors.read().await;
        self.report_diagnostics(uri.clone(), errors.clone()).await;
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
        // let text = params.text_document.text;
        self.parse(&uri).await;
        // self.documents.write().await.insert(uri, text);
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        dbg!(&params);
        let completion_list = CompletionList {
            is_incomplete: false,
            items: vec![
                CompletionItem {
                    label: "Hello".to_string(),
                    kind: Some(CompletionItemKind::FUNCTION),
                    detail: Some("Hello, World!".to_string()),
                    ..Default::default()
                },
            ],
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
