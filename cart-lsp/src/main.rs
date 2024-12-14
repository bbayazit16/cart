use cart::context::{FileContext, Span};
use cart::errors::CompileError;
use cart::parser::Parser;
use cart::reporter::Reporter;
use std::collections::HashMap;
use std::sync::Arc;
use tokio::io::{stdin, stdout};
use tokio::sync::RwLock;
use std::sync::{RwLock as StdRwLock};
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};
use cart::hir::TypeChecker;

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
    documents: Arc<RwLock<HashMap<Url, String>>>,
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

        let errors = reporter.errors.read().unwrap().clone();

        dbg!("Errors: {:?}", &errors);
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
        // let text = params.text_document.text;
        self.parse(&uri).await;
        // self.documents.write().await.insert(uri, text);
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri;
        // let text = params.text_document.text;
        self.parse(&uri).await;
        // self.documents.write().await.insert(uri, text);
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        // [cart-lsp/src/main.rs:122:9] &params = CompletionParams {
        //     text_document_position: TextDocumentPositionParams {
        //         text_document: TextDocumentIdentifier {
        //             uri: Url {
        //
        //         scheme: "file",
        //                 cannot_be_a_base: false,
        //                 username: "",
        //                 password: None,
        //                 host: None,
        //                 port: None,
        //                 path: "/Users/baris/dev/Languages/Rust/cart/program.cart",
        //                 query: None,
        //                 fragment: None,
        //             },
        //         },
        //         position: Position {
        //             line: 28,
        //             character: 47,
        //         },
        //     },
        //     work_done_progress_params: WorkDoneProgressParams {
        //         work_done_token: None,
        //     },
        //     partial_result_params: PartialResultParams {
        //         partial_result_token: None,
        //     },
        //     context: Some(
        //         CompletionContext {
        //             trigger_kind: Invoked,
        //             trigger_character: None,
        //         },
        //     ),
        // }
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
