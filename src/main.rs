use pomelo_lsp::LspBackend;
use tower_lsp::{LspService, Server};

#[tokio::main]
async fn main() {
    tracing_subscriber::fmt().init();

    let (stdin, stdout) = (tokio::io::stdin(), tokio::io::stdout());
    let (service, socket) = LspService::new(|client| LspBackend::new(client));
    Server::new(stdin, stdout, socket).serve(service).await;
}
