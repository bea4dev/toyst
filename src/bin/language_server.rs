use tower_lsp::{LspService, Server};
use toyst::lsp::ToystLanguageServer;

#[tokio::main]
async fn main() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(|client| ToystLanguageServer::new(client));
    Server::new(stdin, stdout, socket).serve(service).await;
}
