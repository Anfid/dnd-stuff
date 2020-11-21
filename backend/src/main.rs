use actix_files::Files;
use actix_web::{App, HttpServer};

#[actix_rt::main]
async fn main() -> std::io::Result<()> {
    HttpServer::new(|| App::new().service(Files::new("/", "./web/").index_file("index.html")))
        .bind("0.0.0.0:8088")?
        .run()
        .await
}
