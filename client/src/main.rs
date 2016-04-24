extern crate clap;
extern crate websocket;

use clap::{Arg, App};
use websocket::{Client, Message};
use websocket::client::request::Url;

fn platform() -> &'static str {
    if cfg!(osx) {
        "osx"
    } else if cfg!(windows) {
        "windows"
    } else {
        "unix"
    }
}


fn main() {
    let matches = App::new("DNeatClient")
                          .version(env!("CARGO_PKG_VERSION"))
                          .arg(Arg::with_name("address")
                               .short("a")
                               .long("address")
                               .value_name("INPUT")
                               .help("Sets the websocket's host address")
                               .takes_value(true))
                          .arg(Arg::with_name("port")
                               .short("p")
                               .long("port")
                               .value_name("INPUT")
                               .help("Sets the websocket's host port")
                               .takes_value(true))
                          .get_matches();

    let address = matches.value_of("address").unwrap_or("localhost");
    let port = matches.value_of("port").unwrap_or("9000");

    let url_str = format!("ws://{}:{}/worker/{}", address, port, platform());
    println!("connecting to {}", url_str);
    let url = Url::parse(url_str.as_str()).unwrap();
    let request = match Client::connect(url) {
        Ok(r) => r,
        Err(e) => {
            println!("failed to connect ({})", e);
            std::process::exit(1);
        },
    };

    let response = request.send().unwrap();
    response.validate().unwrap();

    let mut client = response.begin();

    let message = Message::text("Hallöchen");
    client.send_message(&message).unwrap();

    for message in client.incoming_messages() {
        let message: Message = message.unwrap();
        println!("recv'd: {:?}", message);
    }
}
