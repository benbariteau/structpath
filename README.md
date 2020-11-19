# structpath
Library for parsing url paths into structs and generating url paths from structs in rust

structpath is a libary which allows parsing and generating url paths in a convenient type safe way.

structpath leverages serde to help parse values into structs.

# Examples

## Basic example

```rust,ignore
use serde::{Deserialize, Serialize};
use structpath::Schema;

#[derive(Deserialize)]
struct FooParams {
    foo_id: u128,
    bar: String,
}

const foo_path = "/foo/<foo_id:u128>/bar/<bar>";

// This is a general idea of a web request handler, not important for the demonstration
fn foo_bar(request: Request) -> Response {
    let params: FooParams = Schema::path(foo_path).parse(request.path);
}

fn baz(request: Request) -> Response {
    let foo_path = Schema::path(foo_path).generate(FooParams{foo_id: foo_id, bar: bar});
    Response::Redirect(foo_path)
}
```
