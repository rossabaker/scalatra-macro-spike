object Demo extends Scalatra {
  get("/foo/:id") { params.id }

  get("/foo/:id") { params("query") }

  // Should not compile!
  get("/bar/:id") { params.oops }
}
