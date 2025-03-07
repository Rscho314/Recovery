test_that("rolog works", {
    rolog::rolog_init(argv1 = commandArgs()[1])
    rolog::query(call("member",1L,list(1L)))
    expect_equal(rolog::submit(), list())
    rolog::clear()
})
