# # MIT License
#
# Copyright (c) 2020 rlang authors
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
#   The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

#' rlang deprecated call_standardise, but this is necessary for
#' the `modify_nested_call()` function. The capability isn't
#' matched by the `call_match()` function.
#'
#' In modify nested call, this is used to restructure the function call
#' to have all named arguments. Since all the functions called inside
#' `call_standardise()` are exported and currently stable within rlang,
#' I'm porting the function here.
#'
#' @param call
#'
#' @return call
#' @noRd
tplyr_call_standardise <- function(call, env= rlang::caller_env()) {

  expr <- rlang::get_expr(call)

  if (!rlang::is_call(expr)) {
    stop("call_standardise error")
  }

  # The call name might be a literal, not necessarily a symbol
  env <- rlang::get_env(call, env)
  fn <- rlang::eval_bare(rlang::node_car(expr), env)

  if (rlang::is_primitive(fn)) {
    call
  } else {
    matched <- match.call(fn, expr)
    rlang::set_expr(call, matched)
  }

}

