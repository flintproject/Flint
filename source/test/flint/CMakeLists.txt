set(test_files
  test-cng
  test-sexp
  test-sexp-parser
  test-sexp-tokenizer
  test-temporary-file
  test-translate
  test-trim
  )

foreach(test_file ${test_files})
  add_executable(test-flint-${test_file}
    ${test_file}.cc
    )
  target_link_libraries(test-flint-${test_file}
    libflintxx
    libflint
    SQLite::SQLite3
    Boost::filesystem
    Boost::unit_test_framework
    )
  add_test(NAME test-flint-${test_file}
    COMMAND test-flint-${test_file}
    )
endforeach(test_file ${test_files})
