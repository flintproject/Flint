set(test_files
  test_bcc
  test_sort
  test_tac
  )

foreach(test_file ${test_files})
  add_executable(test-compiler-${test_file}
    ${test_file}.cc
    )
  target_link_libraries(test-compiler-${test_file}
    libflintxx
    libflint
    sqlite3
    Boost::filesystem
    Boost::unit_test_framework
    )
  add_test(NAME test-compiler-${test_file}
    COMMAND test-compiler-${test_file}
    )
endforeach(test_file ${test_files})
