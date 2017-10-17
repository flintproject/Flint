set(test_files
  test-load
  test_param
  test_var
  )

foreach(test_file ${test_files})
  add_executable(test-load-${test_file}
    ${test_file}.cc
    )
  target_link_libraries(test-load-${test_file}
    libflintxx
    libflint
    sqlite3
    Boost::filesystem
    Boost::unit_test_framework
    )
  add_test(NAME test-load-${test_file}
    COMMAND test-load-${test_file}
    )
endforeach(test_file ${test_files})
