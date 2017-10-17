set(test_files
  test-run
  test_spec
  )

foreach(test_file ${test_files})
  add_executable(test-run-${test_file}
    ${test_file}.cc
    )
  target_link_libraries(test-run-${test_file}
    libflintxx
    libflint
    sqlite3
    Boost::filesystem
    Boost::unit_test_framework
    )
  add_test(NAME test-run-${test_file}
    COMMAND test-run-${test_file}
    )
endforeach(test_file ${test_files})
