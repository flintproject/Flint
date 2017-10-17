set(test_files
  test_read
  )

foreach(test_file ${test_files})
  add_executable(test-sedml-${test_file}
    ${test_file}.cc
    )
  target_link_libraries(test-sedml-${test_file}
    libflintxx
    libflint
    sqlite3
    Boost::filesystem
    Boost::unit_test_framework
    )
  add_test(NAME test-sedml-${test_file}
    COMMAND test-sedml-${test_file}
    )
endforeach(test_file ${test_files})
