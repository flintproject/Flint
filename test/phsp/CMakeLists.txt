set(test_files
  test_read
  )

foreach(test_file ${test_files})
  add_executable(test-phsp-${test_file}
    ${test_file}.cc
    )
  target_link_libraries(test-phsp-${test_file}
    libflintxx
    libflint
    sqlite3
    Boost::filesystem
    Boost::unit_test_framework
    )
  add_test(NAME test-phsp-${test_file}
    COMMAND test-phsp-${test_file}
    )
endforeach(test_file ${test_files})
