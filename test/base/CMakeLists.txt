set(test_files
  test_rational
  )

foreach(test_file ${test_files})
  add_executable(test-base-${test_file}
    ${test_file}.cc
    )
  target_link_libraries(test-base-${test_file}
    libflintxx
    libflint
    SQLite::SQLite3
    Boost::filesystem
    Boost::unit_test_framework
    )
  add_test(NAME test-base-${test_file}
    COMMAND test-base-${test_file}
    )
endforeach(test_file ${test_files})
