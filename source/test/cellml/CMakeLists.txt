set(test_files
  test-read
  )

foreach(test_file ${test_files})
  add_executable(test-cellml-${test_file}
    ${test_file}.cc
    )
  target_link_libraries(test-cellml-${test_file}
    libflintxx
    libflint
    SQLite::SQLite3
    Boost::filesystem
    Boost::unit_test_framework
    )
  add_test(NAME test-cellml-${test_file}
    COMMAND test-cellml-${test_file}
    )
endforeach(test_file ${test_files})
