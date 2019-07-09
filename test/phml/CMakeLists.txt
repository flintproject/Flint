set(test_files
  test-nc
  test-read
  test-uot
  )

foreach(test_file ${test_files})
  add_executable(test-phml-${test_file}
    ${test_file}.cc
    )
  target_link_libraries(test-phml-${test_file}
    libflintxx
    libflint
    SQLite::SQLite3
    Boost::filesystem
    Boost::unit_test_framework
    )
  add_test(NAME test-phml-${test_file}
    COMMAND test-phml-${test_file}
    )
endforeach(test_file ${test_files})
