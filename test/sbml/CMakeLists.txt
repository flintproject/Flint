set(test_files
  test_load
  test_parse
  test-read
  )

foreach(test_file ${test_files})
  add_executable(test-sbml-${test_file}
    ${test_file}.cc
    )
  target_link_libraries(test-sbml-${test_file}
    libflintxx
    libflint
    sqlite3
    Boost::filesystem
    Boost::unit_test_framework
    )
  add_test(NAME test-sbml-${test_file}
    COMMAND test-sbml-${test_file}
    )
endforeach(test_file ${test_files})
