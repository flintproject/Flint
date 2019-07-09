set(test_files
  test-formula-lexer
  test-formula-parser
  )

foreach(test_file ${test_files})
  add_executable(test-gui-${test_file}
    ${test_file}.cc
    )
  target_link_libraries(test-gui-${test_file}
    libflint2
    libflintxx
    libflint
    SQLite::SQLite3
    Boost::filesystem
    Boost::unit_test_framework
    )
  add_test(NAME test-gui-${test_file}
    COMMAND test-gui-${test_file}
    )
endforeach(test_file ${test_files})
