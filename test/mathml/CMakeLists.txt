set(test_files
  test_math_dumper
  )

foreach(test_file ${test_files})
  add_executable(test-mathml-${test_file}
    ${test_file}.cc
    )
  target_link_libraries(test-mathml-${test_file}
    libflintxx
    libflint
    SQLite::SQLite3
    Boost::filesystem
    Boost::unit_test_framework
    )
  add_test(NAME test-mathml-${test_file}
    COMMAND test-mathml-${test_file}
    )
endforeach(test_file ${test_files})
