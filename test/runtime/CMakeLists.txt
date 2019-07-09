set(test_files
  test-init
  )

foreach(test_file ${test_files})
  add_executable(test-runtime-${test_file}
    ${test_file}.cc
    )
  target_link_libraries(test-runtime-${test_file}
    libflintxx
    libflint
    SQLite::SQLite3
    Boost::filesystem
    Boost::unit_test_framework
    )
  add_test(NAME test-runtime-${test_file}
    COMMAND test-runtime-${test_file}
    )
endforeach(test_file ${test_files})
