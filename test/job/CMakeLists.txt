set(test_files
  test-evolve
  test-path
  )

foreach(test_file ${test_files})
  add_executable(test-job-${test_file}
    ${test_file}.cc
    )
  target_link_libraries(test-job-${test_file}
    libflintxx
    libflint
    sqlite3
    Boost::filesystem
    Boost::unit_test_framework
    )
  add_test(NAME test-job-${test_file}
    COMMAND test-job-${test_file}
    )
endforeach(test_file ${test_files})
