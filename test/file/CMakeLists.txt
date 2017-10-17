set(test_files
  test_detect
  )

foreach(test_file ${test_files})
  add_executable(test-file-${test_file}
    ${test_file}.cc
    )
  target_link_libraries(test-file-${test_file}
    libflintxx
    libflint
    sqlite3
    Boost::filesystem
    Boost::unit_test_framework
    )
  add_test(NAME test-file-${test_file}
    COMMAND test-file-${test_file}
    )
endforeach(test_file ${test_files})
