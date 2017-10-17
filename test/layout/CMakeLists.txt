set(test_files
  test-generate
  )

foreach(test_file ${test_files})
  add_executable(test-layout-${test_file}
    ${test_file}.cc
    )
  target_link_libraries(test-layout-${test_file}
    libflintxx
    libflint
    sqlite3
    Boost::filesystem
    Boost::unit_test_framework
    )
  add_test(NAME test-layout-${test_file}
    COMMAND test-layout-${test_file}
    )
endforeach(test_file ${test_files})
