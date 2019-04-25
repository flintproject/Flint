set(test_files
  test_assign
  test_euler
  test_euler_maruyama
  test_event
  test_rk4
  )

foreach(test_file ${test_files})
  add_executable(test-method-${test_file}
    ${test_file}.cc
    )
  target_link_libraries(test-method-${test_file}
    libflintxx
    libflint
    sqlite3
    Boost::filesystem
    Boost::unit_test_framework
    )
  add_test(NAME test-method-${test_file}
    COMMAND test-method-${test_file}
    )
endforeach(test_file ${test_files})
