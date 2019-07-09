set(test_files
  test-eq-determinant
  test-eq-outerproduct
  test-eq-scalarproduct
  test-eq-selector
  test-eq-transpose
  test-equations
  test-eq-vectorproduct
  )

foreach(test_file ${test_files})
  add_executable(test-cas-${test_file}
    ${test_file}.cc
    )
  target_link_libraries(test-cas-${test_file}
    libflintxx
    libflint
    SQLite::SQLite3
    Boost::filesystem
    Boost::unit_test_framework
    )
  add_test(NAME test-cas-${test_file}
    COMMAND test-cas-${test_file}
    )
endforeach(test_file ${test_files})
