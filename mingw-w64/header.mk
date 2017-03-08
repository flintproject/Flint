JARS := \
    GarudaSDK-2.0 \
    commons-cli-1.3.1 \
    commons-io-2.5 \
    commons-logging-1.2 \
    dom4j-1.6.1 \
    jackson-all-1.8.1 \
    jcommon-1.0.23 \
    jfreechart-1.0.19 \
    log4j-1.2.17 \
    protobuf-java-3.2.0 \
    sqlite-jdbc-3.14.2.1
BIN_DLLS := \
    libODES-0 \
    libczmq \
    libprotobuf-$(PROTOBUF_CURRENT) \
    libsbml \
    libsedml-0 \
    libxml2-2 \
    libzmq
BOOST_DLLS := \
    libboost_filesystem-mt \
    libboost_program_options-mt \
    libboost_system-mt
SUNDIALS_DLLS := \
    libsundials_arkode \
    libsundials_cvodes \
    libsundials_ida \
    libsundials_kinsol \
    libsundials_nvecserial
FLINT_DLLS := \
    libflint-0 \
    libflintxx-0 \
    libsqlite3-0
EXES := \
    csv2isd \
    flint-exec \
    flint-lodbg \
    flint-open \
    flint-pause \
    flint-resume \
    flint-run \
    flint-tr \
    flint2 \
    isd2csv \
    isdcut \
    isddiff \
    isdhead \
    isdls \
    isdmerge \
    isdplot \
    isdsort \
    isdstrip \
    isdwc
