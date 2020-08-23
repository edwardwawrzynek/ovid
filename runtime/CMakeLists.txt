set(LIB bootstrap.c)

add_library(ovidruntime STATIC ${LIB})
install(TARGETS ovidruntime DESTINATION ${CMAKE_INSTALL_LIBDIR})
