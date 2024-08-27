project('minrx', ['c', 'cpp'],
        version : '0.0.1',
        default_options : ['warning_level=3', 'cpp_std=c++20'])

libminrx_inc = include_directories('.')
libminrx_lib = library('minrx',
		       'minrx.cpp',
		       install : true,
		       soversion : '0')

libminrx_dep = declare_dependency(
    link_with : libminrx_lib,
    include_directories : libminrx_inc)

executable('tryit', 'tryit.c', dependencies : [libminrx_dep])

install_headers('minrx.h')
install_man('minrx.3')
pkgconfig = import('pkgconfig')
pkgconfig.generate(libminrx_lib,
                   description : 'A minimalist POSIX Extended Regular Expression matcher')
