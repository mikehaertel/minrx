project('minrx', ['c'],
        version : '0.0.1',
        default_options : ['warning_level=3'])

libminrx_inc = include_directories('.')
libminrx_lib = library('minrx',
		       'minrx.c',
		       install : true,
		       soversion : '0')

libminrx_dep = declare_dependency(
    link_with : libminrx_lib,
    include_directories : libminrx_inc)

executable('rxgrep', 'rxgrep.c', dependencies : [libminrx_dep])
executable('tryit', 'tryit.c', dependencies : [libminrx_dep])

install_headers('minrx.h')
install_man('minrx.3')
pkgconfig = import('pkgconfig')
pkgconfig.generate(libminrx_lib,
                   description : 'A minimalist POSIX Extended Regular Expression matcher')
