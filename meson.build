# Zoa Cross Platform Build
# Jeremy Nesbitt
# Started 2023
# 
# started from gtk-fortran meson.build file

project('Zoa', ['fortran', 'c'],
        version: '0.1.0',
        license: 'GPL3',
        meson_version: '>=0.60')


# Print some informations:
system = host_machine.system()
message('Host machine system = '+system)
build_type = get_option('buildtype')
message('Build type: '+build_type)

osArg = ' '
if system == 'darwin'
   osArg = '-DMACOS'
   message('System is Darwin')
   #message('- ' , globArg)
endif
if system == 'windows'
  osArg = '-DWINDOWS'
  message('System is Windows')
  fc_ld='link'
endif

bldArg = '' 
if get_option('buildtype').startswith('release')
   bldArg = '-D__RELEASE'
endif

if get_option('buildtype').contains('debug')
   bldArg = '-D__DEBUG'
endif

globArg = [osArg, bldArg]

message('Global Arguments are ', globArg)

fortran = meson.get_compiler('fortran')


if fortran.get_id().contains('intel-cl')
  # Define arguments for different build types (debug is default):
  if build_type == 'debug'
    #warning_level = 2
    supported_arguments = fortran.get_supported_arguments([
    '-w',
    '/Qdiag-disable:remark',
    '/traceback',
    '/Od',
    '/debug:full',
    '/dbglibs',
    '/winapp',
    '/assume:byterecl',
    '/fpp'])
  elif build_type == 'release'
    # -w inhibit all warning messages
    supported_arguments = fortran.get_supported_arguments([
    #'-pthread',
    '-w',
    '/Qdiag-disable:remark',
    '/O2',
    '/DNDEBUG',
    '/winapp',
    '/assume:byterecl',
    '/fpp'])
  endif
endif

if fortran.get_id().contains('gcc')
  # Define arguments for different build types (debug is default):
  if build_type == 'debug'
    warning_level = 2
    supported_arguments = fortran.get_supported_arguments([
    '-pthread',
    '-gdwarf-2',
    '-g',
    #'-Wall',
    #'-fsanitize=address,undefined',
    #'-pedantic',
    #'-Wtabs',
    #'-fcheck=bounds',
    '-fcheck=mem',
    '-fcheck=pointer',
    '-fcheck=bits',
    '-fbacktrace',
    #'-Wno-unused-dummy-argument',
    '-fno-align-commons',
    '-static-libgcc',
    '-static-libgfortran',
    '-fPIC',
    '-cpp',
    '-rdynamic',
    '-static'])
  elif build_type == 'release'
    # -w inhibit all warning messages
    supported_arguments = fortran.get_supported_arguments([
    '-pthread',
    '-w',
    '-fno-align-commons',
    '-static-libgcc',
    '-static-libgfortran',
    '-fPIC',
    '-g',
    '-cpp'])
endif


endif


add_global_arguments(globArg, language : 'fortran')
add_project_arguments(supported_arguments, language: 'fortran')

# Need > 4.4.1 due to bug fix for _set_accels binding
dep0 = dependency('gtk-4-fortran', version: '>=4.4.1', required: true)
dep3 = dependency('gtk4', version: '>4.0', required: true)
dep1 = dependency('plplot', version: '>=5.13.0', required: true)
dep2 = dependency('plplot-fortran',
       version: '>=5.13.0',
       required: true)

zoa_deps = [dep3, dep0, dep1, dep2]

lib_deps = []

srcs = []
prog = []

# Each of these subdirectories contain a meson.build file:
subdir('etc')
subdir('src')

# Build target
zoa_lib = static_library(
  meson.project_name(),
  sources: srcs,
  #version: meson.project_version(),
  dependencies: zoa_deps,
  link_language: 'fortran',
  install: true,
  install_dir: 'Contents/libs'
)

thread_dep = dependency('threads')

if system == 'windows'
  windows = import('windows')
  resources = ['Windows/icon.ico']
  prog += windows.compile_resources('Windows/zoawin.rc', depend_files: resources, include_directories: meson.project_source_root())


endif

xtb_exe = executable(
  meson.project_name(),
  sources: prog,
  link_with: zoa_lib,
  dependencies:  [zoa_deps, thread_dep],
  link_language: 'fortran',
  install: true
)

if system == 'darwin'
  MacOSDir = meson.project_source_root() / 'MacOS'
  install_data(MacOSDir / 'AppIcon.icns', install_dir : 'Contents/Resources')
  install_data(MacOSDir / 'Info.plist', install_dir : 'Contents')
  meson.add_install_script(files('macos_install.script.sh'))
endif
