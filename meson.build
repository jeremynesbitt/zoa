# Zoa Cross Platform Build
# Jeremy Nesbitt
# Started 2023
# 
# started from gtk-fortran meson.build file


project('Zoa', ['fortran', 'c'],
        version: files(meson.project_source_root() / 'version.txt'),
        # version: '0.1.1',
        license: 'GPL3',
        meson_version: '>=0.60')


# Print some informations:
#system = host_machine.system()
system = host_machine.system()
message('Host machine system = '+system)
build_type = get_option('buildtype')
message('Build type: '+build_type)
versionArg = '-D__VERSION=' + '"' + meson.project_version() + '"' 
osArg = ' '
if system == 'darwin'
   osArg = '-DMACOS'
   message('System is Darwin')
   #message('- ' , globArg)
endif
if system == 'windows'
  osArg = '-DWINDOWS'
  message('System is Windows')
  #fc_ld='link'
endif

bldArg = '' 
if get_option('buildtype').startswith('release')
   bldArg = '-D__RELEASE'
endif

if get_option('buildtype').contains('debug')
   bldArg = '-D__DEBUG'
endif

globArg = [osArg, bldArg, versionArg]

message('Global Arguments are ', globArg)


# Include the GNOME module
gnome = import('gnome')

# Tell meson where to find our resources file and to compile it as a GResource
gresource = gnome.compile_resources(
    'gresource',
    'data' / 'gresource.xml',
    source_dir: 'data'
)




#For C code, could use this api vs global arguments
#But global arguments are working so never fully fleshed this out
#eg see gtksourceview4 for example
#config_h = configuration_data()

fortran = meson.get_compiler('fortran')


if fortran.get_id().contains('intel-cl')
  # Define arguments for different build types (debug is default):
  if build_type == 'debug'
    #warning_level = 2
    supported_arguments = fortran.get_supported_arguments([
    '-warn:all',
    '/Qdiag-disable:remark',
    '/Qdiag-disable:8889',
    '/Qm64',
    '/threads',
    '/traceback',
    #'/Od',
    '/debug:full',
    '/dbglibs',
    '/winapp',
    '/assume:byterecl',
    '/fpp'])
    add_project_link_arguments(['/INCREMENTAL:NO','/NODEFAULTLIB:MSVCRT'], language: 'fortran')


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
    add_project_link_arguments(['/NODEFAULTLIB:MSVCRTD'], language: 'fortran')

  endif
endif

if fortran.get_id().contains('gcc')
  # Define arguments for different build types (debug is default):
  if build_type == 'debug'
    warning_level = 2
    supported_arguments = fortran.get_supported_arguments([
    #'-pthread',
    '-gdwarf-2',
    '-g',
    #'-Wall',
    #'-fsanitize=address,undefined',
    #'-pedantic',
    #'-Wtabs',
    '-Wno-unused-variable',
    '-Wno-unused-dummy-argument',
    '-frecursive',
    '-fcheck=bounds',
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
    #'-pthread',
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
# 
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
src_c = []

# Each of these subdirectories contain a meson.build file:
subdir('etc')
subdir('slsqp')
subdir('src')

thread_dep = dependency('threads')

c_args = []
c_deps = []
c_deps += thread_dep
c_deps += dep3
# Build for c interface code
if system == 'darwin'

c_args += '-xobjective-c'
cocoa_dep = dependency('appleframeworks', modules : ['Cocoa'])
c_deps += cocoa_dep

endif

add_global_arguments(osArg, language: 'c')

c_lib = static_library(
  'zoa_c_lib',
  sources: src_c,
  dependencies:  c_deps,
  link_language: 'c',
  c_args: c_args,
  install: false
)

c_dep = declare_dependency(link_with : c_lib)

# Build target
zoa_lib = static_library(
  meson.project_name(),
  sources: srcs,
  #version: meson.project_version(),
  dependencies: [zoa_deps, c_dep],
  link_language: 'fortran',
  install: false#,
  #install_dir: 'Contents/libs'
)


# Icons
# Define PKG_DATADIR as a global symbol
#pkg_datadir = get_option('prefix') / get_option('datadir') / meson.project_name()
#add_project_arguments('-DPKG_DATADIR=@0@'.format(datadir), language: 'c')

# List the icons to install
#action_icons_dir = MacOS / 'icons/actions'
#action_icons = [
#  'icon8-tst.png'
#]

# Install the icons




if system == 'windows'
  windows = import('windows')
  resources = ['Windows/icon.ico']
  prog += windows.compile_resources('Windows/zoawin.rc', depend_files: resources, include_directories: meson.project_source_root())


endif

xtb_exe = executable(
  meson.project_name(),
  gresource,
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
  install_subdir(meson.project_source_root() / 'help/html', install_dir : 'Contents/Resources/help', strip_directory: false)
  meson.add_install_script(files('macos_install.script.sh'))
endif
