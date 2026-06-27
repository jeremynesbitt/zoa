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
osArg = '-DLINUX'
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
supported_arguments = []
pkgconf = find_program('pkg-config', required: true)

if fortran.get_id().contains('intel-cl') or fortran.get_id().contains('intel-llvm-cl')
  # Define arguments for different build types (debug is default):
  if build_type == 'debug'
    #warning_level = 2
    supported_arguments = fortran.get_supported_arguments([
    #'-warn:all',
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
    '-Wno-maybe-uninitialized',
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
    '-cpp',
    '-static'])
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

if system == 'windows'
  dep0_compile_args_raw = run_command(
    pkgconf, ['--cflags', 'gtk-4-fortran'], check: true
  ).stdout().strip().split()
  dep0_compile_args = []
  foreach arg : dep0_compile_args_raw
    if arg != '/machine:x64'
      dep0_compile_args += arg
    endif
  endforeach

  dep0_link_args_raw = run_command(
    pkgconf, ['--libs', 'gtk-4-fortran'], check: true
  ).stdout().strip().split()
  dep0_libdir = run_command(
    pkgconf, ['--variable=libdir', 'gtk-4-fortran'], check: true
  ).stdout().strip()
  dep0_link_args = []
  foreach arg : dep0_link_args_raw
    if arg == '-lgtk-4-fortran' and fortran.get_id().contains('gcc')
      dep0_link_args += dep0_libdir / 'libgtk-4-fortran.a'
    elif not arg.startswith('-Wl,-rpath,')
      dep0_link_args += arg
    endif
  endforeach

  dep0 = declare_dependency(
    compile_args: dep0_compile_args,
    link_args: dep0_link_args,
  )

  dep3_compile_args = run_command(
    pkgconf, ['--cflags', 'gtk4'], check: true
  ).stdout().strip().split()
  dep3_link_args_raw = run_command(
    pkgconf, ['--libs', 'gtk4'], check: true
  ).stdout().strip().split()
  dep3_libdir = run_command(
    pkgconf, ['--variable=libdir', 'gtk4'], check: true
  ).stdout().strip()
  dep3_link_args = ['-L' + dep3_libdir]
  foreach arg : dep3_link_args_raw
    if not arg.startswith('-Wl,-rpath,')
      dep3_link_args += arg
    endif
  endforeach

  dep3 = declare_dependency(
    compile_args: dep3_compile_args,
    link_args: dep3_link_args,
  )
endif

zoa_deps = [dep3, dep0, dep1, dep2]

lib_deps = []

srcs = []
plotcad5_src = []
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

plotcad5_override_options = []
if (fortran.get_id().contains('intel-cl') or fortran.get_id().contains('intel-llvm-cl')) and build_type == 'release'
  plotcad5_override_options = ['optimization=0']
endif

plotcad5_lib = static_library(
  'zoa_plotcad5',
  sources: plotcad5_src,
  link_with: zoa_lib,
  dependencies: [zoa_deps, c_dep],
  link_language: 'fortran',
  override_options: plotcad5_override_options,
  install: false
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
  link_with: [zoa_lib, plotcad5_lib],
  dependencies:  [zoa_deps, thread_dep],
  link_language: 'fortran',
  install: true
)

# ZeroMQ server executable (headless, for Jupyter integration)
zmq_dep = dependency('libzmq', required: false)
if zmq_dep.found()
  zoa_server_exe = executable(
    'zoa_server',
    sources: [server_prog, server_srcs],
    link_with: [zoa_lib, plotcad5_lib],
    dependencies: [zoa_deps, thread_dep, zmq_dep],
    link_language: 'fortran',
    install: true,
    install_dir: 'Contents/MacOS'
  )
endif

# Test runner executable (headless, no GResource needed)
test_runner = executable(
  'zoa_test',
  sources: test_prog,
  link_with: [zoa_lib, plotcad5_lib],
  dependencies: [zoa_deps, thread_dep],
  link_language: 'fortran',
  install: false
)

# Test cases
# is_parallel: false because tests share disk state (currlens.zoa temp file)
test('basic_paraxial', test_runner,
     args: [meson.project_source_root() / 'test/basic_paraxial.zoa',
            meson.project_source_root() / 'test/basic_paraxial.ref'],
     timeout: 60, is_parallel: false)

test('fieldflattener', test_runner,
     args: [meson.project_source_root() / 'test/fieldflattener.zoa',
            meson.project_source_root() / 'test/fieldflattener.ref'],
     timeout: 120, is_parallel: false)

test('newlenstest', test_runner,
     args: [meson.project_source_root() / 'test/newlensTest.zoa',
            meson.project_source_root() / 'test/newlensTest.ref'],
     timeout: 120, is_parallel: false)

test('DoubleGaussTest', test_runner,
     args: [meson.project_source_root() / 'test/DoubleGaussTest.zoa',
            meson.project_source_root() / 'test/DoubleGaussTest.ref'],
     timeout: 120, is_parallel: false)

test('macro_ex1', test_runner,
     args: [meson.project_source_root() / 'test/macro_ex1.zoa',
            meson.project_source_root() / 'test/macro_ex1.ref'],
     timeout: 120, is_parallel: false)

test('raytrace', test_runner,
     args: [meson.project_source_root() / 'test/raytrace.zoa',
            meson.project_source_root() / 'test/raytrace.ref'],
     timeout: 120, is_parallel: false)

test('stop_aperture', test_runner,
     args: [meson.project_source_root() / 'test/stop_aperture.zoa',
            meson.project_source_root() / 'test/stop_aperture.ref'],
     timeout: 120, is_parallel: false)

test('stop_draw', test_runner,
     args: [meson.project_source_root() / 'test/stop_draw.zoa',
            meson.project_source_root() / 'test/stop_draw.ref'],
     timeout: 120, is_parallel: false)

test('vie_settings', test_runner,
     args: [meson.project_source_root() / 'test/vie_settings.zoa',
            meson.project_source_root() / 'test/vie_settings.ref'],
     timeout: 120, is_parallel: false)

test('refrays', test_runner,
     args: [meson.project_source_root() / 'test/refrays.zoa',
            meson.project_source_root() / 'test/refrays.ref'],
     timeout: 120, is_parallel: false)

test('bentley_twice', test_runner,
     args: [meson.project_source_root() / 'test/bentley_twice.zoa',
            meson.project_source_root() / 'test/bentley_twice.ref'],
     timeout: 120, is_parallel: false)

test('litho_restart', test_runner,
     args: [meson.project_source_root() / 'test/litho_restart.zoa',
            meson.project_source_root() / 'test/litho_restart.ref'],
     timeout: 120, is_parallel: false)

test('tho_test', test_runner,
     args: [meson.project_source_root() / 'test/tho_test.zoa',
            meson.project_source_root() / 'test/tho_test.ref'],
     timeout: 120, is_parallel: false)

test('zoom_basic', test_runner,
     args: [meson.project_source_root() / 'test/zoom_basic.zoa',
            meson.project_source_root() / 'test/zoom_basic.ref'],
     timeout: 120, is_parallel: false)

test('plots_geom', test_runner,
     args: [meson.project_source_root() / 'test/plots_geom.zoa',
            meson.project_source_root() / 'test/plots_geom.ref'],
     timeout: 120, is_parallel: false)

test('plots_wave', test_runner,
     args: [meson.project_source_root() / 'test/plots_wave.zoa',
            meson.project_source_root() / 'test/plots_wave.ref'],
     timeout: 240, is_parallel: false)

test('plots_seidel', test_runner,
     args: [meson.project_source_root() / 'test/plots_seidel.zoa',
            meson.project_source_root() / 'test/plots_seidel.ref'],
     timeout: 120, is_parallel: false)

test('pickups', test_runner,
     args: [meson.project_source_root() / 'test/pickups.zoa',
            meson.project_source_root() / 'test/pickups.ref'],
     timeout: 120, is_parallel: false)

test('solves', test_runner,
     args: [meson.project_source_root() / 'test/solves.zoa',
            meson.project_source_root() / 'test/solves.ref'],
     timeout: 120, is_parallel: false)

test('apertures_bes', test_runner,
     args: [meson.project_source_root() / 'test/apertures_bes.zoa',
            meson.project_source_root() / 'test/apertures_bes.ref'],
     timeout: 120, is_parallel: false)

test('zmx_import', test_runner,
     args: [meson.project_source_root() / 'test/zmx_import.zoa',
            meson.project_source_root() / 'test/zmx_import.ref'],
     timeout: 120, is_parallel: false)

test('export_roundtrip', test_runner,
     args: [meson.project_source_root() / 'test/export_roundtrip.zoa',
            meson.project_source_root() / 'test/export_roundtrip.ref'],
     timeout: 120, is_parallel: false)

test('mirror_example', test_runner,
     args: [meson.project_source_root() / 'test/mirror_example.zoa',
            meson.project_source_root() / 'test/mirror_example.ref'],
     timeout: 120, is_parallel: false)

test('debug_rmd', test_runner,
     args: [meson.project_source_root() / 'test/debug_rmd.zoa',
            meson.project_source_root() / 'test/debug_rmd.ref'],
     timeout: 120, is_parallel: false)

test('undo_redo', test_runner,
     args: [meson.project_source_root() / 'test/undo_redo.zoa',
            meson.project_source_root() / 'test/undo_redo.ref'],
     timeout: 120, is_parallel: false)

test('queries_basic', test_runner,
     args: [meson.project_source_root() / 'test/queries_basic.zoa',
            meson.project_source_root() / 'test/queries_basic.ref'],
     timeout: 120, is_parallel: false)

test('lens_edit', test_runner,
     args: [meson.project_source_root() / 'test/lens_edit.zoa',
            meson.project_source_root() / 'test/lens_edit.ref'],
     timeout: 120, is_parallel: false)

test('tilts_decenters', test_runner,
     args: [meson.project_source_root() / 'test/tilts_decenters.zoa',
            meson.project_source_root() / 'test/tilts_decenters.ref'],
     timeout: 120, is_parallel: false)

test('wavefront_zern', test_runner,
     args: [meson.project_source_root() / 'test/wavefront_zern.zoa',
            meson.project_source_root() / 'test/wavefront_zern.ref'],
     timeout: 120, is_parallel: false)

test('optim_extra', test_runner,
     args: [meson.project_source_root() / 'test/optim_extra.zoa',
            meson.project_source_root() / 'test/optim_extra.ref'],
     timeout: 120, is_parallel: false)

test('plot_modifiers', test_runner,
     args: [meson.project_source_root() / 'test/plot_modifiers.zoa',
            meson.project_source_root() / 'test/plot_modifiers.ref'],
     timeout: 120, is_parallel: false)

test('gui_guards', test_runner,
     args: [meson.project_source_root() / 'test/gui_guards.zoa',
            meson.project_source_root() / 'test/gui_guards.ref'],
     timeout: 120, is_parallel: false)

test('files_misc', test_runner,
     args: [meson.project_source_root() / 'test/files_misc.zoa',
            meson.project_source_root() / 'test/files_misc.ref'],
     timeout: 120, is_parallel: false)

test('misc_commands', test_runner,
     args: [meson.project_source_root() / 'test/misc_commands.zoa',
            meson.project_source_root() / 'test/misc_commands.ref'],
     timeout: 120, is_parallel: false)

test('raytrace_opt', test_runner,
     args: [meson.project_source_root() / 'test/raytrace_opt.zoa',
            meson.project_source_root() / 'test/raytrace_opt.ref'],
     timeout: 120, is_parallel: false)


if system == 'darwin'
  MacOSDir = meson.project_source_root() / 'MacOS'
  install_data(MacOSDir / 'AppIcon.icns', install_dir : 'Contents/Resources')
  install_data(MacOSDir / 'Info.plist', install_dir : 'Contents')
  install_subdir(meson.project_source_root() / 'help/html', install_dir : 'Contents/Resources/help', strip_directory: false)
  meson.add_install_script(files('macos_install.script.sh'))
endif
