# Copyright (C) 2019
# Free Software Foundation, Inc.
#
# This file is part of the gtk-fortran GTK / Fortran Interface library.
#
# This is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3, or (at your option)
# any later version.
#
# This software is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# Under Section 7 of GPL version 3, you are granted additional
# permissions described in the GCC Runtime Library Exception, version
# 3.1, as published by the Free Software Foundation.
#
# You should have received a copy of the GNU General Public License along with
# this program; see the files COPYING3 and COPYING.RUNTIME respectively.
# If not, see <http://www.gnu.org/licenses/>.
#
# Modified from gtk-4-fortran by jrnesbitt (first commit: 2019-05-30)


project('zoa', ['fortran', 'c'],
        version: '0.1.0',
        license: 'GPL3',
        meson_version: '>=0.54')


# Print some informations:
system = host_machine.system()
message('Host machine system = '+system)
build_type = get_option('buildtype')
message('Build type: '+build_type)

globArg = ' '
if system == 'darwin'
   globArg = '-DMACOS'
   message('System is Darwin')
   message('- ' , globArg)
endif
if system == 'windows'
  globArg = '-DWINDOWS'
  message('System is Windows')
endif

fortran = meson.get_compiler('fortran')

add_global_arguments(globArg, language : 'fortran')

# Define arguments for different build types (debug is default):
if build_type == 'debug'
  warning_level = 2
  supported_arguments = fortran.get_supported_arguments([
  '-pthread',
  '-pedantic',
  '-Wtabs',
  '-fcheck=bounds',
  '-fbacktrace',
  '-Wno-unused-dummy-argument',
  '-fno-align-commons',
  '-static-libgcc',
  '-static-libgfortran',
  '-fPIC',
  '-cpp'])
elif build_type == 'release'
  # -w inhibit all warning messages
  supported_arguments = fortran.get_supported_arguments([
  '-pthread',
  '-w',
  '-fno-align-commons',
  '-static-libgcc',
  '-static-libgfortran',
  '-fPIC',
  '-cpp'])
endif

add_project_arguments(supported_arguments, language: 'fortran')

dep0 = dependency('gtk-4-fortran', version: '>=4.3.0', required: true)
dep3 = dependency('gtk4', version: '>4.0', required: true)
dep1 = dependency('plplot', version: '>=5.13.0', required: true)
dep2 = dependency('plplot-fortran', version: '>=5.13.0', required: true)

zoa_deps = [dep0, dep3, dep1, dep2]

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
  link_language: 'fortran'
)

xtb_exe = executable(
  meson.project_name(),
  sources: prog,
  link_with: zoa_lib,
  dependencies:  zoa_deps,
  link_language: 'fortran'
)
