# Load PLplot tcl extension

# Copyright (C) 2004-2015 Alan W. Irwin
# Copyright (C) 2004  Joao Cardoso
#
# This file is part of PLplot.
#
# PLplot is free software; you can redistribute it and/or modify
# it under the terms of the GNU Library General Public License as published
# by the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# PLplot is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Library General Public License for more details.
#
# You should have received a copy of the GNU Library General Public License
# along with PLplot; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA

# This proc takes care of finding & loading the proper driver.

# The original version dealt with the possibility that either or both $dir and
# $file may contain spaces, or characters like []{}, but this hasn't been
# tested for this version.

proc load_pkg_Pltcl {dir} {
    global pllibrary tcl_platform auto_path
    #puts "loading Pltcl, dir: $dir"
    set pllibrary $dir

    set thisDir [pwd]
    # Find absolute path of $dir
    cd $dir
    set pkgIndexDir [pwd]
    cd $thisDir
    set buildDir "/Users/jrn/Fortran/plplot-5.15.0-gtk4/build"
    set bLen [string length $buildDir]
    # If pkgIndexDir is in the build tree...
    if {![string compare -length $bLen $buildDir $pkgIndexDir]} then {
	set searchdirs [list "/Users/jrn/Fortran/plplot-5.15.0-gtk4/build/bindings/tcl"]
    } else {
	set searchdirs [list "/usr/local/lib"]
    }
    #puts $searchdirs
    set dlnames [list "libplplottcltk.14.1.0.dylib"]
    set file ""
    foreach reldir $searchdirs {
	foreach dlname $dlnames {
	    set driver [file join $dir $reldir $dlname]
	    #puts "looking for driver file: $driver"
	    if [file exists $driver] {
		set file $driver
		break
	    }
	}
	if { $file != "" } { break }
    }
    if { $file == "" } {
	error "load_pkg_Pltcl: could not find loadable driver"
    }

    load $file Pltcl
    # put core tcl scripts in path
    lappend auto_path $dir/tcl
    rename load_pkg_Pltcl {}
}

package ifneeded Pltcl 5.15.0 [list load_pkg_Pltcl $dir]
# Load PLplot tk extension

# Copyright (C) 2004-2015 Alan W. Irwin
# Copyright (C) 2004  Joao Cardoso
#
# This file is part of PLplot.
#
# PLplot is free software; you can redistribute it and/or modify
# it under the terms of the GNU Library General Public License as published
# by the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# PLplot is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Library General Public License for more details.
#
# You should have received a copy of the GNU Library General Public License
# along with PLplot; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA

# This proc takes care of finding & loading the proper driver.

# The original version dealt with the possibility that either or both $dir and
# $file may contain spaces, or characters like []{}, but this hasn't been
# tested for this version.

proc load_pkg_Pltk {dir} {
    global pllibrary tcl_platform auto_path
    #puts "loading Pltk, dir: $dir"
    set pllibrary $dir

    set thisDir [pwd]
    # Find absolute path of $dir
    cd $dir
    set pkgIndexDir [pwd]
    cd $thisDir
    set buildDir "/Users/jrn/Fortran/plplot-5.15.0-gtk4/build"
    set bLen [string length $buildDir]
    # If pkgIndexDir is in the build tree...
    if {![string compare -length $bLen $buildDir $pkgIndexDir]} then {
	set searchdirs [list "/Users/jrn/Fortran/plplot-5.15.0-gtk4/build/drivers"]
    } else {
	set searchdirs [list "/usr/local/lib/plplot5.15.0/drivers"]
    }
    #puts $searchdirs
    set dlnames [list "tk.so"]
    set file ""
    foreach reldir $searchdirs {
	foreach dlname $dlnames {
	    set driver [file join $dir $reldir $dlname]
	    #puts "looking for driver file: $driver"
	    if [file exists $driver] {
		set file $driver
		break
	    }
	}
	if { $file != "" } { break }
    }
    if { $file == "" } {
	error "load_pkg_Pltk: could not find loadable driver"
    }

    load $file Pltk
    # put core tcl scripts in path
    lappend auto_path $dir/tcl
    rename load_pkg_Pltk {}
}

package ifneeded Pltk 5.15.0 [list load_pkg_Pltk $dir]
# Load PLplot tkwin extension which is called Plplotter

# Copyright (C) 2004-2015 Alan W. Irwin
# Copyright (C) 2004  Joao Cardoso
#
# This file is part of PLplot.
#
# PLplot is free software; you can redistribute it and/or modify
# it under the terms of the GNU Library General Public License as published
# by the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# PLplot is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Library General Public License for more details.
#
# You should have received a copy of the GNU Library General Public License
# along with PLplot; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA

# This proc takes care of finding & loading the proper driver.

# The original version dealt with the possibility that either or both $dir and
# $file may contain spaces, or characters like []{}, but this hasn't been
# tested for this version.

proc load_pkg_Plplotter {dir} {
    global pllibrary tcl_platform auto_path
    #puts "loading Plplotter, dir: $dir"
    set pllibrary $dir

    set thisDir [pwd]
    # Find absolute path of $dir
    cd $dir
    set pkgIndexDir [pwd]
    cd $thisDir
    set buildDir "/Users/jrn/Fortran/plplot-5.15.0-gtk4/build"
    set bLen [string length $buildDir]
    # If pkgIndexDir is in the build tree...
    if {![string compare -length $bLen $buildDir $pkgIndexDir]} then {
	set searchdirs [list "/Users/jrn/Fortran/plplot-5.15.0-gtk4/build/drivers"]
    } else {
	set searchdirs [list "/usr/local/lib/plplot5.15.0/drivers"]
    }
    #puts $searchdirs
    set dlnames [list "tkwin.so"]
    set file ""
    foreach reldir $searchdirs {
	foreach dlname $dlnames {
	    set driver [file join $dir $reldir $dlname]
	    #puts "looking for driver file: $driver"
	    if [file exists $driver] {
		set file $driver
		break
	    }
	}
	if { $file != "" } { break }
    }
    if { $file == "" } {
	error "load_pkg_Plplotter: could not find loadable driver"
    }

    load $file Plplotter
    # put core tcl scripts in path
    lappend auto_path $dir/tcl
    rename load_pkg_Plplotter {}
}

package ifneeded Plplotter 5.15.0 [list load_pkg_Plplotter $dir]
