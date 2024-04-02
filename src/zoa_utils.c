/* GIMP - The GNU Image Manipulation Program
 * Copyright (C) 1995 Spencer Kimball and Peter Mattis
 *
 * Web Browser Plug-in
 * Copyright (C) 2003  Henrik Brix Andersen <brix@gimp.org>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */

 /* #include "config.h" */



#include <string.h> /* strlen, strstr */

#ifdef MACOS
#include <Cocoa/Cocoa.h>
#endif

#ifdef WINDOWS
#include <windows.h>
#endif

#include <gio/gio.h>
#include <glib/gstdio.h>

//#include <CoreFoundation/CoreFoundation.h>
//#include <CoreServices/CoreServices.h>
//#include <ApplicationServices/ApplicationServices.h>
//#include <sys/types.h>

 /* #include <gtk/gtk.h> */


gboolean
browser_open_url (const char  *url)
{

#ifdef WINDOWS

  HINSTANCE hinst = ShellExecute (GetDesktopWindow(),
                                  "open", url, NULL, NULL, SW_SHOW);
#endif


#ifdef MACOS
  NSURL    *ns_url;
  gboolean  retval;

  @autoreleasepool
    {
      ns_url = [NSURL URLWithString: [NSString stringWithUTF8String: url]];
      retval = [[NSWorkspace sharedWorkspace] openURL: ns_url];
    }


  return retval;
#endif

}

const gchar *
get_macos_bundle_dir()
{

    
#ifdef MACOS    
    NSAutoreleasePool *pool;
    NSString          *resource_path;
    gchar             *basename;
    gchar             *basepath;
    gchar             *dirname;

    pool = [[NSAutoreleasePool alloc] init];

    resource_path = [[NSBundle mainBundle] resourcePath];

    basename = g_path_get_basename ([resource_path UTF8String]);
    basepath = g_path_get_dirname ([resource_path UTF8String]);
    dirname  = g_path_get_basename (basepath);

    return basepath;
#endif
   
   return 'null';

}
