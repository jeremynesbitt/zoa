/* This code was adapted from GIMP to get URLS for Zoa help files
 */

 /* #include "config.h" */



#include <string.h> /* strlen, strstr */

#ifdef MACOS
#include <Cocoa/Cocoa.h>
#endif

#ifdef WINDOWS
#include <windows.h>
#include <shellapi.h>
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

  //HINSTANCE hinst = ShellExecute (GetDesktopWindow(),
  //                                "open", url, NULL, NULL, SW_SHOW);
  //char str[1024];
  //strcpy(str, "open ");
  //strcat(str, url);

  system(url);

  return TRUE;                                  
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
