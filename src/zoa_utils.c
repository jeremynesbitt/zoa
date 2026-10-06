/* This code was adapted from GIMP to get URLS for Zoa help files
 */

 /* #include "config.h" */



#include <string.h> /* strlen, strstr */
#include <stdio.h>
#include <stdlib.h>

#ifdef MACOS
#include <Cocoa/Cocoa.h>
#include <dirent.h>
#include <sys/types.h>
#endif

#ifdef WINDOWS
#include <windows.h>
#include <shellapi.h>
#endif

#include <gio/gio.h>
#include <glib/gstdio.h>

#ifdef _WIN32
#include <io.h>
#define dup _dup
#define dup2 _dup2
#define close _close
#define fileno _fileno
#else
#include <unistd.h>
#endif

/* Preserve the original stdout destination, including shell redirection. */
int zoa_stdout_redirect(int saved)
{
    FILE *sink;
    int result;
    fflush(stdout);
    if (saved >= 0) {
        result = dup2(saved, fileno(stdout));
#ifdef _WIN32
        SetStdHandle(STD_OUTPUT_HANDLE, (HANDLE)_get_osfhandle(fileno(stdout)));
#endif
        close(saved);
        return result;
    }
    saved = dup(fileno(stdout));
    if (saved < 0) return -1;
#ifdef _WIN32
    sink = fopen("NUL", "w");
#else
    sink = fopen("/dev/null", "w");
#endif
    if (!sink) { close(saved); return -1; }
    result = dup2(fileno(sink), fileno(stdout));
#ifdef _WIN32
    SetStdHandle(STD_OUTPUT_HANDLE, (HANDLE)_get_osfhandle(fileno(stdout)));
#endif
    fclose(sink);
    if (result < 0) { close(saved); return -1; }
    return saved;
}

int zoa_plot_temp_dir(char *buffer, int capacity)
{
    static gchar *directory = NULL;
    if (!directory) directory = g_dir_make_tmp("zoa-plots-XXXXXX", NULL);
    if (!directory || strlen(directory) >= (size_t)capacity) return -1;
    g_strlcpy(buffer, directory, capacity);
    return 0;
}

/* The absolute, normalized form of path (relative to the current directory)
   in buffer (0 = ok). */
int zoa_absolute_path(const char *path, char *buffer, int capacity)
{
    gchar *abs = g_canonicalize_filename(path, NULL);
    int rc = -1;
    if (abs && strlen(abs) < (size_t)capacity) {
        g_strlcpy(buffer, abs, capacity);
        rc = 0;
    }
    g_free(abs);
    return rc;
}

/* Change the current directory (0 = ok). */
int zoa_change_dir(const char *path)
{
    return g_chdir(path) == 0 ? 0 : -1;
}

/* Create a directory and any missing parents (0 = ok). */
int zoa_make_dir(const char *path)
{
    return g_mkdir_with_parents(path, 0755) == 0 ? 0 : -1;
}

int zoa_copy_file(const char *source, const char *destination)
{
    GFile *src = g_file_new_for_path(source);
    GFile *dst = g_file_new_for_path(destination);
    gboolean ok = g_file_copy(src, dst, G_FILE_COPY_OVERWRITE, NULL, NULL, NULL, NULL);
    g_object_unref(src);
    g_object_unref(dst);
    return ok ? 0 : -1;
}

void zoa_configure_plplot_runtime(void)
{
#ifdef _WIN32
    static gboolean initialized = FALSE;
    wchar_t filename[32768];
    HMODULE module = GetModuleHandleW(L"plplot.dll");
    gchar *utf8, *bin, *drivers, *candidate, *path;
    const gchar *configured = g_getenv("PLPLOT_DRV_DIR");
    if (initialized) return;
    if (!module) module = GetModuleHandleW(L"libplplot.dll");
    if (configured && *configured) {
        drivers = g_strdup(configured);
    } else {
        if (!module || !GetModuleFileNameW(module, filename, G_N_ELEMENTS(filename))) return;
        utf8 = g_utf16_to_utf8((const gunichar2 *)filename, -1, NULL, NULL, NULL);
        if (!utf8) return;
        bin = g_path_get_dirname(utf8);
        g_free(utf8);
        drivers = g_build_filename(bin, "plplot5.15.0", "drivers", NULL);
        candidate = g_build_filename(drivers, "cairo.dll", NULL);
        if (!g_file_test(candidate, G_FILE_TEST_IS_REGULAR)) {
            g_free(drivers);
            drivers = g_build_filename(bin, "..", "lib", "plplot5.15.0", "drivers", NULL);
        }
        g_free(candidate);
        g_free(bin);
    }
    candidate = g_build_filename(drivers, "cairo.dll", NULL);
    if (g_file_test(candidate, G_FILE_TEST_IS_REGULAR)) {
        g_setenv("PLPLOT_DRV_DIR", drivers, TRUE);
        /* PLplot's Windows loader loads cairo.dll by basename. */
        path = g_strconcat(drivers, ";", g_getenv("PATH") ? g_getenv("PATH") : "", NULL);
        g_setenv("PATH", path, TRUE);
        g_free(path);
        initialized = TRUE;
    }
    g_free(candidate);
    g_free(drivers);
#endif
}

//#include <CoreFoundation/CoreFoundation.h>
//#include <CoreServices/CoreServices.h>
//#include <ApplicationServices/ApplicationServices.h>
//#include <sys/types.h>

 /* #include <gtk/gtk.h> */


void list_files(const char *directory) {
#ifdef WINDOWS
    WIN32_FIND_DATA findFileData;
    HANDLE hFind = INVALID_HANDLE_VALUE;

    char path[MAX_PATH];
    snprintf(path, sizeof(path), "%s\\*", directory);

    hFind = FindFirstFile(path, &findFileData);
    if (hFind == INVALID_HANDLE_VALUE) {
        printf("Error opening directory: %s\n", directory);
        return;
    }

    do {
        if (!(findFileData.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY)) {
            printf("%s\n", findFileData.cFileName);
        }
    } while (FindNextFile(hFind, &findFileData) != 0);

    FindClose(hFind);

#endif

#ifdef MACOS
    DIR *dir = opendir(directory);
    struct dirent *entry;

    if (dir == NULL) {
        perror("Error opening directory");
        return;
    }

    while ((entry = readdir(dir)) != NULL) {
        if (entry->d_type != DT_DIR) {  // Ignore directories
            printf("%s\n", entry->d_name);
        }
    }

    closedir(dir);
#endif
}


gboolean
browser_open_url (const char  *url)
{

#ifdef WINDOWS

  GFile *file = g_file_new_for_path(url);
  gchar *uri = g_file_get_uri(file);
  GError *error = NULL;
  gboolean result = g_app_info_launch_default_for_uri(uri, NULL, &error);
  if (error) {
    g_warning("Unable to open help: %s", error->message);
    g_error_free(error);
  }
  g_free(uri);
  g_object_unref(file);
  return result;
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
    gchar              nullStr[] = "null";
    gchar             *nullResult = nullStr ;
    
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
    return nullResult;
   
}
