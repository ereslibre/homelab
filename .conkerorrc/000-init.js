url_completion_use_history = false;
url_completion_use_bookmarks = true;
can_kill_last_buffer = true;

cwd = get_home_directory();
cwd.append("Downloads");

homepage = "https://start.duckduckgo.com";

define_webjump("d", "http://www.duckduckgo.com/?q=%s");
define_webjump("g", "http://www.google.com/search?q=%s");
define_webjump("w", "https://en.wikipedia.org/w/index.php?search=%s");
define_webjump("ws", "https://es.wikipedia.org/w/index.php?search=%s");

define_webjump("bsc", "https://bugzilla.suse.com/show_bug.cgi?id=%s");
define_webjump("bscn", "https://bugzilla.suse.com/buglist.cgi?cmdtype=runnamed&namedcmd=%s");


session_pref("general.useragent.compatMode.firefox", true);
session_pref("xpinstall.whitelist.required", false);
