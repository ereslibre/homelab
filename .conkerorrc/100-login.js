/*
  Implements new login-manager.
  See:
  - http://librelist.com/browser//conkeror/2013/11/28/login-manager-xulrunner-24-0/
  - https://blog.mozilla.org/dolske/2013/08/21/a-change-in-password-manager/

  (C) Copyright 2014 thorkill
  BSD License
*/

let Cu = Components.utils;
Cu.import("resource://gre/modules/XPCOMUtils.jsm");
XPCOMUtils.defineLazyModuleGetter(this,
                                  "LoginManagerContent",
                                  "resource://gre/modules/LoginManagerContent.jsm");
XPCOMUtils.defineLazyModuleGetter(this,
                                  "LoginManagerParent",
                                  "resource://gre/modules/LoginManagerParent.jsm");

define_buffer_local_hook("content_buffer_dom_form_has_password_hook");
define_buffer_local_hook("content_buffer_dom_auto_complete_hook");

session_pref("signon.rememberSignons", true);
session_pref("signon.expireMasterPassword", false);
session_pref("signon.SignonFileName", "signons.txt");
session_pref("signon.useDOMFormHasPassword", true);

Cc["@mozilla.org/login-manager;1"].getService(Ci.nsILoginManager);

LoginManagerParent.init();

add_hook("create_buffer_hook", function (buffer) {

        buffer.browser.addEventListener("DOMFormHasPassword", function(event) {
            content_buffer_dom_form_has_password_hook.run(buffer, event);
        }, true /* captrue */);

        buffer.browser.addEventListener("DOMAutoComplete", function(event) {
            content_buffer_dom_auto_complete_hook.run(buffer, event);
        }, true /* captrue */);

        buffer.browser.addEventListener("blur", function(event) {
            content_buffer_dom_auto_complete_hook.run(buffer, event);
        }, true /* captrue */);

        buffer.browser.addEventListener("change", function(event) {
            content_buffer_dom_auto_complete_hook.run(buffer, event);
        }, true /* captrue */);

});

add_hook("content_buffer_dom_form_has_password_hook", function(buffer, event) {
    // Sometimes onFormPassword is undefined
    if (LoginManagerContent.onFormPassword) {
        LoginManagerContent.onFormPassword(event);
    }
});

add_hook("content_buffer_dom_auto_complete_hook", function(buffer, event) {
        LoginManagerContent.onUsernameInput(event);
});
