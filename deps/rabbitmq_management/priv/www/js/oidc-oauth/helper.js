
//import {UserManager} from "./scripts/oidc-client-ts/oidc-client-ts.js"
// import {axios} from "./scripts/axios/axios.min.js"


var settings = {
  response_type: "code",

  filterProtocolClaims: true,
  revokeAccessTokenOnSignout: true,
};
var mgr;

function initOidcClient(externalSettings, rabbit_base_uri) {
  externalSettings.redirect_uri = rabbit_base_uri + "/js/oidc-oauth/login-callback.html",
  externalSettings.post_logout_redirect_uri = rabbit_base_uri + "/js/oidc-oauth/logout-callback.html",

  Object.assign(settings, externalSettings);

  mgr = new oidc.UserManager(settings);

  oidc.Log.setLogger(console);
  oidc.Log.setLevel(oidc.Log.INFO);

}
function log() {
    message = ""
    Array.prototype.forEach.call(arguments, function(msg) {
        if (msg instanceof Error) {
            msg = "Error: " + msg.message;
        }
        else if (typeof msg !== "string") {
            msg = JSON.stringify(msg, null, 2);
        }
        message += msg
    });
    console.log(message)
}




function registerCallbacks() {
  mgr.events.addUserLoaded(function (user) {
      console.log("addUserLoaded=> ", user);
      mgr.getUser().then(function() {
          console.log("getUser loaded user after userLoaded event fired");
      });
  });
  mgr.events.addUserUnloaded(function (e) {
      console.log("addUserUnloaded=> ", e);
  });

  mgr.events.addUserSignedIn(function (e) {
      log("addUserSignedIn=> " , e);
  });
  mgr.events.addUserSignedOut(function (e) {
      log("addUserSignedOut=> ", e);
  });

}
function isLoggedIn() {
    return mgr.getUser().then(user => {
        if (!user) {
            return { "loggedIn": false };
        }
        return { "user": user, "loggedIn": !user.expired };
    });
}


function initiateLogin() {
    mgr.signinRedirect({ state: { foo: "bar" } /*, useReplaceToNavigate: true*/ }).then(function() {
        log("signinRedirect done");
    }).catch(function(err) {
        console.error(err);
        log(err);
    });
}
function redirectToHome() {
  location.href = "/index.html"
}
function redirectToLogin() {
  location.href = "/login.html"
}
function completeLogin() {
    mgr.signinRedirectCallback().then(user => redirectToHome()).catch(function(err) {
        console.error(err);
        log(err);
    });
}

function initiateLogout() {
    mgr.signoutRedirect();
}
function completeLogout() {
    mgr.signoutRedirectCallback().then(_ => redirectToLogin());
}
