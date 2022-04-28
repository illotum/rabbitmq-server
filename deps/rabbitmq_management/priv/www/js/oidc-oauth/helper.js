
//import {UserManager} from "./scripts/oidc-client-ts/oidc-client-ts.js"
// import {axios} from "./scripts/axios/axios.min.js"


var mgr;

function initializeOAuthIfRequired() {
    rabbit_port = window.location.port ? ":" +  window.location.port : ""
    rabbit_base_uri = window.location.protocol + "//" + window.location.hostname + rabbit_port

    var request = new XMLHttpRequest();
    request.open('GET', rabbit_base_uri + '/api/auth', false);
    request.send(null);
    if (request.status === 200) {
        return initializeOAuth(JSON.parse(request.responseText));
    }else {
        return { "enable" : false };
    }

}


function initializeOAuth(authSettings) {
    oauth = {
      "logged_in": false,
      "enable" : authSettings.oauth_enable,
      "client_id" : authSettings.oauth_client_id,
      "url" : authSettings.oauth_url,
      "readiness_url" : authSettings.oauth_url + "/.well-known/openid-configuration"
    }

    if (!oauth.enable) return oauth;

    oidcSettings = {
        //userStore: new WebStorageStateStore({ store: window.localStorage }),
        authority: authSettings.oauth_url,
        client_id: authSettings.oauth_client_id,
        client_secret: authSettings.oauth_client_secret,
        response_type: "code",
        scope: "openid profile rabbitmq.*",
        resource: authSettings.oauth_resource_id,
        redirect_uri: rabbit_base_uri + "/js/oidc-oauth/login-callback.html",
        post_logout_redirect_uri: rabbit_base_uri + "/js/oidc-oauth/logout-callback.html",

        response_type: "code",
        filterProtocolClaims: true,
        revokeAccessTokenOnSignout: true,
    };
    if (authSettings.enable_uaa) {
      oidcSettings.metadataSeed = {
        end_session_endpoint: authSettings.oauth_url + '/logout.do'
      }
    }

    mgr = new oidc.UserManager(oidcSettings);
    oidc.Log.setLogger(console);
    oidc.Log.setLevel(oidc.Log.INFO);

    return oauth;
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
  location.href = "/"
}
function redirectToLogin() {
  location.href = "/"
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
