const FIVE_MINUTES = 60 * 1000 * 5;


const reloadAuthToken = async ({ app, googleUser }) => {
  try {
    await googleUser.reloadAuthResponse();
  } catch (error) {
    localStorage.clear();
    app.ports.googleUserLoggedOut.send();
  }
};

const setRefreshTimeout = async ({ expiresAtInSeconds, app, googleUser }) => {
  // 5 minutes before the deadline should prevent thrashing while keeping the credentials fresh.

  const expiresAtInMilliseconds = expiresAtInSeconds * 1000;
  const refreshDeadline = Math.max(
    FIVE_MINUTES,
    expiresAtInMilliseconds - FIVE_MINUTES,
  );

  await new Promise(resolve => setTimeout(() => {
    reloadAuthToken({ app, googleUser });
    resolve();
  }, refreshDeadline));
};

function auth(app) {
  // Listener method for when the user changes
  const userChanged = async (user) => {
    if (user.uc) {
      localStorage.clear();
      localStorage.setItem('flashcard_app_id_token', user.uc.id_token);
      localStorage.setItem('flashcard_app_access_token', user.uc.access_token);

      app.ports.logInSuccessful.send({
        idToken: user.uc.id_token,
        accessToken: user.uc.access_token,
      });
      await setRefreshTimeout({ expiresAtInSeconds: user.uc.expires_in, app, googleUser: user });
    } else {
      localStorage.clear();
    }
  };

  window.gapi.load('auth2', () => {
    const auth2 = window.gapi.auth2.init({
      client_id: '3aau0dfa94gvi8l3hatjr478ql',
      scope: 'email profile openid',
    });

    // Listen for changes to current user.
    auth2.currentUser.listen(userChanged);

    app.ports.attemptLogIn.subscribe(() => {
      auth2.signIn();
    });
  });
}

export default auth;
