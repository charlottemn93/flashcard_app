import * as AWS from 'aws-sdk/global';

const {
  CognitoUserAttribute, CognitoUser, AuthenticationDetails, CognitoUserPool,
} = require('amazon-cognito-identity-js');
const {
  userPoolId, clientId, identityPoolId, logins,
} = require('./cognito_ids');

const poolData = {
  UserPoolId: userPoolId,
  ClientId: clientId,
};

const userPool = new CognitoUserPool(poolData);
function auth(app) {
  app.ports.signup.subscribe(({
    emailAddress, username, password,
  }) => {
    const attributeEmail = new CognitoUserAttribute({
      Name: 'email',
      Value: emailAddress,
    });

    const attributeList = [attributeEmail];

    userPool.signUp(username, password, attributeList, null, (err, result) => {
      if (err) {
        app.ports.signupFailed.send({ errorMessage: err.message || JSON.stringify(err) });
        return;
      }
      console.log(`result.user: ${JSON.stringify(result.user)}`);
      app.ports.signupSuccess.send();
    });
  });

  app.ports.verifyAccount.subscribe(({ username, verificationCode }) => {
    const cognitoUser = new CognitoUser({
      Username: username,
      Pool: userPool,
    });
    cognitoUser.confirmRegistration(verificationCode, true, (err, result) => {
      if (err) {
        app.ports.verificationFailed.send({ errorMessage: err.message || JSON.stringify(err) });
      } else {
        console.log(JSON.stringify(result));
        app.ports.verificationSuccessful.send();
      }
    });
  });

  app.ports.attemptLogIn.subscribe(({ username, password }) => {
    const cognitoUser = new CognitoUser({
      Username: username,
      Pool: userPool,
    });
    const authenticationDetails = new AuthenticationDetails(
      { Username: username, Password: password },
    );
    cognitoUser.authenticateUser(authenticationDetails, {
      onSuccess(result) {
        AWS.config.region = 'eu-west-2';

        AWS.config.credentials = new AWS.CognitoIdentityCredentials({
          IdentityPoolId: identityPoolId,
          Logins: logins(result),
        });
        AWS.config.credentials.refresh((error) => {
          if (error) {
            app.ports.logInFailed.send({ errorMessage: error });
          } else {
            // Instantiate aws sdk service objects now that the credentials have been updated.
            // example: var s3 = new AWS.S3();
            console.log('Successfully logged in!');
            localStorage.setItem('flashcard_app_id_token', result.getIdToken().getJwtToken());
            localStorage.setItem('flashcard_app_access_token', result.getAccessToken().getJwtToken());
          }
        });
      },

      onFailure(err) {
        console.log(`err: ${err}`);
        app.ports.logInFailed.send({ errorMessage: err.message || JSON.stringify(err) });
      },
    });
  });
}

export default auth;
