// const { CognitoUserAttribute, CognitoUserPool } = require('amazon-cognito-identity-js');
// const { userPoolId, clientId } = require('./cognito_ids');

// const poolData = {
//   UserPoolId: userPoolId,
//   ClientId: clientId,
// };

// const userPool = new CognitoUserPool(poolData);
function auth(app) {
  // app.ports.signup.subscribe(({ emailAddress, username, password, phoneNumber }) => {
  //   const attributeEmail = new CognitoUserAttribute({
  //     Name: 'email',
  //     Value: emailAddress,
  //   });

  //   const attributePhoneNumber = new CognitoUserAttribute({
  //     Name: 'phone_number',
  //     Value: phoneNumber,
  //   });

  //   const attributeList = [attributeEmail, attributePhoneNumber];

  //   userPool.signUp(username, password, attributeList, null, (err, result) => {
  //     if (err) {
  //       const errorMessage = err.message || JSON.stringify(err);
  //       app.ports.signupFailed.send(errorMessage);
  //       return;
  //     }
  //     const cognitoUser = result.user;
  //     console.log(`result.user: ${result.user}`);
  //     app.ports.signupSuccess.send(cognitoUser.getUsername());
  //   });
  // });

  app.ports.attemptLogIn.subscribe(({ emailAddressOrUsername, password }) => {
    console.log(`attempt log in with ${emailAddressOrUsername} and ${password}`);
    app.ports.logInFailed.send({ errorMessage: 'Login failed. Incorrect username/email address or/and password.' });
    // app.ports.loginSuccess.send(cognitoUser.getUsername());
  });
}

export default auth;
