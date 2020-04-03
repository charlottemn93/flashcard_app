# flashcard_app
Flashcard application in Elm - client side

# Set up

npm i

# To build

./build.sh - builds the Elm code and JS code

# To run

./develop.sh - builds the Elm code and uses elm-live to build and reload the app on save.

This uses AWS Cognito for sign up/verification/log in

Create a js/cognito_ids.js which will include all cognito pool data:
```function logins(result) {
  return {
    'cognito-idp.<REGION>.amazonaws.com/<USER_POOL_ID>': result.getIdToken().getJwtToken(),
  };
}

module.exports = {
  clientId: '...', /* your client app id */
  identityPoolId: '...',/* your identity pool id */
  userPoolId: '...',/* your user pool id */
  logins,
};```

Please see https://aws.amazon.com/getting-started/hands-on/build-serverless-web-app-lambda-apigateway-s3-dynamodb-cognito/ for the infrastructure, you will need to follow these steps to create a cognito pool and upload the contents of `public elm` to your S3 bucket.
