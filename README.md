# flashcard_app
Flashcard application in Elm - client side

# Set up

npm i

This uses AWS Cognito for sign up/verification/log in

Create a js/cognito_ids.js which will include all cognito pool data (make sure this file is included in your .gitignore):
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
};
```

Please see https://aws.amazon.com/getting-started/hands-on/build-serverless-web-app-lambda-apigateway-s3-dynamodb-cognito/ for the infrastructure, you will need to follow these steps to create a cognito pool and upload the contents of `public-elm` to your S3 bucket and host a static website from your S3 bucket.

# To build

./build.sh - builds the Elm code and JS code

# To run

./develop.sh - builds the Elm code and uses elm-live to build and reload the app on save.

# Test the app out

This is what I have built: http://flashcardapp-charlotte-neill.s3-website.eu-west-2.amazonaws.com

# Progress

Things left to do:

* When we do the shuffle - do not show current flashcard in shuffle (BUG)
* Two options - shuffle and click through in order. 
* Make sticky notes and buttons go in the middle
* Create a nav bar at the top and create a sign out on there
* Forgot password functionality
* lambda integration to save/load flashcards for user


