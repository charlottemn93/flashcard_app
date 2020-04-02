import auth from './auth';

document.addEventListener('DOMContentLoaded', () => {
  const idToken = localStorage.getItem('flashcard_app_id_token');
  const accessToken = localStorage.getItem('flashcard_app_access_token');

  const app = window.Elm.Main.init({
    node: document.getElementById('root'),
    flags: {
      idToken: idToken || null,
      accessToken: accessToken || null,
    },
  });

  try {
    auth(app);
  } catch (e) {
    console.log(e);
  }
});
