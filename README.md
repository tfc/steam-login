# Steam-Login Web App Demonstrator

![](https://steamcdn-a.akamaihd.net/steamcommunity/public/images/steamworks_docs/english/sits_large_noborder.png)

This repository contains a small Haskell web application that provides an
[OpenID login](https://openid.net/connect/) via the [Steam platform](https://partner.steamgames.com/doc/features/auth).
I created it in order to learn how OpenID + Steam + [JWT](https://jwt.io/) + Cookies work.

## Implemented User Story

1. User visits `/` and gets an HTTP 401 error (Unauthorized) because they are
   not logged in.
2. User visits `/login` and gets redirected to the Steam login page, where they
   are asked if they really want to login to that service.

   ![steam-login](https://user-images.githubusercontent.com/29044/194697519-f3f64fcb-71c4-4531-a0ef-d41fbade863a.png)

3. After clicking the "Sign in" button, Steam redirects back to the
   `/login-redirect` route with information about the user.
   - The web app first needs to check this information against a trusted steam
     server, because users could easily forge such redirects.
   - If the information is correct, the Steam ID is extracted from it and more
     user information is obtained via another request to the steam servers
   - The web app then stores the Steam ID and username in a JWT cookie and sends
     it back to the user.

   ![login-redirect-hello](https://user-images.githubusercontent.com/29044/194697647-9afb5386-4c88-4732-b9ef-5315c0cd67a8.png)

4. Another visit on `/` reveals that the web app now recognizes the user.
   They are logged in!

   ![logged-in](https://user-images.githubusercontent.com/29044/194697659-4262110c-e1af-427d-a6ba-9238b105b1a8.png)

## How to Run the Demonstrator App

1. Install [nix](https://nixos.org/download.html)
  (You can build the app yourself via manual installation of Cabal and GHC, which is also straightforward but out of scope)
2. Obtain a [steam web API key](https://steamcommunity.com/dev/apikey)
3. Run

   ```bash
   nix run github:tfc/steam-login -- --steam-client-key <web client key from steam>
   ```

4. Browse to `http://localhost:8000/login`
