# AufAchseKartenSpiel

Creating the famous card game "Auf Achse" with elm.

## Run Locally

Start by cloning the repo...
```
$ git clone https://github.com/Boman/AufAchseKartenSpiel.git && cd AufAchseKartenSpiel
```

### Dependencies

Install elm. Installation instructions can be found [here](https://guide.elm-lang.org/install.html).

To install all packages required for the project just do the following steps...
```
$ elm make
```

### Run with elm reactor

To get the project up and running just do the following steps...
```
$ elm reactor
```
Visit http://localhost:8000/src/Main.elm in your browser.

And that's it 🎉🎉🎉🎉🎉

### Run in dev mode

If you want to run this application in a dev environment I would recommend installing
[elm-live](https://github.com/wking-io/elm-live). Installation instructions can be found
[here](https://github.com/wking-io/elm-live#installation).

Once you have installed `elm-live`, do the following...
```
$ elm-live src/Main.elm --open --dir=public -- --output=public/js/elm.js --debug
```

## Why

I decided to start building this game because I wanted to get more experienced with elm and also try to implement some
nice Deep Reinforcement Learning for an
AI opponent.

## Credits

Credits go to Wolfgang Kramer for creating such a marvellous game.

Icons made by [Freepik](https://www.flaticon.com/de/autoren/freepik) and
[photo3idea-studio](https://www.flaticon.com/de/autoren/photo3idea-studio) from [flaticon](www.flaticon.com).