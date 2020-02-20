# AufAchseKartenSpiel

Creating the famous card gane "Auf Achse" with elm.

## Run Locally

Start by cloning the repo...
```
$ git clone https://github.com/Boman/AufAchseKartenSpiel.git && cd AufAchseKartenSpiel
```

### Dependencies

Install elm. Installation instructions can be found [here](https://guide.elm-lang.org/install.html)

### Run with elm reactor

To get to project up and running just do the following steps...
```
$ elm reactor
```
Visit http://localhost:8000/src/Main.elm in your browser

And that's it 🎉🎉🎉🎉🎉

### Run in dev mode

If you want to run this application in a dev environment I would recommend
installing [elm-live](https://github.com/wking-io/elm-live). Installation
instructions can be found
[here](https://github.com/wking-io/elm-live#installation)

Once you have installed `elm-live`, do the following...
```
$ elm-live src/Main.elm --open --dir=public -- --output=public/js/elm.js --debug
```

## Why

I decided to start building this game because I wanted to get more experienced
with elm and also try to implement some nice Deep Reinforcement Learning for an
AI opponent.