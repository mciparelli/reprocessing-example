open Reprocessing;

let screenWidth = 1024 * 4;

let screenHeight = 720 * 4;

let xStart = 0;

let yStart = 250;

let segmentSize = 30;

type coords = (int, int);

type direction =
  | Left
  | Right
  | Up
  | Down;

type state = {
  snakeCoordsQueue: Queue.t(coords),
  latestSnakeCoords: coords,
  fruitCoords: coords,
  score: int,
  direction: option(direction),
  running: bool
};

let coordsMatch = ((x, y), (candX, candY)) => x === candX && y === candY;

let getRandomCoord = bound =>
  int_of_float(floor(float_of_int(Utils.random(~min=10, ~max=(bound - 100) / 10))) *. 10.);

let getRandomCoords = (~width, ~height) => (getRandomCoord(width), getRandomCoord(height));

let getInitialSnakeCoords = numSegments => {
  let snakeCoordsQueue = Queue.create();
  /* initially display horizontally */
  for (segmentNumber in 1 to numSegments - 1) {
    Queue.add((xStart + segmentNumber * segmentSize, yStart), snakeCoordsQueue);
  };
  let latestSnakeCoords = (xStart + numSegments * segmentSize, yStart);
  Queue.add(latestSnakeCoords, snakeCoordsQueue);
  (snakeCoordsQueue, latestSnakeCoords);
};

let getNewSnakeCoords = (~latestSnakeCoords as (x, y), ~direction, ()) =>
  switch direction {
  | Some(Left) => (x - segmentSize, y)
  | Some(Right) => (x + segmentSize, y)
  | Some(Up) => (x, y - segmentSize)
  | Some(Down) => (x, y + segmentSize)
  | None => (x, y)
  };

let drawSnake = (snakeCoordsQueue, env) =>
  Queue.fold(
    (p1, p2) => {
      Draw.line(~p1, ~p2, env);
      p2;
    },
    Queue.peek(snakeCoordsQueue),
    snakeCoordsQueue
  )
  |> ignore;

let moveSnake = (~latestSnakeCoords, ~snakeCoordsQueue, ~direction) => {
  let latestSnakeCoords = getNewSnakeCoords(~latestSnakeCoords, ~direction, ());
  if (direction != None) {
    Queue.add(latestSnakeCoords, snakeCoordsQueue);
    Queue.take(snakeCoordsQueue) |> ignore;
  };
  latestSnakeCoords;
};

let setup = env => {
  Env.size(~width=500, ~height=500, env);
  Draw.stroke(Constants.white, env);
  Draw.strokeWeight(10, env);
  let numSegments = 10;
  let score = 0;
  let fruitCoords = getRandomCoords(~width=screenWidth, ~height=screenHeight);
  let (snakeCoordsQueue, latestSnakeCoords) = getInitialSnakeCoords(numSegments);
  let direction = None;
  let running = true;
  {fruitCoords, snakeCoordsQueue, latestSnakeCoords, score, direction, running};
};

let getCurrentDirection = (direction, env) => {
  let pressed = key => Env.keyPressed(key, env);
  switch (pressed(Left), pressed(Right), pressed(Up), pressed(Down)) {
  | (true, false, false, false) => Some(Left)
  | (false, true, false, false) => Some(Right)
  | (false, false, true, false) => Some(Up)
  | (false, false, false, true) => Some(Down)
  | (_, _, _, _) => direction
  };
};

let snakeHitItself = (~latestSnakeCoords, ~snakeCoordsQueue) => {
  let auxQueue = Queue.copy(snakeCoordsQueue);
  let newQueue = Queue.create();
  for (_ in 1 to Queue.length(snakeCoordsQueue) - 1) {
    Queue.add(Queue.pop(auxQueue), newQueue);
  };
  Queue.fold(
    (acc, candCoord) =>
      if (coordsMatch(latestSnakeCoords, candCoord)) {
        true;
      } else {
        acc;
      },
    false,
    newQueue
  );
};

let isOutOfBounds = ((x, y)) => x < 0 || x > screenWidth || y < 0 || y > screenHeight;

let runApp = (state, env) => {
  let {direction, snakeCoordsQueue, latestSnakeCoords, fruitCoords, score} = state;
  let direction = getCurrentDirection(direction, env);
  let latestSnakeCoords = moveSnake(~latestSnakeCoords, ~snakeCoordsQueue, ~direction);
  let lost =
    isOutOfBounds(latestSnakeCoords) || snakeHitItself(~latestSnakeCoords, ~snakeCoordsQueue);
  let running = ! lost;
  let newState = {...state, latestSnakeCoords, direction, running};
  let ateFruit = coordsMatch(latestSnakeCoords, fruitCoords);
  if (ateFruit) {
    let score = score + 1;
    let newQueue = Queue.create();
    Queue.add(Queue.peek(snakeCoordsQueue), newQueue);
    Queue.transfer(snakeCoordsQueue, newQueue);
    let snakeCoordsQueue = newQueue;
    let fruitCoords = getRandomCoords(~width=screenWidth, ~height=screenHeight);
    {...newState, fruitCoords, snakeCoordsQueue, score};
  } else {
    newState;
  };
};

let draw = (state, env) => {
  Draw.background(Constants.black, env);
  drawSnake(state.snakeCoordsQueue, env);
  Draw.pixel(~pos=state.fruitCoords, ~color=Constants.white, env);
  if (state.running) {
    runApp(state, env);
  } else {
    Draw.text(
      ~font=Draw.loadFont(~filename="assets/font.fnt", env),
      ~body="Score: " ++ string_of_int(state.score),
      ~pos=(20, 20),
      env
    );
    state;
  };
};

run(~setup, ~draw, ());
/* // the snake is divided into small segments, which are drawn and edited on each 'draw' call
   var numSegments = 10;
   var direction = 'right';

   var xStart = 0; //starting x coordinate for snake
   var yStart = 250; //starting y coordinate for snake
   var diff = 10;

   var xCor = [];
   var yCor = [];

   var xFruit = 0;
   var yFruit = 0;
   var scoreElem;

   function setup() {
     scoreElem = createDiv('Score = 0');
     scoreElem.position(20, 20);
     scoreElem.id = 'score';
     scoreElem.style('color', 'white');

     createCanvas(500, 500);
     frameRate(15);
     stroke(255);
     strokeWeight(10);
     updateFruitCoordinates();

     for (var i = 0; i < numSegments; i++) {
       xCor.push(xStart + (i * diff));
       yCor.push(yStart);
     }
   }

   function draw() {
     background(0);
     for (var i = 0; i < numSegments - 1; i++) {
       line(xCor[i], yCor[i], xCor[i + 1], yCor[i + 1]);
     }
     updateSnakeCoordinates();
     checkGameStatus();
     checkForFruit();
   }

   /*
    The segments are updated based on the direction of the snake.
    All segments from 0 to n-1 are just copied over to 1 till n, i.e. segment 0
    gets the value of segment 1, segment 1 gets the value of segment 2, and so on,
    and this results in the movement of the snake.

    The last segment is added based on the direction in which the snake is going,
    if it's going left or right, the last segment's x coordinate is increased by a
    predefined value 'diff' than its second to last segment. And if it's going up
    or down, the segment's y coordinate is affected.
   */
   function updateSnakeCoordinates() {

     for (var i = 0; i < numSegments - 1; i++) {
       xCor[i] = xCor[i + 1];
       yCor[i] = yCor[i + 1];
     }
     switch (direction) {
       case 'right':
         xCor[numSegments - 1] = xCor[numSegments - 2] + diff;
         yCor[numSegments - 1] = yCor[numSegments - 2];
         break;
       case 'up':
         xCor[numSegments - 1] = xCor[numSegments - 2];
         yCor[numSegments - 1] = yCor[numSegments - 2] - diff;
         break;
       case 'left':
         xCor[numSegments - 1] = xCor[numSegments - 2] - diff;
         yCor[numSegments - 1] = yCor[numSegments - 2];
         break;
       case 'down':
         xCor[numSegments - 1] = xCor[numSegments - 2];
         yCor[numSegments - 1] = yCor[numSegments - 2] + diff;
         break;
     }
   }

   /*
    I always check the snake's head position xCor[xCor.length - 1] and
    yCor[yCor.length - 1] to see if it touches the game's boundaries
    or if the snake hits itself.
   */
   function checkGameStatus() {
     if (xCor[xCor.length - 1] > width ||
         xCor[xCor.length - 1] < 0 ||
         yCor[yCor.length - 1] > height ||
         yCor[yCor.length - 1] < 0 ||
         checkSnakeCollision()) {
       noLoop();
       var scoreVal = parseInt(scoreElem.html().substring(8));
       scoreElem.html('Game ended! Your score was : ' + scoreVal);
     }
   }

   /*
    If the snake hits itself, that means the snake head's (x,y) coordinate
    has to be the same as one of its own segment's (x,y) coordinate.
   */
   function checkSnakeCollision() {
     var snakeHeadX = xCor[xCor.length - 1];
     var snakeHeadY = yCor[yCor.length - 1];
     for (var i = 0; i < xCor.length - 1; i++) {
       if (xCor[i] === snakeHeadX && yCor[i] === snakeHeadY) {
         return true;
       }
     }
   }

   /*
    Whenever the snake consumes a fruit, I increment the number of segments,
    and just insert the tail segment again at the start of the array (basically
    I add the last segment again at the tail, thereby extending the tail)
   */
   function checkForFruit() {
     point(xFruit, yFruit);
     if (xCor[xCor.length - 1] === xFruit && yCor[yCor.length - 1] === yFruit) {
       var prevScore = parseInt(scoreElem.html().substring(8));
       scoreElem.html('Score = ' + (prevScore + 1));
       xCor.unshift(xCor[0]);
       yCor.unshift(yCor[0]);
       numSegments++;
       updateFruitCoordinates();
     }
   }

   function updateFruitCoordinates() {
     /*
       The complex math logic is because I wanted the point to lie
       in between 100 and width-100, and be rounded off to the nearest
       number divisible by 10, since I move the snake in multiples of 10.
     */

     xFruit = floor(random(10, (width - 100) / 10)) * 10;
     yFruit = floor(random(10, (height - 100) / 10)) * 10;
   }

   function keyPressed() {
     switch (keyCode) {
       case 74:
         if (direction != 'right') {
           direction = 'left';
         }
         break;
       case 76:
         if (direction != 'left') {
           direction = 'right';
         }
         break;
       case 73:
         if (direction != 'down') {
           direction = 'up';
         }
         break;
       case 75:
         if (direction != 'up') {
           direction = 'down';
         }
         break;
     }
   } */
