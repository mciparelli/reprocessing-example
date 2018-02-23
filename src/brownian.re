open Reprocessing;

let coordsCount = 2000;

let range = 600;

type coords = (int, int);

type state = Queue.t(coords);

let width = 640;

let height = 360;

let setup = env => {
  Env.size(~width, ~height, env);
  let queue = Queue.create();
  for (_ in 1 to coordsCount) {
    Queue.add((width / 2, height / 2), queue);
  };
  Draw.background(Constants.white, env);
  queue;
};

let draw = (queue, env) => {
  let p1 = Queue.pop(queue);
  let (lastElementX, lastElementY) = Queue.pop(queue);
  let rnd = () => Utils.random(~min=- range, ~max=range);
  let randomInRange1 = rnd();
  let randomInRange2 = rnd();
  let newCoordX = Utils.constrain(~amt=lastElementX + randomInRange1, ~low=0, ~high=width);
  let newCoordY = Utils.constrain(~amt=lastElementY + randomInRange2, ~low=0, ~high=height);
  let p2 = (newCoordX, newCoordY);
  Queue.add(p2, queue);
  let i = Utils.random(~min=1, ~max=Queue.length(queue));
  let color = float_of_int(i) /. float_of_int(Queue.length(queue)) *. 204.0 +. 255.0;
  Draw.stroke({r: color, g: color, b: color, a: 1.0}, env);
  Draw.line(~p1, ~p2, env);
  queue;
};

run(~setup, ~draw, ());
