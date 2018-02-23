open Reprocessing;

type coords = (float, float);

type state = {
  start: coords,
  current: coords,
  finish: coords,
  pct: float
};

let screen = (640, 360);

let exponent = 4.0;

let step = 0.01;

let setup = env => {
  let (width, height) = screen;
  Env.size(~width, ~height, env);
  Draw.noStroke(env);
  {start: (20.0, 10.0), current: (0.0, 0.0), finish: (570.0, 320.0), pct: 0.0};
};

let getCurrentCoords = ({start: (startX, startY), current, finish: (finishX, finishY), pct}) =>
  if (pct < 1.0) {
    let distX = finishX -. startX;
    let distY = finishY -. startY;
    let x = startX +. pct *. distX;
    let y = startY +. pct ** exponent *. distY;
    (x, y);
  } else {
    current;
  };

let draw = (state, env) => {
  let pct = state.pct +. step;
  let current = getCurrentCoords(state);
  Draw.fill(Constants.white, env);
  Draw.ellipsef(~center=current, ~radx=20.0, ~rady=20.0, env);
  {...state, pct, current};
};

let mouseDown = ({current}, env) => {
  let (mouseX, mouseY) = Env.mouse(env);
  let pct = 0.0;
  let finish = (float_of_int(mouseX), float_of_int(mouseY));
  {current, pct, finish, start: current};
};

run(~setup, ~draw, ~mouseDown, ());
