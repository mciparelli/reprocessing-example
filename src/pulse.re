open Reprocessing;

type state = {
  angle: int,
  mousePressed: bool
};

let setup = env => {
  Env.size(~width=600, ~height=700, env);
  Draw.noStroke(env);
  Draw.background(Utils.color(~r=102, ~g=102, ~b=102, ~a=0), env);
  Draw.fill(Utils.color(~r=0, ~g=0, ~b=0, ~a=102), env);
  {angle: 0, mousePressed: false};
};

let doDraw = (angle, env) => {
  let newAngle = angle + 5;
  print_endline(string_of_int(newAngle));
  print_endline("angle");
  print_endline(string_of_float(Utils.radians(float_of_int(newAngle))));
  print_endline("radians");
  print_endline(string_of_float(cos(Utils.radians(float_of_int(newAngle)))));
  print_endline("cos");
  let radians = Utils.radians(float_of_int(newAngle));
  let val_ = abs_float(cos(radians) *. 12.0);
  let mousef = () => {
    let (mouseX, mouseY) = Env.mouse(env);
    (float_of_int(mouseX), float_of_int(mouseY));
  };
  for (v in 0 to 4) {
    let (mouseX, mouseY) = mousef();
    let radians = Utils.radians(float_of_int(v) *. 75.0);
    let xoff = cos(radians) *. val_;
    let yoff = sin(radians) *. val_;
    Draw.fill(Utils.color(~r=0, ~g=255, ~b=0, ~a=255), env);
    print_endline(string_of_float(mouseX +. xoff));
    print_endline(string_of_float(mouseY +. yoff));
    print_endline(string_of_float(val_));
    Draw.ellipsef(~center=(mouseX +. xoff, mouseY +. yoff), ~radx=val_, ~rady=val_, env);
  };
  Draw.fill(Utils.color(~r=255, ~g=255, ~b=255, ~a=255), env);
  let (mouseX, mouseY) = mousef();
  Draw.ellipsef(~center=(mouseX, mouseY), ~radx=2.0, ~rady=2.0, env);
  newAngle;
};

let draw = ({mousePressed, angle}, env) =>
  switch mousePressed {
  | false => {mousePressed, angle}
  | true => {mousePressed, angle: doDraw(angle, env)}
  };

let mouseDown = (state, env) => {...state, mousePressed: true};

let mouseUp = (state, env) => {...state, mousePressed: false};

run(~setup, ~draw, ~mouseDown, ~mouseUp, ());
