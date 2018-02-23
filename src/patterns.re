open Reprocessing;

let setup = env => Env.size(~width=640, ~height=360, env);

let draw = ((), env) => {
  let (mouseX, mouseY) = Env.mouse(env);
  let (pmouseX, pmouseY) = Env.pmouse(env);
  let speed = abs(mouseX - pmouseX) + abs(mouseY - pmouseY);
  let color = float_of_int(speed);
  Draw.stroke({r: color, g: color, b: color, a: 1.0}, env);
  Draw.ellipse(~center=(mouseX, mouseY), ~radx=speed, ~rady=speed, env);
  ();
};

run(~setup, ~draw, ());
