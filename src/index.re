open Reprocessing;

let snakeBaseSize = 10;
let step = 10;
let redrawThreshold = 0.5;
let initialDrawingTime = 0.;
let initialSnake = [(50, 50), (50, 60), (50, 70)];

type stateT = {
  totalDrawingTime: float,
  snake: list((int, int)),
};

let initialState = {
  totalDrawingTime: initialDrawingTime,
  snake: initialSnake,
};

let setup = env => {
  Env.size(~width=600, ~height=600, env);
  initialState;
};

let drawSnake = (snake, env) =>
  List.iter(
    pos => Draw.rect(~pos, ~width=snakeBaseSize, ~height=snakeBaseSize, env),
    snake,
  );

let move = snake => List.map(((x, y)) => (x, y + step), snake);

let draw = ({snake, totalDrawingTime}, env) => {
  let deltaTime = Env.deltaTime(env);
  let totalDrawingTime = totalDrawingTime +. deltaTime;

  Draw.background(Utils.color(~r=255, ~g=217, ~b=229, ~a=255), env);
  Draw.fill(Utils.color(~r=41, ~g=166, ~b=244, ~a=255), env);
  drawSnake(snake, env);

  totalDrawingTime > redrawThreshold ?
    {snake: move(snake), totalDrawingTime: initialDrawingTime} :
    {snake, totalDrawingTime};
};

run(~setup, ~draw, ());