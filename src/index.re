open Reprocessing;

module Snake = {
  type snakeT = (int, int);

  let baseSize: int = 10;
  let step: int = 10;
  let initialState: list(snakeT) = [(50, 50), (50, 60), (50, 70)];

  let move = (snake: list(snakeT)): list(snakeT) =>
    List.map(((x, y)) => (x, y + step), snake);
};

module Gameboard = {
  let redrawThreshold: float = 0.5;
  let initialDrawingTime: float = 0.;

  let drawBoard = (env: glEnvT): unit => {
    Draw.background(Utils.color(~r=255, ~g=217, ~b=229, ~a=255), env);
    Draw.fill(Utils.color(~r=41, ~g=166, ~b=244, ~a=255), env);
  };

  let drawSnake = (snake, env: glEnvT): unit =>
    List.iter(
      pos =>
        Draw.rect(~pos, ~width=Snake.baseSize, ~height=Snake.baseSize, env),
      snake,
    );

  let init = (snake: list(Snake.snakeT), env: glEnvT): unit => {
    drawBoard(env);
    drawSnake(snake, env);
  };
};

type stateT = {
  totalDrawingTime: float,
  snake: list(Snake.snakeT),
};

let initialState: stateT = {
  totalDrawingTime: Gameboard.initialDrawingTime,
  snake: Snake.initialState,
};

let setup = (env: glEnvT): stateT => {
  Env.size(~width=600, ~height=600, env);
  initialState;
};

let draw = (state: stateT, env: glEnvT): stateT => {
  let {snake, totalDrawingTime} = state;
  let deltaTime = Env.deltaTime(env);
  let totalDrawingTime = totalDrawingTime +. deltaTime;

  Gameboard.init(snake, env);

  totalDrawingTime > Gameboard.redrawThreshold ?
    {
      snake: Snake.move(snake),
      totalDrawingTime: Gameboard.initialDrawingTime,
    } :
    {snake, totalDrawingTime};
};

run(~setup, ~draw, ());