open Reprocessing;

module Snake = {
  let baseSize = 10;
  let step = 10;
  let initialState = [(50, 50), (50, 60), (50, 70)];

  let move = snake => List.map(((x, y)) => (x, y + step), snake);
};

module Gameboard = {
  let redrawThreshold = 0.5;
  let initialDrawingTime = 0.;

  let drawBoard = (env) => {
    Draw.background(Utils.color(~r=255, ~g=217, ~b=229, ~a=255), env);
    Draw.fill(Utils.color(~r=41, ~g=166, ~b=244, ~a=255), env);
  };

  let drawSnake = (snake, env) =>
    List.iter(
      pos => Draw.rect(~pos, ~width=Snake.baseSize, ~height=Snake.baseSize, env),
      snake,
    );

  let init = (snake, env) => {
    drawBoard(env);
    drawSnake(snake, env)
  };
};

type stateT = {
  totalDrawingTime: float,
  snake: list((int, int)),
};

let initialState = {
  totalDrawingTime: Gameboard.initialDrawingTime,
  snake: Snake.initialState,
};

let setup = env => {
  Env.size(~width=600, ~height=600, env);
  initialState;
};

let draw = ({snake, totalDrawingTime}, env) => {
  let deltaTime = Env.deltaTime(env);
  let totalDrawingTime = totalDrawingTime +. deltaTime;

  Gameboard.init(snake, env);

  totalDrawingTime > Gameboard.redrawThreshold ?
    {snake: Snake.move(snake), totalDrawingTime: Gameboard.initialDrawingTime} :
    {snake, totalDrawingTime};
};

run(~setup, ~draw, ());