open Reprocessing;

module Snake = {
  type snakeT = (int, int);
  type directionT =
    | Left
    | Right
    | Up
    | Down;

  let baseSize: int = 10;
  let step: int = 10;
  let initialState: list(snakeT) = [(50, 50), (50, 60), (50, 70)];
  let initialDirection: directionT = Down;

  let move = (snake: list(snakeT), nextDirection: directionT): list(snakeT) =>
    List.map(
      ((x, y)) =>
        switch (nextDirection) {
        | Left => (x - step, y)
        | Right => (x + step, y)
        | Up => (x, y - step)
        | Down => (x, y + step)
        },
      snake,
    );
};

module Gameboard = {
  let redrawThreshold: float = 0.5;
  let initialDrawingTime: float = 0.;

  let drawBoard = (env: glEnvT): unit => {
    Draw.background(Utils.color(~r=255, ~g=255, ~b=255, ~a=255), env);
    Draw.fill(Utils.color(~r=0, ~g=0, ~b=0, ~a=255), env);
  };

  let drawSnake = (snake: list(Snake.snakeT), env: glEnvT): unit =>
    List.iter(
      pos =>
        Draw.rect(~pos, ~width=Snake.baseSize, ~height=Snake.baseSize, env),
      snake,
    );

  let handleKeyPressed =
      (env: glEnvT, currentDirection: Snake.directionT): Snake.directionT => {
    let isLeftKeyPressed = Env.keyPressed(Left, env);
    let isRightKeyPressed = Env.keyPressed(Right, env);
    let isUpKeyPressed = Env.keyPressed(Up, env);
    let isDownKeyPressed = Env.keyPressed(Down, env);
    switch (
      isLeftKeyPressed,
      isRightKeyPressed,
      isUpKeyPressed,
      isDownKeyPressed,
    ) {
    | (true, _, _, _) => Snake.Left
    | (_, true, _, _) => Snake.Right
    | (_, _, true, _) => Snake.Up
    | (_, _, _, true) => Snake.Down
    | (_, _, _, _) => currentDirection
    };
  };

  let init = (snake: list(Snake.snakeT), env: glEnvT): unit => {
    drawBoard(env);
    drawSnake(snake, env);
  };
};

type stateT = {
  totalDrawingTime: float,
  currentDirection: Snake.directionT,
  snake: list(Snake.snakeT),
};

let initialState: stateT = {
  totalDrawingTime: Gameboard.initialDrawingTime,
  snake: Snake.initialState,
  currentDirection: Snake.initialDirection,
};

let setup = (env: glEnvT): stateT => {
  Env.size(~width=600, ~height=600, env);
  initialState;
};

let draw = (state: stateT, env: glEnvT): stateT => {
  let {snake, totalDrawingTime, currentDirection} = state;
  let deltaTime: float = Env.deltaTime(env);
  let totalDrawingTime: float = totalDrawingTime +. deltaTime;

  Gameboard.init(snake, env);
  let nextDirection: Snake.directionT =
    Gameboard.handleKeyPressed(env, currentDirection);

  totalDrawingTime > Gameboard.redrawThreshold ?
    {
      ...state,
      snake: Snake.move(snake, nextDirection),
      totalDrawingTime: Gameboard.initialDrawingTime,
    } :
    {...state, totalDrawingTime, currentDirection: nextDirection};
};

run(~setup, ~draw, ());