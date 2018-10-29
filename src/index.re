open Reprocessing;

module ListUtils = {
  let last = (input: list('a)): 'a =>
    List.nth(input, List.length(input) - 1);
};

module Dot = {
  type t = (int, int);

  let width: int = 10;
  let height: int = 10;
};

module Snake = {
  type directionT =
    | Left
    | Right
    | Up
    | Down;

  let step: int = 10;
  let initialState: list(Dot.t) = [
    (50, 50),
    (50, 60),
    (50, 70),
    (50, 80),
  ];
  let initialDirection: directionT = Down;

  let move = (snake: list(Dot.t), ~next: directionT): list(Dot.t) =>
    switch (snake) {
    | [_, ...tail] =>
      let (x, y) = ListUtils.last(tail);
      let head =
        switch (next) {
        | Left => (x - step, y)
        | Right => (x + step, y)
        | Up => (x, y - step)
        | Down => (x, y + step)
        };
      tail @ [head];
    | [] => snake
    };
};

module Gameboard = {
  open Reprocessing_Constants;

  let redrawThreshold: float = 0.5;
  let initialDrawingTime: float = 0.;

  let drawBoard = (env: glEnvT): unit => {
    Draw.background(white, env);
    Draw.fill(black, env);
  };

  let drawDots =
      (env: glEnvT, ~dots: list(Dot.t), ~color: colorT=black, ()): unit =>
    List.iter(
      pos => {
        Draw.rect(~pos, ~width=Dot.width, ~height=Dot.height, env);
        Draw.fill(color, env);
      },
      dots,
    );

  let drawSnake = (snake: list(Dot.t), env: glEnvT): unit =>
    drawDots(env, ~dots=snake, ());

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

  let init = (snake: list(Dot.t), env: glEnvT): unit => {
    drawBoard(env);
    drawSnake(snake, env);
  };
};

type stateT = {
  totalDrawingTime: float,
  currentDirection: Snake.directionT,
  snake: list(Dot.t),
};

let initialState: stateT = {
  totalDrawingTime: Gameboard.initialDrawingTime,
  currentDirection: Snake.initialDirection,
  snake: Snake.initialState,
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
      snake: Snake.move(snake, ~next=nextDirection),
      totalDrawingTime: Gameboard.initialDrawingTime,
    } :
    {...state, totalDrawingTime, currentDirection: nextDirection};
};

run(~setup, ~draw, ());
