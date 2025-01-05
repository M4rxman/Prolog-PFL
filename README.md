
## Identification of the Topic and Group

**Topic:**  
Implementation of a Two-Player Board Game in Prolog: _Aqua Pipe Line_

**Group Designation:**  
Group 7 - AquaFlow Architects

**Group Members:**

Hugo Cruz - up202205022  
Oleksandr Aleshchenko - up202210478

1. Hugo Cruz - up202205022
    **Contribution:** 80%
    - Designed and implemented the core game logic in Prolog, including rules for placing and moving pipes, and checking win conditions.
    - Created the text-based user interface and ensured compatibility with three game modes (Human/Human, Human/Computer, and Computer/Computer).
    - Tested game mechanics and debugged logic errors.
    - Revised README file.
    
1. Oleksandr Aleshchenko - up202210478
    **Contribution:** 20%
       - Tested game mechanics
       - Wrote README file
---
## Installation and Execution

#### Installation of SICStus Prolog:

**Prerequisites:**

In order to download and install the binary distributions of, you need the following:

- Your license information, consisting of a site-name, expiration-date, and a license code. 
- Certain library modules and other functionality require optional third-party software. Such software is described [here](https://sicstus.sics.se/portability.html). Usually these can be installed after installing SICStus, if needed.
- Any other limitations are mentioned in the [Release Notes](https://sicstus.sics.se/sicstus/docs/).

## Installation

Before installing, install any platform specific prerequisites, e.g. C compiler and third-party libraries, as described below. Also, make sure you have your license information (license-code, site-name, and expiration date) handy.

#### UNIX
1. [Download](https://sicstus.sics.se/download4.html#download) the binary distribution. Unpack by typing:
   ```bash
    % cat <downloaded-file>.tar.gz | gzip -cd | tar xf -
```
    If tar reports an error, try GNU tar instead.
    
2. Execute the installation script by:
    ```bash
    % cd sp-'<'version>-'<'platform'>'
    % sudo ./InstallSICStus
```
    
    The installation script will ask you some questions about your license, installation directory, locations of third-party software, etc. If you don't understand the question, pressing return will give you the default action which is probably what you want.
    
3. Launch the development system by executing `sicstus`, located in the installation path specified to `InstallSICStus` (e.g. `/usr/local/sicstus4.9.0/bin/sicstus`).

#### Windows 

1. [Download](https://sicstus.sics.se/download4.html#download) the binary distribution.
2. Double-click on the downloaded `.exe` file and follow the instructions.
3. Launch the development system by double-clicking on the `spwin` icon in the `bin` directory or use the shortcut in the Start menu.
4. The `spwin` provides only a minimal top-level. For a full IDE (Integrated Development Environment), download and install [SPIDER](https://sicstus.sics.se/spider/index.html).

The binaries are avaliable on the official sictus site at https://sicstus.sics.se/download4.html

---
## Description of the Game

**Game Name:**  
_Aqua Pipe Line_

**Objective:**  
Players compete to build water pipelines on a board by placing and moving pipes of different sizes and colors. The game ends when one player achieves one of the two winning conditions:

1. Four pipes of the same size and color in a row (_Condition A_).
2. Four sets of three pipes in a row of the same size and color (_Condition B_).

**Rules:**

1. **Setup:**
    - The board is initially empty.
    _ Player 1 is assigned the color Red, while Player 2 is assigned the color Blue.
2. **Gameplay:**
    
    - On their turn, a player may either:
        - Place one of their pipes (small, medium, or large) on an available space.
        - Move one of their previously placed pipes to another available space, provided that they have already placed at least one pipe of each size.
    - Pipes of different sizes can share the same space, as their structures allow stacking.
3. **Winning Conditions:**
    - **Condition A:** Form four pipes in a row of the same size and color (horizontally, vertically, or diagonally).
    - **Condition B:** Form four sets of three pipes in a row, each set having the same size and color.
4. **Game Modes:**
    - **Human vs. Human**
    - **Human vs. Computer**
    - **Computer vs. Human**
    - **Computer vs. Computer** (with two levels of AI difficulty).

---
## Considerations for game extensions

When extending the design of the game, several factors are considered to enhance flexibility, accessibility, and scalability. Below, the primary considerations are discussed, informed by the existing Prolog code and project notes:

#### Variable-Sized Boards

The game is currently implemented with a 4x4 board configuration. However, future versions will support a 3x3 board. The majority of the codebase has been designed with flexibility in mind to accommodate this modification seamlessly.

**Key Considerations:**

- **Dynamic Data Structures:** The code uses structures that are adaptable to different board sizes, ensuring the logic for move validation, win conditions, and pipe placement is size-independent.
- **Scalable AI:** The AI must analyze more complex scenarios as the board size increases. Current logic handles smaller boards efficiently but would need optimization for larger grids.
- **Performance Optimization:** Larger boards increase the number of possible moves, impacting computational performance. Tailored strategies for pruning the search space in AI algorithms are essential.
#### Optional Rules

Optional rules cater to players of varying skill levels and help expand the audience for the game.

**Simplified Rules for Novice Players:**

- Simplified win conditions or restrictions on certain types of pipe placements can lower the learning curve.
- For example, limiting pipe types or providing a guided play mode can make the game more accessible.

**Additional Rules for Expert Players:**

- Expert players may benefit from advanced rules, such as including specialized pipe types like "U-shaped pipes."
- Strategic complexities like point-based scoring for pipe networks can further enrich gameplay.
#### AI Improvements

The AI's current implementation follows a priority system:

1. Prioritize winning moves.
2. Block opponent's winning moves.
3. Create "four-in-a-row" opportunities.
4. Play a pipe of each size.

**Future Enhancements:**

- Improving AI intelligence to better analyze complex board states and develop long-term strategies.
- Ensuring AI adaptability for variable board sizes.
- Correctly handle all diagonals. Not only the two main ones.
#### Code Flexibility and Maintainability

The Prolog implementation emphasizes modularity and flexibility, enabling easy modifications for additional features:

- The code is already flexible regarding board size, as most functions operate on board states abstractly rather than hardcoding dimensions.
- Future implementations, such as reintroducing the 3x3 mode or adding U-shaped pipes, can integrate seamlessly with minimal disruption.

#### Expanding Gameplay Variants

Gameplay can be expanded by offering multiple modes:

- **Classic Mode:** The original rules, optimized for standard gameplay.
- **3x3 Mode:** Introducing additional constraints, such as lover size of board.
- **Novice Mode:** Simplified rules aimed to reduce complexity. Featuring **Reduced Pipe Type**, **AI Adjustments** etc.
- **Advanced mode:** For experienced players seeking more challenge. Featuring specialized pipes and stronger AI.

#### Enhanced User Experience

Extending the game design also involves considering user feedback to refine playability:

- Clear documentation for new players, outlining rules and strategies.
- Visual aids in digital versions, such as highlighting valid moves or suggesting optimal plays, can improve accessibility.


By considering these extensions, the game design can be improved and evolve into a versatile platform appealing to a broader audience while maintaining the core mechanics of the Aqua pipes.

---
## Game Logic

#### Game Configuration Representation

The game configuration encompasses the settings that define how the game is played. The primary components of the configuration include the game type (player vs. player, player vs. computer, computer vs. player, computer vs. computer), the board size, and the game level (difficulty). These parameters are provided by the user through a menu system.

- **Representation**: The game configuration is represented internally as a `game_config` structure with the following fields:
    
    - `type(GameType)` — Specifies the type of game: player vs. player (`h_h`), player vs. computer (`h_pc`), etc.
    - `board_size(Size)` — Indicates the dimensions of the board, typically 4 for this version of the game.
    - `level(Level)` — Defines the difficulty of the game (e.g., `1` for random choices, `2` for greedy choice).
- **Usage**: The configuration is passed into the `initial_state/2` predicate, which uses it to set up the initial game board and determine the rules for starting the game. This configuration is also used to adjust the behavior of the game loop, such as deciding if it's the player's or AI's turn, and determining the available moves.
#### Internal Game State Representation

The internal game state captures the current status of the game at any given point. It reflects the arrangement of pieces on the board, which players are involved, and whose turn it is.

- **Representation**: The game state is represented by a structure like `game_state(Board, CurrentPlayer, RemainingPipes, SetsOfThree)`.

    - `Board` is a list of lists (or a flat list) that represents the positions of the pieces on the board. Each position can have up to three pipes, one of each size.
    - `CurrentPlayer` identifies the player that is going to make the next move.
    - `RemainingPipes` indicates the number of pipes of each size that players have yet to utilize.
    - `SetsOfThree`tracks the number of sets of three pipes in a row that each player has achieved.
- **Usage**: The internal game state is continuously updated during the game as moves are made. It is used by predicates like `game_loop/2` to manage the progression of the game. For example, after each move, the board and turn values are updated, and the current game state is passed around to ensure that the game logic reflects the latest state.
#### Move Representation

In the game, moves can involve either placing a new pipe on the board or moving a previously placed pipe. Pipes can be of three sizes: small, medium, or large. A player may move a pipe they have already placed, but only if they have previously placed at least one pipe of each size.

Moves are represented using the following formats:

- **Placing a new pipe:**  
  `place(player1/player2, small/medium/large, x, y)`

- **Moving an existing pipe:**  
  `move(player1/player2, small/medium/large, x1, y1, x2, y2)`

Here:
- `(x, y)` represents the coordinates on the board where the pipe is placed.
- `(x1, y1)` and `(x2, y2)` represent the starting and ending positions of a moved pipe.


#### User Interaction

The user interaction includes the process of reading inputs for configuring the game, validating moves, and displaying the current state of the game. The interaction with the user is mainly handled through text-based menus and prompts.

- **Game Menu System**: The game begins with a menu where the user selects the type of game (player vs. player, player vs. computer or computer vs, computer) and, if applicable, the difficulty level. Input validation is performed at this stage to ensure that the user selects a valid option.
    
- **Move Input**: When it's the player's turn, the game prompts for a move. The input is validated to ensure it is within bounds, the selected cell can accommodate the move, and the action complies with the game rules, including restrictions on placing and moving pipes.

- **Input Validation**: User inputs are validated to handle common errors, such as choosing an invalid game configuration or trying to place a piece in an already occupied spot. The game ensures that only valid moves are processed, and it gives feedback to the player when an invalid action is attempted.
---

## Conclusions

This project presents a functional strategy game implemented in Prolog, where two players (both human or AI) take turns placing and transferring pipes to form sets of three in a row or one set of four in a row. It supports multiple game modes, including human vs. human, human vs. AI, and AI vs. AI. The game also features a basic AI with two difficulty levels: random and greedy.

#### Key Points:

- **Multiple Game Modes**: Supports various configurations like human vs. human and human vs. AI.
- **Board and Move Management**: Handles board updates and move validation efficiently.
- **AI Player**: The AI is simple, using a greedy approach for gameplay, but could be more strategic.
### Limitations:

1. **AI**: The AI is not overly strategic and may not always offer a challenging experience.
2. **Board Size**: Limited to a 4x4 board, reducing gameplay depth, yet it is expandable by design.
3. **Move Validation**: There could be edge cases not handled perfectly.
4. **User Interface**: The text-based interface is functional but a graphic interface would be more appealing.

### Improvements:

1. **More Strategic AI**: Implement a more advanced AI using algorithms for better strategic decisions.
2. **Customizable Board Size**: Allow users to change the board size to add or remove complexity.
3. **Better Error Handling**: Improve input validation to prevent errors during gameplay.
5. **Performance Enhancements**: Optimize victory condition checks for larger boards.

### Conclusion:

The game is a solid proof of concept with room to grow and improve. This project could be made more engaging and challenging with a smarter AI, a graphical interface, and customizable board sizes.

---

## Bibliography

- [Aqua pipese board game reference ](https://boardgamegeek.com/boardgame/414235/aqua-pipe) 
- Copilot
- [Official SICStus site](https://sicstus.sics.se/download4.html)
- References for Prolog implementation techniques:
    - [Prolog Programming Basics](https://www.swi-prolog.org/)
    - [Board Game AI Strategies](https://boardgamegeek.com/)

---

