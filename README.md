# Game of Life

Programmed by Diogo Valdivieso Damasio Da Costa and Théo Houle.

Programmed the game of life on Gecko FPGA (see board):

![alt text](https://github.com/DiogoVDDC/Assembly_GameOfLife/blob/main/image_2021-11-29_211539.png)

Button Mapping:
1) button 0 is the start/pause button. If pressed, the game toggles between play and pause.
2) button 1 increases the speed of the game.
3) button 2 decreases the speed of the game.
4) button 3 is the reset button. It clears the initial board selection, the number of steps, and stops
the game.
5) button 4 replaces the current game state with a new random one


## Implementation:

### Implementation of led procedures:

We use two function which to change FPGA leds directly: set pixel, clear_leds

The led array in the gecko is indexed in the following way:

![alt text](https://github.com/DiogoVDDC/Assembly_GameOfLife/blob/main/led_array_indexing.png)

And the RAM memory is layed out in the following way: 

![alt text](https://github.com/DiogoVDDC/Assembly_GameOfLife/blob/main/RAM_memory_organization.png)

### Implementation of drawing procedures:

The led array is called a game state array (GSA), we use two function get_gsa and set_gsa to create the procedure draw_gsa which draws all the cells onto the leds.

To draw on the leds we have to change the values of address LEDS in the RAM. We change the value corresponding to what is in the GSA.

### Implementation of update procedures:

To update the GSA we use update_gsa procedure however to implement this procedure we have to determine for each cell it's fate either dead or alive.

To determine the cell fate we use the cell_fate procedure which uses the find_neighbours procedure.

The find_neighbours is implemented such that even cells on the border of the leds have their neighbours.

### Implementation of action functions:

The action functions are the procedures which affect the game such as: change_speed, pause_game, increment_seed, 
