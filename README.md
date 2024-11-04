## Game description
Ignore the previous remark about copying files, everything is here and is built using build gradle like all other projects for the course before.
Welcome to my space invader! Before everything, a quick note: I wanted to use ASCI art for the project, however when I started implementing things I realised that 
making every symbol move would be impossible. This is why my objects are strings alligned at a single point, however they have a hitbox (calculated using various shapes 
ea elipses, circles, squares) which extends up to points where the object should be. Heres the list of features:

Sounds:
1. Background
2. Ship bullet fire
3. Meteor hits ship
4. Game over sound

Objects:
1. Player ship:
-movement in all directions
-health bar (reduced only after shield is destroyed)
-shield bar
-hitbox
-cannon which fires bullet
2. Meteor
-game has as many as 50 meteors spawned at random locations along the x and y axis incomming towards the Player
-hitbox for each meteor
-collision detection with player bullets
3. Boss
-multiple cannons which fire normal bullets
-lazer cannon
-spawns after meteor stage has been completed
-has 100 healthpoints
-can be destroyed
-game over once destroyed
-random movements up and down as well as a tracking player capability every few seconds
-collision detection with player bullets
4. Bullets, Lazers..
-player bullets
-boss bullets
-boss lazer

Other features:
1. Radio - which shows the tutorial transmission
2. Healthbars
3. Planet background
4. Reversible gamestate using button "U" or "u"
5. Game over state
6. Explosion object and animation when player hits meteors with lazer
7. Meteor destroyed counter
8. Double cannons once 5 meteors destroyed
