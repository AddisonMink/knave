# Knave
ASCII rogue-like stealth extravaganza!

Version: 0.2

## Controls
wasdqezc	-	Move. If you try to move into an enemy's tile, you will attack them instead.
g		    -	Pick up an item.
f		    -	Use your weapon's special attack.
space		-	Enter 'look' mode. Mouse over an object to see it's description. Press 'm' to see the full description.
m		    -	View the full log for the previous round. Be default only the last 4 logs are shown in the hud. If there is overflow you can view the full log by pressing 'm'.

## Mechanics
You are a '@' trying to find your wait to the '<'.

Gameplay consists of a sequence of rounds. Every round, the knave and every enemy performs an action simultaneously. Consider the following example where the knave and an enemy are standing next to each other:

```code
...
.@w
...
```
_If the knave and the enemy attack each other, they will both take damage._

In this situation, the enemy will attack the knave. If the knave decides to attack the enemy, both attacks will resolve, even if the knaves attack kills the enemy. If the knave decides to move away from the enemy, the knave will move but the enemy's attack will still resolve because the 2 events took place simultaneously.

```code
.@.
..w
...
```
_If the knave moves and the enemy attacks, the knave will move but will still be damaged by the enemy's attack._

Every third round is a speed round, where slow enemies will skip their turn and fast enemies will take 2 turns in a row. If an enemy is bound to skip its turn, its symbol will be italisized. If an enemy is bound to take 2 turns in a row, its symbol will be bold.

### Stealth
Since your objective is to reach the stairs, it is often in your interest to avoid enemies rather than fight them. You begin the game hidden. Enemies do not know where you are and they will not chase or attack you. If you end the round inside of an enemy's field of vision, represented by a yellow cone, the enemy will spot you and all enemies will be alerted. While alerted, an enemy's behavior changes and its field of vision becomes a full circle with double the radius of its normal cone of vision. You will become hidden again if you end the round outside of every enemy's field of vision.

If you attack an enemy with a normal attack while hidden, you will do double the normal damage. If you damage an enemy while hidden and fail to kill it, the enemy will be alerted.

### Weapons
You begin the game with your fists, which are relatively weak and have no special attack.

You will find weapons throughout the game. These weapons are more powerful but have limited durability that is consumed when you attack. When the durability reaches 0, the weapon breaks and you will be back to using your fists.

Most weapons have a special attack that can be activated by pressing 'f'. Usually, special attacks are more powerful than normal attacks but consume more durability.